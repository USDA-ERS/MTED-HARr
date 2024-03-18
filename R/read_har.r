#' @title Read a GEMPACK HAR file into R
#' @name read_har
#' @description Reads in a GEMPACK HAR file and returns its  representation a list. Currently can only process integer headers, real full headers and character headers
#' @param filename Path to HAR file
#' @param useCoefficientsAsNames If a coefficient name is present in the header, use that instead of the four-letter header
#' @param toLowerCase = TRUE Turn all strings to lower case
#' @param headersToRead = NULL A vector of header names to read in
#' @return A list of headers
#' @export
read_har <-
  function(con,
           useCoefficientsAsNames = FALSE,
           toLowerCase = TRUE, headersToRead = NULL) {

    # Open the file
    if(is.character(con)){
      con = file(con, 'rb')
    }

    # Read all bytes into a vector
    #cf = readBin(con, raw(), n = file.info(filename)$size)
    cf = raw()
    while (length(a <- readBin(con, raw(), n=1e9)) > 0) {
      cf=c(cf,a)
    }


    # Read until you hit the end of the file
    while (length(charRead <- readBin(con, raw())) > 0) {
      cf = c(cf, charRead)
    }


    # Close the file
    close(con)

    if (cf[1] == 0xfd) {
      currentHeader = ""
      headers = list()
      i = 2
      while (i < length(cf)) {
        # read the first byte
        fb = cf[i]
        i = i + 1


        bitsLength = as.integer(rawToBits(fb))[3:8]

        toRead = as.integer(rawToBits(fb))[1:2]

        toReadBytes = Reduce(function(a, f)
          a = a + 2 ^ (f - 1) * toRead[f], 1:length(toRead), 0)


        if (toReadBytes > 0) {
          for (i in (i):(i + toReadBytes - 1)) {
            bitsLength = c(bitsLength, rawToBits(cf[i]))
          }
          #i= i + toReadBytes
          i = i + 1
        }


        recordLength = Reduce(function(a, f)
          a = a + 2 ^ (f - 1) * bitsLength[f],
          1:length(bitsLength),
          0)

        if (recordLength == 4) {
          currentHeader = trimws(rawToChar(cf[(i):(i + recordLength - 1)]))
        }
        if (is.null(headers[[currentHeader]]))
          headers[[currentHeader]] = list()

        if (is.null(headers[[currentHeader]]$records))
          headers[[currentHeader]]$records = list()

        headers[[currentHeader]]$records[[length(headers[[currentHeader]]$records) +
                                            1]] = cf[(i):(i + recordLength - 1)]


        i = i + recordLength

        totalLength = recordLength + 1 + toReadBytes

        endingBits = intToBits(totalLength)

        maxPosition = max(which(endingBits == 1))

        if (maxPosition <= 6) {
          needEnd = 0
        } else {
          needEnd = 0 + ceiling((maxPosition - 6) / 8)
        }

        expectedEnd = packBits(c(intToBits(needEnd)[1:2], intToBits(totalLength))[1:(8 *
                                                                                       (needEnd + 1))], 'raw')

        expectedEnd = expectedEnd[length(expectedEnd):1]

        if (any(cf[i:(i + length(expectedEnd) - 1)] != expectedEnd)) {
          stop("Surprising end of record")
        }

        i = i + length(expectedEnd)


      }
    } else {
      # Prepare a list for the headers
      #message('Scanning records for headers')
      # pb = txtProgressBar(min = 0,
      #                     max = length(cf),
      #                     style = 3)
      headers = list()
      i = 1
      while (i < length(cf)) {
        # Read the length of the record
        toRead = readBin(cf[i:(i + 3)], 'integer', size = 4)
        if (toRead == 4) {
          if (!all(cf[(i + 4):(i + 3 + toRead)] == 0x20)) {
            headers[[trimws(rawToChar(cf[(i + 4):(i + 3 + toRead)]))]] = list(start =
                                                                                i)
          }
        }
        #records[[length(records)+1]]=list(start=i, record= cf[(i+4):(i+3+toRead)])
        i = i + 3 + toRead + 1
        hasRead = readBin(cf[i:(i + 3)], 'integer', size = 4)
        if (hasRead != toRead) {
          warning(paste('A broken record', i, hasRead, toRead))
        }
        i = i + 4
        #setTxtProgressBar(pb, i - 1)
      }
      #close(pb)


      #message(paste('Found', length(headers), 'headers'))

      for (h in 1:length(headers)) {
        headers[[h]]$binary = cf[headers[[h]]$start:ifelse(h < length(headers), headers[[h +
                                                                                           1]]$start - 1, length(cf))]
      }

      #message('Sorting out records within headers')
      #Separate records
      for (h in names(headers)) {
        headers[[h]]$records = list()

        i = 1

        while (i < length(headers[[h]]$binary)) {
          toRead = readBin(headers[[h]]$binary[i:(i + 3)], 'integer', size = 4)
          i = i + 4
          headers[[h]]$records[[length(headers[[h]]$records) + 1]] = readBin(headers[[h]]$binary[i:(i +
                                                                                                      toRead - 1)], raw(), n = toRead)
          i = i + toRead
          hasRead = readBin(headers[[h]]$binary[i:(i + 3)], 'integer', size =
                              4)
          i = i + 4
          if (toRead != hasRead) {
            warning(paste('toRead different from hasRead in ', h))
          }
        }
      }
    }

    # If there is a positive list, exclude other headers
    if(!is.null(headersToRead) & !is.na(headersToRead)){
      headers = headers[headersToRead]
    }


    # Process first and second records
    #message('Processing first and second records within headers')
    for (h in names(headers)) {
      headers[[h]]$type = rawToChar(headers[[h]]$records[[2]][5:10])
      headers[[h]]$description = rawToChar(headers[[h]]$records[[2]][11:80])

      headers[[h]]$numberOfDimensions = readBin(headers[[h]]$records[[2]][81:84], 'integer', size =
                                                  4)

      headers[[h]]$dimensions =  c()

      for (i in 1:headers[[h]]$numberOfDimensions) {
        headers[[h]]$dimensions = c(headers[[h]]$dimensions,
                                    readBin(headers[[h]]$records[[2]][(85 + (i - 1) * 4):(85 + i * 4)], 'integer', size =
                                              4))
      }

    }

    #message('Processing character headers')
    # Process character headers 1CFULL
    for (h in names(headers)) {
      if (headers[[h]]$type == '1CFULL')  {
        contents = Reduce(function(a, f)
          c(a, headers[[h]]$records[[f]][17:length(headers[[h]]$records[[f]])]),
          3:length(headers[[h]]$records),
          c())

        contents[contents == 0x00] = as.raw(0x20)

        m = matrix(
          strsplit(stringi::stri_encode(contents, from = "latin2", to = "UTF-8"),"")[[1]],
          #rawToChar(contents, multiple = TRUE),
          nrow =
            headers[[h]]$dimensions[[2]],
          ncol =
            headers[[h]]$dimensions[[1]]
        )

        # do not remove empty space in the history header
        if(tolower(h)=='xxhs'){
          toRet = apply(m, 2, paste, collapse = '')
        } else {
          toRet = trimws(apply(m, 2, paste, collapse = ''))
        }



        if (toLowerCase) {
          toRet = tolower(toRet)
        }

        headers[[h]]$data = toRet
      }

    }

    #message('Processing integer headers')

    # Process character headers 2IFULL
    for (h in names(headers)) {
      if (headers[[h]]$type == '2IFULL')  {
        m = matrix(
          readBin(
            Reduce(
              function(a, f)
                c(a, headers[[h]]$records[[f]][33:length(headers[[h]]$records[[f]])]),
              3:length(headers[[h]]$records),
              c()
            ),
            #headers[[h]]$records[[3]][33:length(headers[[h]]$records[[3]])],
            'integer',
            size = 4,
            n = prod(headers[[h]]$dimensions)
          ),
          nrow =
            headers[[h]]$dimensions[[1]],
          ncol =
            headers[[h]]$dimensions[[2]]
        )


        headers[[h]]$data = m
      }

    }

    # Process real headers 2RFULL
    for (h in names(headers)) {
      if (headers[[h]]$type == '2RFULL')  {
        m = array(readBin(
          Reduce(
            function(a, f)
              c(a, headers[[h]]$records[[f]][33:length(headers[[h]]$records[[f]])]),
            3:length(headers[[h]]$records),
            c()
          ),
          'double',
          size = 4,
          n = prod(headers[[h]]$dimensions)
        ),
        dim = headers[[h]]$dimensions)


        headers[[h]]$data = m
      }

    }






    #message('Processing real headers')

    # Process real  headers REFULL
    for (h in names(headers)) {
      if (headers[[h]]$type %in% c('REFULL', 'RESPSE'))  {
        # Get used dimensions and their names from record 3
        headers[[h]]$definedDimensions = readBin(headers[[h]]$records[[3]][5:8], 'integer', size =
                                                   4)
        headers[[h]]$usedDimensions = readBin(headers[[h]]$records[[3]][13:16], 'integer', size =
                                                4)
        headers[[h]]$coefficient = rawToChar(headers[[h]]$records[[3]][17:28])

        if (headers[[h]]$usedDimensions > 0) {
          m = matrix(
            strsplit(rawToChar(headers[[h]]$records[[3]][33:(33 + headers[[h]]$usedDimensions *
                                                               12 - 1)]), '')[[1]],
            nrow =
              12,
            ncol =
              headers[[h]]$usedDimensions
          )

          dnames = apply(m, 2, paste, collapse = '')

          dimNames = Map(function(f)NULL, 1:headers[[h]]$usedDimensions)

          actualDimsNamesFlags = headers[[h]]$records[[3]][(33 + headers[[h]]$usedDimensions *
                                                            12)+0:6]
          actualDimsNames=ifelse(actualDimsNamesFlags==0x6b,TRUE,FALSE)

          uniqueDimNames = unique(dnames[actualDimsNames])


          # if (headers[[h]]$definedDimensions == 0) {
          #   uniqueDimNames = as.character()
          # }

          if (length(uniqueDimNames) > 0) {
            for (d in 1:length(uniqueDimNames)) {
              nele = readBin(headers[[h]]$records[[3 + d]][13:16], 'integer', size = 4)

              m = matrix(strsplit(rawToChar(headers[[h]]$records[[3 + d]][17:(17 +
                                                                                nele * 12 - 1)]), '')[[1]],
                         nrow =
                           12,
                         ncol =
                           nele)


              for (dd in which(dnames == uniqueDimNames[d])) {
                dimNames[[dd]] = trimws(apply(m, 2, paste, collapse = ''))
                # Add dimension name
                names(dimNames)[dd] = trimws(uniqueDimNames[d])
              }

            }
          }

          dataStart = 3 + length(uniqueDimNames) + 1

          # # There are no defined dimensions, generate the names etc.
          # if (headers[[h]]$definedDimensions == 0) {
          #   dimNames = Reduce(function(a, f) {
          #     a[[length(a) + 1]] = (paste('Element', 1:headers[[h]]$dimensions[f]))
          #     return(a)
          #   } , #headers[[h]]$dimensions[1:headers[[h]]$usedDimensions]
          #   (1:headers[[h]]$usedDimensions)[1:headers[[h]]$usedDimensions >
          #                                     0]
          #   , list())
          #
          #   names(dimNames) = paste('Dimension', (1:headers[[h]]$usedDimensions)[1:headers[[h]]$usedDimensions >
          #                                                                          0])
          #
          # }

          if (headers[[h]]$type == 'REFULL') {
            numberOfFrames = readBin(headers[[h]]$records[[dataStart]][5:8], 'integer')

            numberOfDataFrames = (numberOfFrames - 1) / 2


            dataFrames  = (dataStart) + 1:numberOfDataFrames * 2

            # dataBytes = Reduce(function(a, f)
            #   c(a, headers[[h]]$records[[f]][9:length(headers[[h]]$records[[f]])]), dataFrames, c())
            dataBytes = do.call(c, Map(function(f)
              headers[[h]]$records[[f]][9:length(headers[[h]]$records[[f]])], dataFrames))

            m = array(
              readBin(
                # headers[[h]]$records[[length(headers[[h]]$records)]][9:(8+Reduce(function(a, f)
                #   a * length(f), dimNames, 1)*4)],
                dataBytes,
                'double',
                size = 4,
                n = prod(headers[[h]]$dimensions)
              ),
              dim = headers[[h]]$dimensions[1:headers[[h]]$usedDimensions],
              dimnames = dimNames
            )
          } else{
            elements = readBin(headers[[h]]$records[[dataStart]][5:8], 'integer', size =
                                 4)
            dataVector = rep(0, prod(headers[[h]]$dimensions))

            for (rr in (dataStart + 1):length(headers[[h]]$records)) {
              dataBytes = headers[[h]]$records[[rr]][17:length(headers[[h]]$records[[rr]])]

              currentPoints = length(dataBytes) / 8

              locations = readBin(dataBytes[1:(4 * currentPoints)],
                                  'integer',
                                  size = 4,
                                  n = currentPoints)
              values = readBin(dataBytes[(4 * currentPoints + 1):(8 * currentPoints)],
                               'double',
                               size = 4,
                               n = currentPoints)

              dataVector[locations] = values
            }


            m = array(
              dataVector,
              dim = headers[[h]]$dimensions[1:headers[[h]]$usedDimensions],
              dimnames = dimNames
            )


          }
        } else {
          m = array(readBin(
            headers[[h]]$records[[length(headers[[h]]$records)]][9:length(headers[[h]]$records[[3]])],
            'double',
            size = 4,
            n = prod(headers[[h]]$dimensions)
          ),
          dim = headers[[h]]$dimensions[1:headers[[h]]$usedDimensions])

        }


        headers[[h]]$data = m
      }

    }

    toRet = Map(function(f) {
      toRet2 = f$data

      if (toLowerCase) {
        if (!is.null(dimnames(toRet2))) {
          names(dimnames(toRet2)) = tolower(names(dimnames(toRet2)))
          dimnames(toRet2) = Map(function(f)
            tolower(f), dimnames(toRet2))
        }


      }
      return(toRet2)
    }
    , headers)

    if (useCoefficientsAsNames == T) {
      for (h in 1:length(headers)) {
        if (!is.null(headers[[h]]$coefficient)) {
          names(toRet)[h] = trimws(headers[[h]]$coefficient)
        }
      }
    }


    if(toLowerCase){
      names(toRet) = tolower(names(toRet))
    }

    return(toRet)
  }
