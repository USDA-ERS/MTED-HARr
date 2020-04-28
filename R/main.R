#' @title Read a GEMPACK HAR file into R
#' @name read_har
#' @description Reads in a GEMPACK HAR file and returns its  representation a list. Currently can only process integer headers, real full headers and character headers
#' @param filename Path to HAR file
#' @return A list of headers
#' @export
read_har <- function(filename) {
  con = file(filename, 'rb')

  cf = c()

  while (length(charRead <- readBin(con, raw())) > 0) {
    cf = c(cf, charRead)
  }

  close(con)

  headers = list()
  for (i in 1:(length(cf) - 12)) {
    if (all(cf[i:(i + 3)] == as.raw(c(0x04, 0x00, 0x00, 0x00))) &
        all(cf[(i + 8):(i + 11)] == as.raw(c(0x04, 0x00, 0x00, 0x00)))
        & all(cf[(i + 4):(i + 7)] != c(0xff, 0xff, 0xff, 0xff))) {
      headers[[trimws(rawToChar(readBin(cf[(i + 4):(i + 7)], raw(), n = 4)))]] = list(start = i)
    }
  }


  for (h in 1:length(headers)) {
    headers[[h]]$binary = cf[headers[[h]]$start:ifelse(h < length(headers), headers[[h +
                                                                                       1]]$start - 1, length(cf))]
  }

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

  # Process first and second records

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

  # Process character headers 1CFULL
  for (h in names(headers)) {
    if (headers[[h]]$type == '1CFULL')  {
      m = matrix(
        strsplit(rawToChar(headers[[h]]$records[[3]][17:length(headers[[h]]$records[[3]])]), '')[[1]],
        nrow =
          headers[[h]]$dimensions[[2]],
        ncol =
          headers[[h]]$dimensions[[1]]
      )


      headers[[h]]$data = trimws(apply(m, 2, paste, collapse = ''))
    }

  }

  # Process character headers 2IFULL
  for (h in names(headers)) {
    if (headers[[h]]$type == '2IFULL')  {
      m = matrix(
        readBin(
          headers[[h]]$records[[3]][33:length(headers[[h]]$records[[3]])],
          'integer',
          size = 4,
          n = prod(headers[[h]]$dimensions)
        ),
        nrow =
          headers[[h]]$dimensions[[2]],
        ncol =
          headers[[h]]$dimensions[[1]]
      )


      headers[[h]]$data = m
    }

  }


  # Process real  headers REFULL
  for (h in names(headers)) {
    if (headers[[h]]$type == 'REFULL')  {
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

        dimNames = list()


        uniqueDimNames = unique(dnames)

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
          }

        }



        m = array(
          readBin(
            headers[[h]]$records[[length(headers[[h]]$records)]][9:length(headers[[h]]$records[[3]])],
            'double',
            size = 4,
            n = Reduce(function(a, f)
              a * length(f), dimNames, 1)
          ),
          dim = Map(function(f)
            length(f), dimNames),
          dimnames = dimNames
        )

      } else {
        m = array(readBin(
          headers[[h]]$records[[length(headers[[h]]$records)]][9:length(headers[[h]]$records[[3]])],
          'double',
          size = 4,
          n = prod(headers[[h]]$dimensions)
        ),
        dim = headers[[h]]$dimensions)

      }


      headers[[h]]$data = m
    }

  }

  return(Map(function(f)f$data,headers))
}
