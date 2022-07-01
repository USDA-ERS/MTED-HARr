write_REFULL = function(headerName,
                        arr,
                        description = NULL,
                        coefficient = NULL,
                        maxSize = 1e6) {

  message(sprintf('%s with maxsize %s', headerName, maxSize))

    # If no description is provided, use the header name
  if (is.null(description)) {
    description = headerName
  }

  if (is.null(coefficient)) {
    coefficient = headerName
  }

  # Number of dimensions of the object
  dimensions =  dim(arr)
  if (is.null(dimensions)) {
    dimensions = c(1)
  }

  # real number arrays must have exactly seven dimensions
  dimensions = c(dimensions, rep(1, 7 - length(dimensions)))

  numberOfDimensions = length(dimensions)


  r = list()

  r[[1]] = writeBin(paste0(headerName, rep(' ', 4 - nchar(headerName))), raw())[1:4]


  r[[2]] =
    c(
      #Empty four characters
      writeBin('    ', raw())[1:4],
      # Type
      writeBin('REFULL', raw())[1:6],
      # Description
      writeBin(paste0(description, paste0(
        rep(' ', 70 - nchar(description)), collapse = ''
      )), raw())[1:70],
      # Number of dimensions
      writeBin(numberOfDimensions, raw(), size = 4),
      # Each dimension size
      unlist(Map(
        function(f)
          writeBin(as.integer(f), raw(), size = 4),
        dimensions
      ))
    )

  # Record 3 contains the number of defined dimensions, number of used dimensions, coefficient name and used set names
  # Each dimension name

  if (is.null(dimnames(arr))) {
    setNames = as.raw(c(0x00, 0x00, 0x00, 0x00))
  } else {
    setNames =  c(unname(unlist(Map(
      function(f)
        writeBin(paste0(f, paste0(
          rep(' ', 12 - nchar(f)), collapse = ''
        )), raw())[1:12], names(dimnames(arr))
    )))
    ,
    as.raw(c(rep(
      0x6b, length(dimnames(arr))
    ))),
    as.raw(c(rep(
      0x00, 4 + 4 * length(dimnames(arr))
    ))),
    as.raw(c(rep(0x00, 7))))

  }

  r[[3]] =
    c(
      #Empty four characters
      writeBin('    ', raw())[1:4],
      # Defined dimensions
      writeBin(as.integer(length(unique(
        names(dimnames(arr))
      ))), raw()),
      as.raw(c(0xff, 0xff, 0xff, 0xff)),
      # Used dimensions
      writeBin(as.integer(length(names(
        dimnames(arr)
      ))), raw()),
      # coefficient name
      writeBin(paste0(coefficient, paste0(
        rep(' ', 12 - nchar(coefficient)), collapse = ''
      )), raw())[1:12],
      as.raw(c(0xff, 0xff, 0xff, 0xff)),
      setNames
    )


  for (ud in unique(names(dimnames(arr)))) {
    ele = dimnames(arr)[[ud]]
    r[[length(r) + 1]] = c(
      writeBin('    ', raw())[1:4],
      writeBin(1L, raw(), size = 4),
      writeBin(as.integer(length(ele)), raw(), size = 4),
      writeBin(as.integer(length(ele)), raw(), size = 4),
      unname(unlist(Map(
        function(f)
          {f=substr(f,1,12)
          writeBin(paste0(f, paste0(
            rep(' ', 12 - nchar(f)), collapse = ''
          )), raw())[1:12]}, ele
      )))

    )
  }


  arr2 = arr
  arr2[] = FALSE


  if(length(arr)<=maxSize){
    sliceSize = length(arr)
    numberDataRecords = 1
  }else{

  arr2[maxSize] = TRUE

  breaks = which(arr2 == TRUE, arr.ind = TRUE)


  for (cc in length(dim(arr)):1) {
    if (breaks[cc] > 1) {
      break
    }
  }

  cutPoint = rep(1, length(dim(arr)))
  #cutPoint[cc] = breaks[cc]
  cutPoint[cc] = 2

  arr2[] = FALSE
  arr2[matrix(cutPoint, nrow = 1)] = TRUE

  sliceSize = which(arr2 == TRUE) - 1


  numberDataRecords = ceiling(length(c(arr)) / sliceSize)
}
  r[[length(r) + 1]] = as.raw(c(writeBin('    ', raw())[1:4],
                                writeBin(c(
                                  as.integer(1 + numberDataRecords * 2),
                                  7L,
                                  as.integer(dimensions)
                                ), raw())))
  # r[[length(r) + 1]] = as.raw(c(writeBin('    ', raw())[1:4],
  #                                                     writeBin(c(as.integer(1+numberDataRecords*2),
  #                                                                c(
  #                                                                  rbind(rep(1L, 7), as.integer(dimensions))
  #                                                                )), raw())))



dataHeaders = Map(function(dr){
  fromElement = (dr - 1) * sliceSize + 1
  toElement = min(dr * sliceSize, length(c(arr)))

  arr2[] = FALSE
  arr2[fromElement] = TRUE
  arr2[toElement] = TRUE
  fromToIndices =as.matrix(which(arr2 == TRUE, arr.ind = TRUE))


  if(nrow(fromToIndices)==1){
    fromToIndices=rbind(fromToIndices,fromToIndices)
  }

  fromToVector = c(as.vector(fromToIndices), rep(c(1, 1), 7 - length(dim(arr))))

  return(list(
  c(
    writeBin('    ', raw())[1:4],
    writeBin(as.integer(numberDataRecords * 2 - (dr) * 2 + 2), raw(), size = 4),
    writeBin(as.integer(fromToVector), raw(), size = 4)
  ),

  c(
    writeBin('    ', raw())[1:4],
    writeBin(as.integer(numberDataRecords * 2 - (dr) * 2 + 1), raw(), size = 4),
    writeBin(as.numeric(c(arr))[((dr - 1) * sliceSize + 1):min(dr * sliceSize, length(c(arr)))], raw(), size = 4)
  )))

},1:numberDataRecords)

#toRet=unlist(dataHeaders,recursive=FALSE)
return(c(r, unlist(dataHeaders,recursive=FALSE)))
  # for (dr in 1:numberDataRecords) {
  #   fromElement = (dr - 1) * sliceSize + 1
  #   toElement = min(dr * sliceSize, length(c(arr)))
  #
  #   arr2[] = FALSE
  #   arr2[fromElement] = TRUE
  #   arr2[toElement] = TRUE
  #   fromToIndices = which(arr2 == TRUE, arr.ind = TRUE)
  #
  #   if(nrow(fromToIndices)==1){
  #     fromToIndices=rbind(fromToIndices,fromToIndices)
  #   }
  #
  #   fromToVector = c(as.vector(fromToIndices), rep(c(1, 1), 7 - length(dim(arr))))
  #
  #   r[[length(r) + 1]] = c(
  #     writeBin('    ', raw())[1:4],
  #     writeBin(as.integer(numberDataRecords * 2 - (dr) * 2 + 2), raw(), size = 4),
  #     writeBin(as.integer(fromToVector), raw(), size = 4)
  #   )
  #
  #   r[[length(r) + 1]] = c(
  #     writeBin('    ', raw())[1:4],
  #     writeBin(as.integer(numberDataRecords * 2 - (dr) * 2 + 1), raw(), size = 4),
  #     writeBin(as.numeric(c(arr))[((dr - 1) * sliceSize + 1):min(dr * sliceSize, length(c(arr)))], raw(), size = 4)
  #   )
  #
  # }


  #return(r)
}

# length(r[[9]])
#
# length(arr)
# f=9
#
# arr2 = unlist(Map(function(f){
#   readBin(r[[f]][9:length(r[[f]])],size=4,n =(length(r[[f]])-8)/4 , what='double')
# }, 9:14))
# f=9
# length(r[[f]])
#
# sum(unlist(Map(function(f){
#   length(r[[f]])-8
# }, 9:14)))/8
#
# arr3=as.vector(arr)
#
# arr2[40000:40100]
# arr3[40000:40100]
#
# arr2[9998:10002]
# arr3[9998:10002]
#
# length(arr2)
# length(arr3)
#
#
# which(round(arr2,1)!=round(arr3,1))
# arr2[14]==arr3[14]
# arr=readRDS('c:/gp/vifm.rds')
