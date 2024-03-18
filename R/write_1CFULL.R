write_1CFULL = function(headerName, arr, description = NULL) {
  # If no description is provided, use the header name
  if (is.null(description)) {
    description = headerName
  }

  # Number of dimensions of the object
  dimensions =  c(length(arr),max(12,nchar(arr)))
  numberOfDimensions = length(dimensions)

  r = list()

  r[[1]] = writeBin(paste0(headerName,paste(rep(' ',4-nchar(headerName)),collapse='')), raw())[1:4]


  r[[2]] =
    c(
      #Empty four characters
      writeBin('    ', raw())[1:4],
      # Type
      writeBin('1CFULL', raw())[1:6],
      # Description
      writeBin(paste0(
        description, paste0(rep(' ', 70 - nchar(
          description
        )), collapse = '')
      ), raw())[1:70],
      # Number of dimensions
      writeBin(as.integer(numberOfDimensions), raw(), size = 4),
      writeBin(as.integer(dimensions), raw(), size = 4)
    )

  if((dimensions[2] * length(arr))>0)
    lastPiece = writeBin(paste0(
    substr(arr, 1, dimensions[2]), Map(
      function(f)
        paste0(rep(' ', f), collapse = ''),
      dimensions[2] - nchar(substr(arr, 1, dimensions[2]))
    )
    ,collapse=''), raw())[1:(dimensions[2] * length(arr))]
  else
    lastPiece = writeBin(numeric(),raw())

  r[[3]] =
    c(
      #Empty four characters
      writeBin('    ', raw())[1:4],
      # Defined dimensions
      writeBin(1L, raw()),
      writeBin(as.integer(length(arr)), raw()),
      writeBin(as.integer(length(arr)), raw()),
      lastPiece
    )

  return(r)
}
