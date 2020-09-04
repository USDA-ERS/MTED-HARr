write_2IFULL = function(headerName, arr, description = NULL) {
  # If no description is provided, use the header name
  if (is.null(description)) {
    description = headerName
  }

  # Number of dimensions of the object
  dimensions =  dim(arr)
  numberOfDimensions = length(dimensions)


  r = list()

  r[[1]] = writeBin(paste0(headerName,rep(' ',4-nchar(headerName))), raw())[1:4]


  r[[2]] =
    c(
      #Empty four characters
      writeBin('    ', raw())[1:4],
      # Type
      writeBin('2IFULL', raw())[1:6],
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

  r[[3]] =
    c(
      #Empty four characters
      writeBin('    ', raw())[1:4],
      # Defined dimensions
      writeBin(1L, raw()),
      writeBin(as.integer(dim(arr)[1]), raw()),
      writeBin(as.integer(dim(arr)[2]), raw()),
      writeBin(1L, raw()),
      writeBin(as.integer(dim(arr)[1]), raw()),
      writeBin(1L, raw()),
      writeBin(as.integer(dim(arr)[2]), raw()),
      writeBin(as.integer(arr),raw(),size=4)
    )

  return(r)
}
