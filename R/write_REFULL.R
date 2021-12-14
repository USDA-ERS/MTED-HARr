write_REFULL = function(headerName, arr, description = NULL, coefficient = NULL) {
  # If no description is provided, use header name
  if (is.null(description) | any(is.na(description))) {
    description = headerName
  } else{
    if (nchar(description) > 70) {
      stop(paste0("The length of the ", headerName, " description is larger than 12"))
    }
  }

  if (is.null(coefficient)) {
    coefficient = headerName
  } else{
    if (nchar(coefficient) > 12) {
      stop(paste0("The length of the ", headerName, " coefficient is larger than 12"))
    }
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

  r[[1]] = writeBin(paste0(c(headerName,rep(' ',4-nchar(headerName))),
                           collapse = ""), raw())[1:4]


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
          writeBin(paste0(f, paste0(
            rep(' ', 12 - nchar(f)), collapse = ''
          )), raw())[1:12], ele
      )))

    )
  }

  r[[length(r) + 1]] = as.raw(c(writeBin('    ', raw())[1:4],
                                                      writeBin(c(3L, 7L, as.integer(dimensions)), raw())))
  r[[length(r) + 1]] = as.raw(c(writeBin('    ', raw())[1:4],
                                                      writeBin(c(2L,
                                                                 c(
                                                                   rbind(rep(1L, 7), as.integer(dimensions))
                                                                 )), raw())))

  r[[length(r) + 1]] = c(
    writeBin('    ', raw())[1:4],
    writeBin(1L, raw(), size = 4),
    writeBin(as.numeric(c(arr)), raw(), size = 4)
  )


  return(r)
}
