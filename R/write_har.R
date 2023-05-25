#' @title Write an R list to GEMPACK HAR
#' @name write_har
#' @description This function writes a HAR file based on a list. If a list element contains attribute "description," then it is used to define the long header name
#' @description Some warnings: (1) you cannot have NA's in a HAR file, (2) empty strings are not allowed, (3) some programs (e.g., GEMPACK models) read chunks of data no longer than 1e4 bytes (set maxSize = 1e4), (4) all dimensions in arrays must have names (see example)
#' @param data A list
#' @returns NULL
#' @examples myList = list(TEST = c('Test'))
#' @examples attr(myList$TEST,'description') = "This is the long header name"
#' @examples write_har(myList,'harfile.har')
#' @export
write_har <- function(data, filename, maxSize = 1e4) {
  # Open the file
  con = file(filename, 'wb')
  records = Map(function(f) {
    headerName = names(data)[f]

    if (nchar(headerName) <= 4) {
      if (any(class(data[[f]]) == 'character')) {
        write_1CFULL(headerName, data[[f]], description = attr(data[[f]],'description'))
      } else if (any(class(data[[f]]) %in% c('matrix','array','numeric'))){
        if(any(class(data[[f]])=='matrix') & is.integer(data[[f]])){
          write_2IFULL(headerName, data[[f]], description = attr(data[[f]],'description'))
        } else {
          write_REFULL(headerName, data[[f]],maxSize=maxSize, description = attr(data[[f]],'description'))
        }
      }
    }
  }
  ,
  1:length(data))

  for (r in records) {
    for (rr in r) {
      writeBin(c(
        writeBin(as.integer(length(rr)), con = raw(), size = 4),
        rr,
        writeBin(as.integer(length(rr)), con = raw(), size = 4)
      ), con = con)
    }
  }


  # Close the file
  close(con)
}
