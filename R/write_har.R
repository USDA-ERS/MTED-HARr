#' @title Write an R list to GEMPACK HAR
#' @name write_har
#' @description
#' @param filename Path to HAR file
#' @param useCoefficientsAsNames If a coefficient name is present in the header, use that instead of the four-letter header
#' @return A list of headers
#' @export
write_har <- function(data, filename) {
  # Open the file
  con = file(filename, 'wb')
  records = Map(function(f) {
    headerName = names(data)[f]

    if (nchar(headerName) <= 4) {
      if (class(data[[f]]) == 'character') {
        write_1CFULL(headerName, data[[f]])
      } else if (class(data[[f]]) %in% c('matrix','array','numeric')){
        if(class(data[[f]])=='matrix' & is.integer(data[[f]])){
          write_2IFULL(headerName, data[[f]])
        } else {
          write_REFULL(headerName, data[[f]])
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
# write_har <- function(data, filename) {
#   # data = list(
#   #   list(
#   #     header = 'ABCD',
#   #     coefficient = 'abCD',
#   #     description = "A test of write HAR",
#   #     type = 'REFULL',
#   #     array = array(
#   #       c(1, 2, 3, 4, 5, 6),
#   #       dim = c(2, 3),
#   #       dimnames = list(ff = c('a', 'b'), gg = c('f', 'g', 'h'))
#   #     )
#   #   ),
#   #   list(
#   #     header = 'DBCD',
#   #     coefficient = 'deCD',
#   #     description = "A test of write HAR",
#   #     type = 'REFULL',
#   #     array = array(
#   #       c(1, 2, 3, 4, 5, 6),
#   #       dim = c(3, 2, 4),
#   #       dimnames = list(
#   #         gg = c('f', 'g', 'h'),
#   #         ff = c('a', 'b'),
#   #         hh = c('f1', 'g2', 'h3', 'y4')
#   #       )
#   #     )
#   #   )
#   #
#   # )
#   #
#   # data=list(
#   #   ABCD = array(
#   #     c(1, 2, 3, 4, 5, 6),
#   #     dim = c(2, 3),
#   #     dimnames = list(ff = c('a', 'b'), gg = c('f', 'g', 'h'))
#   #   )
#   # )
#
#   records = list()
#
#   for (d in 1:length(data)) {
#     type = NULL
#
#     # if the name of the list element is 1-4 letters and the type is known create a header
#     if (nchar(names(data)[d]) <= 4) {
#       header = names(data)[d]
#
#       if (class(data[[d]]) == 'character') {
#         type = '1CFULL'
#       } else if (class(data[[d]]) == 'matrix') {
#         if (typeof(data[[d]]) %in% c('double')) {
#           type = 'REFULL'
#         } else if (typeof(data[[d]]) %in% c('character')) {
#           warning(
#             sprintf(
#               'Element %s has a character matrix which cannot be stored in a HAR file',
#               d
#             )
#           )
#         } else if (typeof(data[[d]]) %in% c('integer')) {
#           warning(
#             sprintf(
#               'Element %s has an integer matrix which cannot be stored in a HAR file (you can save it as double and try again)',
#               d
#             )
#           )
#         }
#       } else if (class(data[[d]]) == 'array') {
#         if (typeof(data[[d]]) %in% c('character')) {
#           warning(
#             sprintf(
#               'Element %s has a character array which cannot be stored in a HAR file',
#               d
#             )
#           )
#         } else if (typeof(data[[d]]) %in% c('integer')) {
#           warning(
#             sprintf(
#               'Element %s has an integer array which cannot be stored in a HAR file (you can save it as double and try again)',
#               d
#             )
#           )
#         } else if (typeof(data[[d]]) %in% c('double')) {
#           if (length(dim(data[[d]])) > 7) {
#             warning(
#               sprintf(
#                 'Element %s has more than seven dimensions which cannot be stored in a HAR file',
#                 d
#               )
#             )
#           } else {
#             type = 'REFULL'
#           }
#         }
#       }
#
#       if (!is.null(type)) {
#         if (type == 'REFULL') {
#           coefficient = names(data)[d]
#           description = names(data)[d]
#           arr = data[[d]]
#
#           if (is.null(dimnames(arr))) {
#             newSets = Map(function(f, g) {
#               paste(header, g, 1:f, sep = '_')
#             }, dim(arr), 1:length(dim(arr)))
#             names(newSets) = paste(header, 1:length(dim(arr)), sep = '_')
#             dimnames(arr) = newSets
#           }
#
#           dimensions =  unname(unlist(Map(
#             function(f)
#               as.integer(length(f)), dimnames(arr)
#           )))
#           dimensions = c(dimensions, rep(1, 7 - length(dimensions)))
#           numberOfDimensions = length(dimensions)
#
#         }
#
#         else if (type == '1CFULL') {
#           arr = data[[d]]
#         }
#       }
#     } else{
#       warning(
#         sprintf(
#           'Element %s has a name over four characters, which cannot be saved as a header',
#           d
#         )
#       )
#     }
#
#     # if (is.null(data[[d]]$header)) {
#     #   message(sprintf(
#     #     'No header name provided for element %s in the provided list',
#     #     d
#     #   ))
#     # } else if (nchar(data[[d]]$header) > 4) {
#     #   message(
#     #     sprintf(
#     #       'Header name  %s provided for element %s in the provided list is over four characteres long',
#     #       data[[d]]$header,
#     #       d
#     #     )
#     #   )
#     # } else{
#     #   header = data[[d]]$header
#     # }
#     # description = data[[d]]$description
#     # type = data[[d]]$type
#     #
#     # if (type == 'REFULL') {
#     #   dimensions =  unname(unlist(Map(
#     #     function(f)
#     #       as.integer(length(f)), dimnames(data[[d]]$array)
#     #   )))
#     #   dimensions = c(dimensions, rep(1, 7 - length(dimensions)))
#     # }
#     #
#     # numberOfDimensions = length(dimensions)
#     # # Process record 1
#
#     if (is.null(type)) {
#       warning(sprintf('Element %s cannot be processed', d))
#     } else {
#       records[[d]] = list()
#       records[[d]][[1]] = writeBin(header, raw())[1:4]
#
#
#
#       if (type == 'REFULL') {
#         #      arr = data[[d]]$array
#         #      coefficient = data[[d]]$coefficient
#         # Process record 2
#
#         records[[d]][[2]] =
#           c(
#             #Empty four characters
#             writeBin('    ', raw())[1:4],
#             # Type
#             writeBin(type, raw())[1:6],
#             # Description
#             writeBin(paste0(
#               description, paste0(rep(' ', 70 - nchar(
#                 description
#               )), collapse = '')
#             ), raw())[1:70],
#             # Number of dimensions
#             writeBin(numberOfDimensions, raw(), size = 4),
#             # Each dimension size
#             unlist(Map(
#               function(f)
#                 writeBin(as.integer(f), raw(), size = 4),
#               dimensions
#             ))
#           )
#
#         # Record 3 contains the number of defined dimensions, number of used dimensions, coefficient name and used set names
#         records[[d]][[3]] =
#           c(
#             #Empty four characters
#             writeBin('    ', raw())[1:4],
#             # Defined dimensions
#             writeBin(as.integer(length(
#               unique(names(dimnames(arr)))
#             )), raw()),
#             as.raw(c(0xff, 0xff, 0xff, 0xff)),
#             # Used dimensions
#             writeBin(as.integer(length(
#               names(dimnames(arr))
#             )), raw()),
#             # coefficient name
#             writeBin(paste0(
#               coefficient, paste0(rep(' ', 12 - nchar(
#                 coefficient
#               )), collapse = '')
#             ), raw())[1:12],
#             as.raw(c(0xff, 0xff, 0xff, 0xff)),
#             # Each dimension name
#             unname(unlist(
#               Map(function(f)
#                 writeBin(paste0(
#                   f, paste0(rep(' ', 12 - nchar(f)), collapse = '')
#                 ), raw())[1:12], names(dimnames(arr)))
#             ))
#             ,
#             as.raw(c(rep(
#               0x6b, length(dimnames(arr))
#             ))),
#             as.raw(c(rep(
#               0x00, 4 + 4 * length(dimnames(arr))
#             ))),
#             as.raw(c(rep(0x00, 7)))
#           )
#
#         #records[[d]][[3]]=c(records[[d]][[3]], as.raw(rep(0x00,87-length(records[[d]][[3]]))))
#
#         for (ud in unique(names(dimnames(arr)))) {
#           ele = dimnames(arr)[[ud]]
#           records[[d]][[length(records[[d]]) + 1]] = c(
#             writeBin('    ', raw())[1:4],
#             writeBin(1L, raw(), size = 4),
#             writeBin(as.integer(length(ele)), raw(), size = 4),
#             writeBin(as.integer(length(ele)), raw(), size = 4),
#             unname(unlist(Map(
#               function(f)
#                 writeBin(paste0(f, paste0(
#                   rep(' ', 12 - nchar(f)), collapse = ''
#                 )), raw())[1:12], ele
#             )))
#
#           )
#         }
#         records[[d]][[length(records[[d]]) + 1]] = as.raw(c(writeBin('    ', raw())[1:4],
#                                                             writeBin(c(
#                                                               3L, 7L, unname(unlist(
#                                                                 Map(function(f)
#                                                                   as.integer(length(f)), dimnames(arr))
#                                                               )), rep(1L, 7 - length(dimnames(arr)))
#                                                             ), raw())))
#         records[[d]][[length(records[[d]]) + 1]] = as.raw(c(writeBin('    ', raw())[1:4],
#                                                             writeBin(c(
#                                                               2L,
#                                                               unname(unlist(Map(
#                                                                 function(f)
#                                                                   c(1L, as.integer(length(f))), dimnames(arr)
#                                                               )))
#                                                               ,
#                                                               rep(c(1L, 1L), 7 - length(dimnames(arr)))
#                                                             ), raw())))
#
#         records[[d]][[length(records[[d]]) + 1]] = c(
#           writeBin('    ', raw())[1:4],
#           writeBin(1L, raw(), size = 4),
#           writeBin(as.numeric(c(arr)), raw(), size = 4)
#         )
#
#
#       }
#       if (type == '1CFULL') {
#         records[[d]][[2]] =
#           c(
#             #Empty four characters
#             writeBin('    ', raw())[1:4],
#             # Type
#             writeBin(type, raw())[1:6],
#             # Description
#             writeBin(paste0(
#               description, paste0(rep(' ', 70 - nchar(
#                 description
#               )), collapse = '')
#             ), raw())[1:70],
#             # Number of dimensions
#             writeBin(2L, raw(), size = 4),
#             writeBin(12L, raw(), size = 4),
#             writeBin(length(arr), raw(), size = 4)
#           )
#
#         records[[d]][[3]] =
#           c(
#             #Empty four characters
#             writeBin('    ', raw())[1:4],
#             # Defined dimensions
#             writeBin(1L, raw()),
#             writeBin(12L, raw()),
#             writeBin(12L, raw()),
#             writeBin(paste0(
#               substr(arr, 1, 12), Map(
#                 function(f)
#                   paste0(rep(' ', f), collapse = ''),
#                 12 - nchar(substr(arr, 1, 12))
#               )
#             ), raw())[1:(12 * length(arr))]
#           )
#       }
#     }
#
#   }
#
#   # Open the file
#   con = file(filename, 'wb')
#
#   for (r in records) {
#     for (rr in r) {
#       writeBin(c(
#         writeBin(as.integer(length(rr)), con = raw(), size = 4),
#         rr,
#         writeBin(as.integer(length(rr)), con = raw(), size = 4)
#       ), con = con)
#     }
#   }
#
#
#   # Close the file
#   close(con)
#
# }
