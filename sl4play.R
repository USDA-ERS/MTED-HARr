readSL4 = function(filename) {
  filename = "C:\\Users\\MAROS.IVANIC\\OneDrive - USDA\\F2F\\Models\\Modified AEZ\\Solutions\\f2faverage.sl4"
  solution = read_har(filename)

  results = list()
  for (v in 1:length(solution$VCNM)) {
    dimensions =  Map(function(f) {
      solution$STEL[solution$ELAD[f]:(solution$ELAD[f] + solution$SSZ[f] - 1)]
    }, solution$VCSN[solution$VCSP[v]:(solution$VCSP[v] + solution$VCNI[v] -
                                         1)])

    names(dimensions) = Map(function(f) {
      solution$STNM[f]
    }, solution$VCSN[solution$VCSP[v]:(solution$VCSP[v] + solution$VCNI[v] -
                                         1)])

    dimensions[['subtotals']] = c('TOTAL', solution$STDS)

    vv = which(solution$VARS == solution$VCNM[v])
    if (length(vv) > 0) {
      if (solution$ORND[vv] == 0) {
        data = NA
      } else {
        data = c(solution$CUMS[solution$PCUM[vv]:(solution$PCUM[vv] + solution$ORND[vv] -
                                                    1)],
                 unlist(Map(
                   function(f) {
                     solution[[paste0(paste(rep('0', 3 - nchar(f)), collapse = ''), f, 'S')]][solution$PCUM[vv]:(solution$PCUM[vv] +
                                                                                                                   solution$ORND[vv] - 1)]
                   } , 1:length(solution$STDS)
                 )))
      }

    } else {
      data = NA
    }

    results[[solution$VCNM[v]]] = array(data, dim = unlist(Map(function(f)
      length(f), dimensions)), dimnames = dimensions)
  }
  return(results)
}
