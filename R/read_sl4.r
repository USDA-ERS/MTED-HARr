#' @title Read an SL4 solution file
#' @name read_SL4
#' @description This function reads SL4 solution files into a list
#' @param filename Path to SL4 file
#' @return A list of variables
#' @export
read_SL4 = function(filename, toLowerCase = TRUE) {
  #filename = "C:\\Users\\MAROS.IVANIC\\OneDrive - USDA\\F2F\\Models\\Modified AEZ\\Solutions\\update.sl4"
  #filename = "C:\\Users\\MAROS.IVANIC\\OneDrive - USDA\\F2F\\Models\\Modified AEZ\\Solutions\\f2faverage.sl4"

  # Read the solution as a HAR file
  solution = read_har(filename, toLowerCase = toLowerCase)

  #browser()

  if(toLowerCase){
    names(solution) = toupper(names(solution))
  }

  # Logical vector to identify those variables that have some exogenous components
  partials = solution$OREX > 0 & solution$OREX != solution$VNCP

  # Initialize an empty list for the results
  results = list()

  # Loop through every variable found in the solution
  for (v in 1:length(solution$VCNM)) {

    # If the variable has zero dimensions create empty dimensions
    if (solution$VCNI[v] == 0) {
      dimensions = list()
    } else{
      # Otherwise read the set names
      dimensions =  Map(function(f) {
        solution$STEL[solution$ELAD[f]:(solution$ELAD[f] + solution$SSZ[f] - 1)]
      }, solution$VCSN[solution$VCSP[v]:(solution$VCSP[v] + solution$VCNI[v] -
                                           1)])
      # Read and assign set names
      names(dimensions) = Map(function(f) {
        solution$STNM[f]
      }, solution$VCSN[solution$VCSP[v]:(solution$VCSP[v] + solution$VCNI[v] -
                                           1)])
    }

    # Create a separate dimension for the total solution and possible subtotals
    dimensions[['subtotals']] = c('TOTAL', solution$STDS)

    # Find the position of the variable in the solution list (some variables may not have values)
    vv = which(solution$VARS == solution$VCNM[v])

    # Check if the variable has values to process it
    if (length(vv) > 0) {

      # Check if the variable has any endogenous components
      if (solution$ORND[vv] == 0) {
        # The variable has no endogenous components, specify zeros
        data = rep(0,solution$VNCP[vv])
      } else {
        # The variable has some endogenous components; read those
        data = solution$CUMS[solution$PCUM[vv]:(solution$PCUM[vv] + solution$ORND[vv] -
                                                  1)]

        if (length(solution$STDS) > 0) {
          data = c(data,
                   unlist(Map(
                     function(f) {
                       solution[[paste0(paste(rep('0', 3 - nchar(f)), collapse = ''), f, 'S')]][solution$PCUM[vv]:(solution$PCUM[vv] +
                                                                                                                     solution$ORND[vv] - 1)]
                     } , 1:length(solution$STDS)
                   )))
        }

      }

      # If this is a partially exogenous variable, recast it
      if (partials[vv]) {
        excludedElements = solution$OREL[(sum(solution$OREX[1:vv][partials[1:vv]]) -
                                            solution$OREX[vv][partials[vv]] + 1):sum(solution$OREX[1:vv][partials[1:vv]])]

        newData = rep(0, solution$VNCP[vv] * (length(solution$STDS) + 1))

        allExcludedElements = excludedElements
        if (length(solution$STDS) > 0) {
          allExcludedElements = c(allExcludedElements, unlist(Map(
            function(f) {
              f * solution$VNCP[vv] + excludedElements
            }, 1:length(solution$STDS)
          )))

        }

        newData[-allExcludedElements] = data
        data = newData
      }

      # If this variable has any shocked components, speficy those
      if(solution$SHCK[vv]>0){
        #shocks = solution$SHOC

        partialShocks = solution$SHCK<solution$VNCP &solution$SHCK>0

        # See if the entire variable is shocked
        if(solution$SHCK[vv]==solution$VNCP[vv] ){
          components = 1:solution$VNCP[vv]
        } else {
          components = solution$SHCL[(sum(solution$SHCK[1:vv][partialShocks[1:vv]])-solution$SHCK[vv]+1) : sum(solution$SHCK[1:vv][partialShocks[1:vv]])]

        }
        values = solution$SHOC[(sum(solution$SHCK[1:vv])-solution$SHCK[vv]+1) : sum(solution$SHCK[1:vv])]

        tempShock = rep(0,solution$VNCP[vv])
        tempShock[components]=values

        if(length(solution$STDS)>0){
          temp = Map( function(f){
            stC = paste0(paste(rep('0', 3 - nchar(f)), collapse = ''), f, 'C')
            stL = paste0(paste(rep('0', 3 - nchar(f)), collapse = ''), f, 'L')

            #solution[[stC]][vv]
            #f * solution$VNCP[vv] +
            comps = NULL
            if((sum(solution[[stC]][1:vv])-solution[[stC]][vv]+1)<= sum(solution[[stC]][1:vv])){
              comps = solution[[stL]][ (sum(solution[[stC]][1:vv])-solution[[stC]][vv]+1): sum(solution[[stC]][1:vv])]
            }

            vals = tempShock[comps]

            return(list(f * solution$VNCP[vv]+comps, vals))
          }, 1:length(solution$STDS))

          components = c(components,unlist(Map(function(f)f[[1]], temp)))
          values = c(values, unlist(Map(function(f)f[[2]], temp)))
        }

        data[components]=values

      }


    } else {
      data = NA
    }


    results[[solution$VCNM[v]]] = array(data, dim = unlist(Map(function(f)
      length(f), dimensions)), dimnames = dimensions)
  }
  return(results)
}
