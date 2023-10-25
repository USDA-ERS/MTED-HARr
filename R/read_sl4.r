#' @title Read an SL4 solution file
#' @name read_SL4
#' @description This function reads SL4 solution files into a list
#' @param filename Path to SL4 file
#' @return A list of variables
#' @export
read_SL4 = function(filename, toLowerCase = TRUE) {

  # Read the solution as a HAR file
  solution = read_har(filename, toLowerCase = toLowerCase)

  #browser()

  if(toLowerCase){
    names(solution) = toupper(names(solution))
  }

  subtotals = c('TOTAL', solution$STDS)

  # Create empty arrays for eac variable

  results =

    Map(function(f) {

      if(solution$VCNI[f]>0){
        dimensions = solution$VCSN[solution$VCSP[f]:(solution$VCSP[f]+solution$VCNI[f]-1)]
        sizes = c(solution$SSZ[dimensions],length(subtotals))
        labels = c(Map(function(g)if(solution$SSZ[g]==0) c() else solution$STEL[solution$ELAD[g]:(solution$ELAD[g]+solution$SSZ[g]-1)], dimensions),list(subtotals))
        names(labels) = c(solution$STNM[ dimensions],'subtotal')

      } else {
        sizes = c(length(subtotals))
        labels = list(subtotals)
        names(labels) = c('subtotal')

      }
      array(NA, dim = sizes, dimnames = labels)


    }
    # Loop over all variables
    , 1:length(solution$VCNM)

    )



  names(results) = solution$VCNM

  partials = solution$OREX > 0 & solution$OREX != solution$VNCP


  stHeaders = c('CUMS',unlist(Map(function(f)sprintf('%sS',formatC(f,width=3, zero.print = TRUE, flag = "0")), 1:length(solution$STDS))))


  # Assign values to the endogenous variables with no partials

  for (v in which(partials == FALSE & solution$PCUM>0)) {
    range = solution$PCUM[v]:(solution$PCUM[v]+solution$ORND[v]-1)
    results[[solution$VARS[v]]][] =  unlist(Map(function(f) solution[[f]][range] , stHeaders))
  }

  # Assign zeros to the exogenous variables with no partials

  for (v in which(partials == FALSE & solution$PCUM==0)) {
    results[[solution$VARS[v]]][] =  0

  }

  start = 1
  for (v in which(partials == TRUE & solution$PCUM>0)) {
    range = solution$PCUM[v]:(solution$PCUM[v]+solution$ORND[v]-1)

    positions = solution$ORNL[start - 1 + (1 : solution$ORND[v]) ]

    start = solution$ORND[v]+start

    toFill = rep(FALSE, solution$VNCP[v])

    toFill[positions] = TRUE

    results[[solution$VARS[v]]][toFill] =  unlist(Map(function(f) solution[[f]][range] , stHeaders))
  }

  # Assign exogenous to partials

  start = 1
  for (v in which(partials == TRUE & solution$PCUM>0)) {
    positions = solution$OREL[start - 1 + (1 : solution$OREX[v]) ]

    start = solution$OREX[v]+start

    toFill = rep(FALSE, solution$VNCP[v])

    toFill[positions] = TRUE

    results[[solution$VARS[v]]][toFill] =  0
  }

  for(st in 0:(length(subtotals)-1)){

    if(st ==0){
      SHCK = 'SHCK'
      SHCL = 'SHCL'
    } else {
      SHCK = sprintf('%sC',formatC(st,width=3, zero.print = TRUE, flag = "0"))
      SHCL = sprintf('%sL',formatC(st,width=3, zero.print = TRUE, flag = "0"))
    }


    # Fill in the shocks for TOTAL fully shocked variables
    for(v in which(solution[[SHCK]]>0 & solution[[SHCK]] ==solution$VNCP)){

      positions = 1:solution$VNCP[v]

      toFill = rep(FALSE, length(results[[solution$VARS[v]]]))

      toFill[positions + st * solution$VNCP[v]]=TRUE

      results[[solution$VARS[v]]][toFill] = solution$SHOC[solution$PSHK[v]:(solution$PSHK[v]-1+solution$VNCP[v])]
    }


    # Fill in the shocks for TOTAL partially shocked variables
    start = 1
    for(v in which(solution[[SHCK]]>0 & solution[[SHCK]] < solution$VNCP)){

      positions = solution[[SHCL]][start:(start+solution[[SHCK]][v]-1)]

      toFill = rep(FALSE, length(results[[solution$VARS[v]]]))

      toFill[positions + st * solution$VNCP[v]]=TRUE

      start = solution$SHCK[v]+start

      results[[solution$VARS[v]]][toFill] = solution$SHOC[solution$PSHK[v]:(solution$PSHK[v]-1+solution[[SHCK]][v])]
    }


  }

  return(results)
}
