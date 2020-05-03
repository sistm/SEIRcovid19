#' SetParamVariability generic
#'
#' @param obj Object to set
#' @param ParamVaria List of new variability
#' @export
SetParamVariability <- function(obj, ParamVaria)
{
  UseMethod("SetParamVariability",obj)
}

#' @export
SetParamVariability.default <- function(obj, ParamVaria)
{
  print("No method implemented for this class")
  return(obj)
}

#' @describeIn SetParamVariability Set Variability for parameter attribute of an object of class \code{OdeSystem}
#' @export
SetParamVariability.OdeSystem <- function(ode, ParamVaria)
{
  if (length(ParamVaria)==length(ode$Variability$param)){
    ode$Variability$param<-ParamVaria
  }else{
    for (p in 1:length(ParamVaria)){
      ode$Variability$param[names(ode$parameter)==names(ParamVaria[p])]<-ParamVaria[[p]]
    }
  }
  return(ode)
}
