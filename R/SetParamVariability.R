#' SetParamVariability generic
#'
#' @param obj Object to set
#' @param ParamVaria List of new variability
#' @export





SetParamVariability <- function(obj, ParamVaria)
{
  UseMethod("SetParamVariability",obj)
}
#' @describeIn default
SetParamVariability.default <- function(obj, ParamVaria)
{
  print("No method implemented for this class")
  return(obj)
}

#' @describeIn Set Variability for parameter attribute of an object of class \code{OdeSystem}
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