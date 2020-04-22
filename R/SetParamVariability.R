#' SetParamVariability generic
#'
#' @param obj Object to set
#' @param ParamVaria List of new boolean
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

#' @describeIn Set Variability param for parameter of an object of class an object of class \code{OdeSystem}
SetParamVariability.OdeSystem <- function(ode, ParamVaria)
{
  if (length(ParamVaria)==length(ode$Variability$param)){
    print("Set all Regressor Parameter info")
    ode$Variability$param<-ParamVaria
  }else{
    print("Set some Regressor Parameter info")
    for (p in 1:length(ParamVaria)){
      ode$Variability$param[names(ode$parameter)==names(ParamVaria[p])]<-ParamVaria[[p]]
    }
  }
  return(ode)
}