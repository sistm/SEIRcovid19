#' SetParamIsRegressor generic
#'
#' @param obj Object to set
#' @param ParamRegressor List of new Regressor info
#' @export





SetParamIsRegressor <- function(obj, ParamRegressor)
{
  UseMethod("SetParamIsRegressor",obj)
}
#' @describeIn default
SetParamIsRegressor.default <- function(obj, ParamRegressor)
{
  print("No method implemented for this class")
  return(obj)
}

#' @describeIn Set Regressor info for parameter attribute of an object of class \code{OdeSystem}
SetParamIsRegressor.OdeSystem <- function(ode, ParamRegressor)
{
  if (length(ParamRegressor)==length(ode$IsRegressor$param)){
    ode$IsRegressor$param<-ParamRegressor
  }else{
    for (p in 1:length(ParamRegressor)){
      ode$IsRegressor$param[names(ode$parameter)==names(ParamRegressor[p])]<-ParamRegressor[[p]]
    }
  }
  return(ode)
}