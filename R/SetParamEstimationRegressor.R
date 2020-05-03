#' SetParamEstimationRegressor generic
#'
#' @param obj Object to set
#' @param ParamRegressor List of new Regressor info
#' @export
SetParamEstimationRegressor <- function(obj, ParamRegressor)
{
  UseMethod("SetParamEstimationRegressor",obj)
}

#' @export
SetParamEstimationRegressor.default <- function(obj, ParamRegressor)
{
  print("No method implemented for this class")
  return(obj)
}

#' @describeIn SetParamEstimationRegressor Set Regressor info for parameter attribute of an object of class \code{OdeSystem}
#' @export
SetParamEstimationRegressor.OdeSystem <- function(ode, ParamRegressor)
{
  if (length(ParamRegressor)==length(ode$EstimationRegressor$param)){
    ode$EstimationRegressor$param<-ParamRegressor
  }else{
    for (p in 1:length(ParamRegressor)){
      ode$EstimationRegressor$param[names(ode$parameter)==names(ParamRegressor[p])]<-ParamRegressor[[p]]
    }
  }
  return(ode)
}
