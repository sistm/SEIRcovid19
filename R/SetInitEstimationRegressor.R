#' SetInitEstimationRegressor generic
#'
#' @param obj Object to set
#' @param InitRegressor List of new Regressor info
#' @export





SetInitEstimationRegressor <- function(obj, InitRegressor)
{
  UseMethod("SetInitEstimationRegressor",obj)
}
#' @describeIn default
SetInitEstimationRegressor.default <- function(obj, InitRegressor)
{
  print("No method implemented for this class")
  return(obj)
}

#' @describeIn Set Regressor info for parameter attribute of an object of class \code{OdeSystem}
#' @export
SetInitEstimationRegressor.OdeSystem <- function(ode, InitRegressor)
{
  if (length(InitRegressor)==length(ode$EstimationRegressor$init)){
    ode$EstimationRegressor$init<-InitRegressor
  }else{
    for (p in 1:length(InitRegressor)){
      ode$EstimationRegressor$init[names(ode$InitState)==names(InitRegressor[p])]<-InitRegressor[[p]]
    }
  }
  return(ode)
}