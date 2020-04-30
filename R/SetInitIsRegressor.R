#' SetInitIsRegressor generic
#'
#' @param obj Object to set
#' @param InitRegressor List of new int
#' @export





SetInitIsRegressor <- function(obj, InitRegressor)
{
  UseMethod("SetInitIsRegressor",obj)
}
#' @describeIn default
SetInitIsRegressor.default <- function(obj, InitRegressor)
{
  print("No method implemented for this class")
  return(obj)
}

#' @describeIn Set Regressor info for init attribute of an object of class \code{OdeSystem}
#' @export
SetInitIsRegressor.OdeSystem <- function(ode, InitRegressor)
{
  if (length(InitRegressor)==length(ode$IsRegressor$init)){
    ode$IsRegressor$init<-InitRegressor
  }else{
    for (p in 1:length(InitRegressor)){
      ode$IsRegressor$init[names(ode$InitState)==names(InitRegressor[p])]<-InitRegressor[[p]]
    }
  }
  return(ode)
}