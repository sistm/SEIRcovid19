#' SetInitIsRegressor generic
#'
#' @param obj Object to set
#' @param InitRegressor List of new boolean
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

#' @describeIn Set Regressor info for init of an object of class an object of class \code{OdeSystem}
SetInitIsRegressor.OdeSystem <- function(ode, InitRegressor)
{
  if (length(InitRegressor)==length(ode$IsRegressor$init)){
    print("Set all Optimizable Parameter info")
    ode$IsRegressor$init<-InitRegressor
  }else{
    print("Set some Optimizable Parameter info")
    for (p in 1:length(InitRegressor)){
      ode$IsRegressor$init[names(ode$InitState)==names(InitRegressor[p])]<-InitRegressor[[p]]
    }
  }
  return(ode)
}