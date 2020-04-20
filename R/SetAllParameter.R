#' SetAllParameter generic
#'
#' @param obj Object to set
#' @param parameter List of new parameter
#' @export

foobar.numeric <- function(x) abs(mean(x) - median(x))




SetAllParameter <- function(obj, parameter)
{
  UseMethod("SetAllParameter",obj)
}
#' @describeIn default
SetAllParameter.default <- function(obj, parameter)
{
  print("No method implemented for this class")
  return(obj)
}

#' @describeIn Set parameter for an object of class an object of class \code{OdeSystem}
SetAllParameter.OdeSystem <- function(ode, parameter)
{
  print("Set all parameter for OdeSytem")
  ode$parameter <- parameter
  return(ode)
}