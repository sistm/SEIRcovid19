#' SetAllInit generic
#'
#' @param obj Object to set
#' @param parameter List of new parameter
#' @export

foobar.numeric <- function(x) abs(mean(x) - median(x))




SetAllInit <- function(obj, init)
{
  UseMethod("SetAllInit",obj)
}
#' @describeIn default
SetAllInit.default <- function(obj, init)
{
  print("No method implemented for this class")
  return(obj)
}

#' @describeIn Set parameter for an object of class an object of class \code{OdeSystem}
SetAllInit.OdeSystem <- function(ode, init)
{
  print("Set all init state for OdeSytem")
  ode$InitState <- init
  return(ode)
}