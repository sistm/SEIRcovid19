#' SetSomeParameter generic
#'
#' @param obj Object to set
#' @param parameter List of new parameter
#' @export
SetSomeParameter <- function(obj, parameter)
{
  UseMethod("SetSomeParameter",obj)
}

#' @export
SetSomeParameter.default <- function(obj, parameter)
{
  print("No method implemented for this class")
  return(obj)
}

#' @describeIn SetSomeParameter Set some parameter for an object of class \code{OdeSystem}
#' @export

SetSomeParameter.OdeSystem <- function(ode, param)
{
  for (p in 1:length(param)){
    ode$parameter[names(ode$parameter)==names(param[p])]<-param[[p]]
  }
  return(ode)
}
