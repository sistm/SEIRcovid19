#' SetAllParameter generic
#'
#' @param obj Object to set
#' @param parameter List of new parameter
#' @export
SetAllParameter <- function(obj, parameter)
{
  UseMethod("SetAllParameter",obj)
}

#' @export
SetAllParameter.default <- function(obj, parameter)
{
  print("No method implemented for this class")
  return(obj)
}

#' @describeIn SetAllParameter Set parameter for an object of class \code{OdeSystem}
#' @export
SetAllParameter.OdeSystem <- function(ode, parameter)
{
  print("Set all parameter for OdeSytem")
  if (length(ode$parameter)==length(parameter)){
    ode$parameter <- parameter
    return(ode)

  }
  else{
    stop("Argument should have same length as Attribute")
  }
}
