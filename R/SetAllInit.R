#' SetAllInit generic
#'
#' @param obj Object to set
#' @param init List of new initState
#' @export
#'
SetAllInit <- function(obj, init)
{
  UseMethod("SetAllInit",obj)
}

#' @export
SetAllInit.default <- function(obj, init)
{
  print("No method implemented for this class")
  return(obj)
}

#' @describeIn SetAllInit Set InitState for an object of class \code{OdeSystem}
#' @export
SetAllInit.OdeSystem <- function(ode, init)
{
  print("Set all init state for OdeSytem")
  if (length(ode$InitState)==length(init)){
    ode$InitState <- init
    return(ode)

  }
  else{
    stop("Argument should have same length as Attribute")
  }
}
