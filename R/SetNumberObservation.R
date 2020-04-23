#' SetNumberObservation generic
#'
#' @param obj Object to set
#' @param Nobs Number of observation
#' @export
#' 

SetNumberObservation <- function(obj, Nobs)
{
  UseMethod("SetNumberObservation",obj)
}

SetNumberObservation.default <- function(obj, Nobs)
{
  print("No method implemented for this class")
  return(obj)
}

#' @describeIn Set number of observation for an object of class \code{OdeSystem}
SetNumberObservation.OdeSystem <- function(ode, Nobs)
{
  if (length(Nobs)==1 & is.numeric(Nobs)){
    ode$NumberObservation<-Nobs
    return(ode)
  }else{
    stop("Argument should be numeric and of length 1")
  }
  
}