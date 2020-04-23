#' SetNumberRandomEffect generic
#'
#' @param obj Object to set
#' @param Nrand Number of random effect
#' @export
#' 

SetNumberRandomEffect <- function(obj, Nrand)
{
  UseMethod("SetNumberRandomEffect",obj)
}

SetNumberRandomEffect.default <- function(obj, Nrand)
{
  print("No method implemented for this class")
  return(obj)
}

#' @describeIn Set number of random effect for an object of class \code{OdeSystem}
SetNumberRandomEffect.OdeSystem <- function(ode, Nrand)
{
  if (length(Nrand)==1 & is.numeric(Nrand)){
    ode$NumberRandomEffect<-Nrand
    return(ode)
  }else{
    stop("Argument should be numeric and of length 1")
  }
  
}