#' SetSomeInit generic
#'
#' @param obj Object to set
#' @param init List of new initState
#' @export
#' 

SetSomeInit <- function(obj, init)
{
  UseMethod("SetSomeInit",obj)
}

SetSomeInit.default <- function(obj, init)
{
  print("No method implemented for this class")
  return(obj)
}

#' @describeIn Set Some InitState for an object of class \code{OdeSystem}
SetSomeInit.OdeSystem <- function(ode, init)
{
  for (p in 1:length(init)){
    ode$InitState[names(ode$InitState)==names(init[p])]<-init[[p]]
  }
  return(ode)
}