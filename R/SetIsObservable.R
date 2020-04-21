#' SetIsObservable generic
#'
#' @param obj Object to set
#' @param StateObservable List of new boolean
#' @export





SetIsObservable <- function(obj, StateObservable)
{
  UseMethod("SetIsObservable",obj)
}
#' @describeIn default
SetIsObservable.default <- function(obj, StateObservable)
{
  print("No method implemented for this class")
  return(obj)
}

#' @describeIn Set Observable info for state of an object of class an object of class \code{OdeSystem}
SetIsObservable.OdeSystem <- function(ode, StateObservable)
{
  if (length(StateObservable)==length(ode$IsStateObservable)){
    print("Set all Observable info")
    ode$IsStateObservable<-StateObservable
  }else{
    print("Set some Observable info")
    for (p in 1:length(StateObservable)){
      ode$IsStateObservable[names(ode$InitState)==names(StateObservable[p])]<-StateObservable[[p]]
    }
  }
  return(ode)
}