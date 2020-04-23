#' SetSomeParameter generic
#'
#' @param obj Object to set
#' @param isspecific define is the init state is specific or not
#' @export
SetSpecificInit <- function(obj, isspecific)
{
  UseMethod("SetSpecificInit",obj)
}

SetSpecificInit.default <- function(obj, isspecific)
{
  print("No method implemented for this class")
  return(obj)
}

#' @describeIn Set IsSpecificInit attribute for an object of class \code{OdeSystem}

SetSpecificInit.OdeSystem <- function(ode, isspecific)
{
  if (length(isspecific)==length(ode$IsSpecificInit)){
    print("Set all Specific Init info")
    ode$IsSpecificInit<-isspecific
  }else{
    print("Set some Specific Init info")
    for (p in 1:length(isspecific)){
      ode$IsSpecificInit[names(ode$InitState)==names(isspecific[p])]<-isspecific[[p]]
    }
  }
  return(ode)
}