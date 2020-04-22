#' SetInitVariability generic
#'
#' @param obj Object to set
#' @param InitVaria List of new boolean
#' @export





SetInitVariability <- function(obj, InitVaria)
{
  UseMethod("SetInitVariability",obj)
}
#' @describeIn default
SetInitVariability.default <- function(obj, InitVaria)
{
  print("No method implemented for this class")
  return(obj)
}

#' @describeIn Set Variability info for init of an object of class an object of class \code{OdeSystem}
SetInitVariability.OdeSystem <- function(ode, InitVaria)
{
  if (length(InitVaria)==length(ode$Variability$init)){
    print("Set all Optimizable Init info")
    ode$Variability$init<-InitVaria
  }else{
    print("Set some Optimizable Init info")
    for (p in 1:length(InitVaria)){
      ode$Variability$init[names(ode$InitState)==names(InitVaria[p])]<-InitVaria[[p]]
    }
  }
  return(ode)
}