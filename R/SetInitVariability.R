#' SetInitVariability generic
#'
#' @param obj Object to set
#' @param InitVaria List of new variability
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

#' @describeIn Set Variability info for init attribute of an object of class \code{OdeSystem}
SetInitVariability.OdeSystem <- function(ode, InitVaria)
{
  if (length(InitVaria)==length(ode$Variability$init)){
    ode$Variability$init<-InitVaria
  }else{
    for (p in 1:length(InitVaria)){
      ode$Variability$init[names(ode$InitState)==names(InitVaria[p])]<-InitVaria[[p]]
    }
  }
  return(ode)
}