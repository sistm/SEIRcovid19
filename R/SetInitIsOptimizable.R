#' SetInitIsOptimizable generic
#'
#' @param obj Object to set
#' @param InitOptim List of new boolean
#' @export





SetInitIsOptimizable <- function(obj, InitOptim)
{
  UseMethod("SetInitIsOptimizable",obj)
}
#' @describeIn default
SetInitIsOptimizable.default <- function(obj, InitOptim)
{
  print("No method implemented for this class")
  return(obj)
}

#' @describeIn Set Optimizable info for init of an object of class an object of class \code{OdeSystem}
SetInitIsOptimizable.OdeSystem <- function(ode, InitOptim)
{
  if (length(InitOptim)==length(ode$IsOptimizable$init)){
    print("Set all Optimizable Init info")
    ode$IsOptimizable$init<-InitOptim
  }else{
    print("Set some Optimizable Init info")
    for (p in 1:length(InitOptim)){
      ode$IsOptimizable$init[names(ode$InitState)==names(InitOptim[p])]<-InitOptim[[p]]
    }
  }
  return(ode)
}