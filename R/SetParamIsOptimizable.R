#' SetParamIsOptimizable generic
#'
#' @param obj Object to set
#' @param ParamOptim List of new boolean
#' @export





SetParamIsOptimizable <- function(obj, ParamOptim)
{
  UseMethod("SetParamIsOptimizable",obj)
}
#' @describeIn default
SetParamIsOptimizable.default <- function(obj, ParamOptim)
{
  print("No method implemented for this class")
  return(obj)
}

#' @describeIn Set Regressor param for init of an object of class an object of class \code{OdeSystem}
SetParamIsOptimizable.OdeSystem <- function(ode, ParamOptim)
{
  if (length(ParamOptim)==length(ode$IsOptimizable$param)){
    print("Set all Regressor Parameter info")
    ode$IsOptimizable$param<-ParamOptim
  }else{
    print("Set some Regressor Parameter info")
    for (p in 1:length(ParamOptim)){
      ode$IsOptimizable$param[names(ode$parameter)==names(ParamOptim[p])]<-ParamOptim[[p]]
    }
  }
  return(ode)
}