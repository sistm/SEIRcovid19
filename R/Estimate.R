#' Estimate generic
#'
#' @param obj Object to set
#' @param time List of new initState
#' @export
#' 
Estimate <- function(obj, time,is_global)
{
  UseMethod("Estimate",obj)
}
#' @describeIn default
Estimate.default <- function(obj, time,is_global)
{
  print("No method implemented for this class")
  return(obj)
}

#' @describeIn Estimation after optimisation for an object of class \code{OdeSystem}
#' @export
Estimate.OdeSystem <- function(ode, time,is_global=0)
{
  lixoftConnectorsState<-lixoftConnectors::getLixoftConnectorsState(quietly = TRUE)
  
  if (lixoftConnectorsState$software == "simulx"){ # => nothing to be done
  }else{
    lixoftConnectors::initializeLixoftConnectors(software="simulx",force=TRUE)
  }

  param_and_init<-c(ode$parameter,ode$InitState)
  result<-SolveThroughSimulx(ode,is_global,time,param_and_init)
  ode$solution<-result
  return(ode)
  
}