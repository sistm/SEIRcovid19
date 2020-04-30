#' SetParamDistribution generic
#'
#' @param obj Object to set
#' @param ParamDist List of new Distribution
#' @export





SetParamDistribution <- function(obj, ParamDist)
{
  UseMethod("SetParamDistribution",obj)
}
#' @describeIn default
SetParamDistribution.default <- function(obj, ParamDist)
{
  print("No method implemented for this class")
  return(obj)
}

#' @describeIn Set Distribution for parameter attribute of an object of class \code{OdeSystem}
#' @export
SetParamDistribution.OdeSystem <- function(ode, ParamDist)
{
  if (length(ParamDist)==length(ode$Distribution$param)){
    ode$Distribution$param<-ParamDist
  }else{
    for (p in 1:length(ParamDist)){
      ode$Distribution$param[names(ode$parameter)==names(ParamDist[p])]<-ParamDist[[p]]
    }
  }
  return(ode)
}