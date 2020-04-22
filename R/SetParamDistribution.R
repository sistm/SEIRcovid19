#' SetParamDistribution generic
#'
#' @param obj Object to set
#' @param ParamDist List of new boolean
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

#' @describeIn Set Distribution for parameter of an object of class an object of class \code{OdeSystem}
SetParamDistribution.OdeSystem <- function(ode, ParamDist)
{
  if (length(ParamDist)==length(ode$Distribution$param)){
    print("Set all Regressor Parameter info")
    ode$Distribution$param<-ParamDist
  }else{
    print("Set some Regressor Parameter info")
    for (p in 1:length(ParamDist)){
      ode$Distribution$param[names(ode$parameter)==names(ParamDist[p])]<-ParamDist[[p]]
    }
  }
  return(ode)
}