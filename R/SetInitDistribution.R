#' SetInitDistribution generic
#'
#' @param obj Object to set
#' @param InitDist List of new value
#' @export





SetInitDistribution <- function(obj, InitDist)
{
  UseMethod("SetInitDistribution",obj)
}
#' @describeIn default
SetInitDistribution.default <- function(obj, InitDist)
{
  print("No method implemented for this class")
  return(obj)
}

#' @describeIn Set Distribution for init attribute of an object of class \code{OdeSystem}
#' @export
SetInitDistribution.OdeSystem <- function(ode, InitDist)
{
  if (length(InitDist)==length(ode$Distribution$init)){
    print("Set all Regressor Parameter info")
    ode$Distribution$init<-InitDist
  }else{
    print("Set some Regressor Parameter info")
    for (p in 1:length(InitDist)){
      ode$Distribution$init[names(ode$InitState)==names(InitDist[p])]<-InitDist[[p]]
    }
  }
  return(ode)
}