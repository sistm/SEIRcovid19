#' Estimate generic
#'
#' @param obj Object to set
#' @param time List of new initState
#' @export
Estimate <- function(obj, time,is_global,regressor_value,TimeDependantParameter)
{
  UseMethod("Estimate",obj)
}

#' @export
Estimate.default <- function(obj, time,is_global,regressor_value,TimeDependantParameter)
{
  print("No method implemented for this class")
  return(obj)
}

#' @describeIn Estimate Estimation after optimisation for an object of class \code{OdeSystem}
#' @export
Estimate.OdeSystem <- function(ode, time,is_global=0,regressor_value,TimeDependantParameter)
{
  .hiddenCall("lixoftConnectorsState<-lixoftConnectors::getLixoftConnectorsState(quietly = TRUE)")

  if (lixoftConnectorsState$software == "simulx"){ # => nothing to be done
  }else{
    ".hiddenCall(lixoftConnectors::initializeLixoftConnectors(software='simulx',force=TRUE))"
  }

  param_and_init<-c(ode$parameter,ode$InitState)
  param_and_init<-c(ode$parameter[ode$IsRegressor$param==0],ode$InitState[ode$IsRegressor$init==0])
  regressor<-paste(c(names(ode$InitState[ode$IsRegressor$init>0]),names(ode$parameter[ode$IsRegressor$param>0])),collapse=',')
  regressor<-unlist(strsplit(regressor, ","))
  regressor_info<-list()
  if (length(regressor)>0){
    for (ireg in 1:length(regressor)){
      regressor_info[[ireg]]<-list(name=regressor[ireg],
                                   time=time,
                                   value=regressor_value[[which(names(regressor_value)==regressor[ireg])]])
    }
  }
  
  result<-SolveThroughSimulx(ode,is_global,time,param_and_init,regressor_info,TimeDependantParameter)
  ode$solution<-select(result,-TimeDependantParameter)
  ode$TimeDependantParameter<-select(result,TimeDependantParameter)
  return(ode)

}
