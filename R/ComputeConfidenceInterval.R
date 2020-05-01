#' ComputeConfidenceInterval generic
#'
#' @param obj Object to set
#' @param nb_mc Number of monte carlo simulation
#' @param time time vector of the simulation
#' @param is_global integer for global estimation of time step
#' @param indiv Individual parameter of monolix optimisation
#' @param pop Population parameter of monolix optimisation
#' @param id Index of actual id

#' @export
#' 

ComputeConfidenceInterval <- function(obj,indiv,pop,id,time,nb_mc,is_global=1,regressor_value)
{
  UseMethod("ComputeConfidenceInterval",obj)
}

ComputeConfidenceInterval.default <- function(obj,indiv,pop,id,time,nb_mc,is_global=1,regressor_value)
{
  print("No method implemented for this class")
  return(obj)
}

#' @describeIn Compute monte Carlo estimation for an object of class \code{OdeSystem}
#' @export

ComputeConfidenceInterval.OdeSystem <-function(systemode,indiv,pop,id,time,nb_mc,is_global=1,regressor_value){
  optimize_param_name<-c(names(systemode$parameter[systemode$Variability$param>0]),names(systemode$InitState[systemode$Variability$init>0]))
  SdOptimizeParam<-as.list(rep(NA,length(optimize_param_name)))
  names(SdOptimizeParam)<-optimize_param_name
  OptimizeParam<-as.list(rep(NA,length(optimize_param_name)))
  names(OptimizeParam)<-optimize_param_name
  for (j in 1:length(optimize_param_name)){
    optimize_monolix_name<-paste(optimize_param_name[j],"_sd",sep="")
    SdOptimizeParam[j]<-indiv[id,optimize_monolix_name]
    optimize_monolix_name<-paste(optimize_param_name[j],"_mode",sep="")
    OptimizeParam[j]<-indiv[id,optimize_monolix_name]
    if (SdOptimizeParam[j]==0){
      optimize_pop_name<-paste(optimize_param_name[j],"_pop",sep="")
      SdOptimizeParam[j]<-pop[optimize_pop_name,"stochasticApproximation"]
    }
  }
  regressor<-paste(c(names(systemode$InitState[systemode$EstimationRegressor$init>0]),names(systemode$parameter[systemode$EstimationRegressor$param>0])),collapse=',')
  regressor_info<-list()
  if (length(regressor)>0){
    for (ireg in 1:length(regressor)){
      regressor_info[[ireg]]<-list(name=regressor[[ireg]],
                                   time=time,
                                   value=regressor_value[[names(regressor_value)==regressor[[ireg]]]])
    }
  }
  mc_res <- parallel::mclapply(X = 1:nb_mc, mc.cores=1, FUN=function(mc_cur){
    param_and_init<-c(systemode$parameter,systemode$InitState)
    param_and_init<-c(systemode$parameter[systemode$EstimationRegressor$param==0],systemode$InitState[systemode$EstimationRegressor$init==0])

    #For each simulation update the optimize parameter such as param=param_pop+param_sd*rnorm(1,0)
    for (j in 1:length(optimize_param_name)){
      param_and_init[names(param_and_init)==optimize_param_name[j]]<-as.numeric(OptimizeParam[j])+as.numeric(SdOptimizeParam[j])*rnorm(1,0)
    }
    # Now we want to update the specific init state
    InitSpecific<-GetSpecificInitState(param_and_init,SpecificInitBloc)
    for (j in 1:length(InitSpecific)){
      param_and_init[names(param_and_init)==names(InitSpecific[j])]<-InitSpecific[[j]]
    }
    result<-SolveThroughSimulx(systemode,is_global,time,param_and_init,regressor_info)
    return(result)
  }
  )
  systemode<-SetConfidenceInverval(systemode,mc_res,time)
  return(systemode)
}