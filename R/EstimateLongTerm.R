#' @export
EstimateLongTerm<-function(ode_list,time_date,time,regressor_info){
  
  indivParams <-read.table(paste(here::here(),'/MonolixFile/',"/outputMonolix/",ode_list[[1]]$nameproject,"/IndividualParameters/estimatedIndividualParameters.txt",sep=""),header=TRUE,sep=",")
  popParams<-read.table(paste(here::here(),'/MonolixFile/',"/outputMonolix/",ode_list[[1]]$nameproject,"/populationParameters.txt",sep=""),header=TRUE,sep=",")
  data<-read.table(ode_list[[1]]$DataInfo$File,sep=ode_list[[1]]$DataInfo$Sep,header=TRUE)
  InputNames<-colnames(data)
  timename<-InputNames[ode_list[[1]]$DataInfo$HeaderType=="time"]
  TimeDependantParameter<-c()
  for (id in 1:length(ode_list)){
    reg_ode<-regressor_info
    index<-which(time_date==min(as.Date(ode_list[[id]]$ObsData$date)))
    for (i in 1:length(reg_ode)){
      
      reg_ode[[i]]$time<-reg_ode[[i]]$time[index:length(time_date)]
      reg_ode[[i]]$value<-reg_ode[[i]]$value[index:length(time_date)]
      
    }
    reg_ode[[i]]$value
    time_ode<-time[index:length(time_date)]
    
    ode_list[[id]]<-WriteEstimationModelLongTerm(ode_list[[id]], ModeFilename, TimeSpecificEquation, ModelMathBloc,regressor_info)
    # Long terme estiamtion
    param_and_init<-c(ode_list[[id]]$parameter,ode_list[[id]]$InitState)

    # Solve
    result<-SolveThroughSimulx(ode_list[[id]],is_global = 1,time_ode,param_and_init,reg_ode,TimeDependantParameter,IsLongTerm = TRUE)
    result$date<-time_date[index:length(time_date)]
    ode_list[[id]]$LongTerm<-result
    # Long terme ICmin
    ICLong<-ComputeConfidenceInterval(ode_list[[id]],indivParams,popParams,id,nb_mc=100,is_global=1,timename,TimeDependantParameter,IsLongTerm=TRUE,LongTermReg=reg_ode)
    ode_list[[id]]$LongTermMin<-ICLong$LongTermMin
    ode_list[[id]]$LongTermMax<-ICLong$LongTermMax
    
  }
  
  
  return(ode_list)
  
}