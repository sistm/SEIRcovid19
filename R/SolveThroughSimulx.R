#' @export
SolveThroughSimulx<-function(ode,is_global,time,param_and_init,regressor_info,TimeDependantParameter=c()){
  number_param_exept_init<-length(ode$parameter[ode$IsRegressor$param==0])
  pk.model<-ode$ModelFileEstimation
  
  if (is_global==1){
    # mlxR format
    C <- list(name=c(ode$ModelName,TimeDependantParameter), time=time)
    #solution <- mlxR::simulx(model     = pk.model, output    = C,parameter = param_and_init)
    solution <- mlxR::simulx(model     = pk.model, output    = C,parameter = param_and_init,regressor=regressor_info)
    #Output format of mlxR is : time obs_1,time obs_2 ... time obs_n
    result<-as.data.frame(solution)
    result<-result[,c(1,seq(2,(length(ode$ModelName)+length(TimeDependantParameter))*2,by=2))]
    colnames(result)<-c("time",ode$ModelName,TimeDependantParameter)
  }else{
    #Initialisation of the solution
    #C <- list(name=c(ode$ModelName), time=time[1:2])
    reg<-list()
    if (length(regressor_info)>0){
      for (ireg in 1:length(regressor_info)){
        reg[[ireg]]<-regressor_info[[ireg]]
        reg[[ireg]]$time<-reg[[ireg]]$time[1]
        reg[[ireg]]$value<-reg[[ireg]]$value[1]
      }
    }
    C <- list(name=c(ode$ModelName,TimeDependantParameter), time=time[1:2])
    #Predict the estimation
    solution <- mlxR::simulx(model     = pk.model, output    = C,parameter = param_and_init,regressor=reg)
    #Output format of mlxR is : time obs_1,time obs_2 ... time obs_n
    result<-as.data.frame(solution)
    result<-result[,c(1,seq(2,(length(ode$ModelName)+length(TimeDependantParameter))*2,by=2))]
    colnames(result)<-c("time",ode$ModelName,TimeDependantParameter)
    if (length(time)>2){
      for (i in 3:length(time)){
        C$time<-c(time[i-1],time[i])
        res_time<-(result[which(result[,"time"]==time[i-1]),2:(length(ode$ModelName)+1)])
        # Set the init as the last observation
        for (j in 1:length(res_time)){
          param_and_init[names(param_and_init)==(paste("init",colnames(res_time)[j],sep=""))]<-as.numeric(res_time[j])
        }
       
        if (length(regressor_info)>0){
          for (ireg in 1:length(regressor_info)){
            reg[[ireg]]<-regressor_info[[ireg]]
            reg[[ireg]]$time<-reg[[ireg]]$time[i-1]
            if (substr(reg[[ireg]]$name, nchar(reg[[ireg]]$name)-nchar("init"), nchar(reg[[ireg]]$name)-1)=="init"){
              state<-substr(reg[[ireg]]$name,nchar(reg[[ireg]]$name),nchar(reg[[ireg]]$name))
              reg[[ireg]]$value<-as.numeric(result[i-1,which(names(result)==state)])
            }else{
              reg[[ireg]]$value<-reg[[ireg]]$value[i-1]
            }
            
          }
        }
        temp <- mlxR::simulx(model     = pk.model, output    = C,parameter = param_and_init,regressor=reg)
        temp<-as.data.frame(temp[1:(length(ode$ModelName)+length(TimeDependantParameter))])
        result<-rbind(result,as.numeric(temp[2,c(1,seq(2,(length(ode$ModelName)+length(TimeDependantParameter))*2,by=2))]))
      }
    }
  }
  return(result)
  
}