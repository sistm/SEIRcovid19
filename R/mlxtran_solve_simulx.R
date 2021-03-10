#'@export
#'
mlxtran_solve_simulx<-function(pk.model,time,param,init,model_name){
  .hiddenCall("lixoftConnectors::initializeLixoftConnectors(software='simulx',force=TRUE)")
  #Initialisation of the solution
  C <- list(name=c(model_name), time=time[1:2])
  param_and_init<-c(param,init)
  number_param_exept_init<-length(param)
  #Predict the estimation
  solution <- mlxR::simulx(model     = pk.model, output    = C,parameter = param_and_init)
  #Output format of mlxR is : time obs_1,time obs_2 ... time obs_n
  result<-as.data.frame(solution)
  result<-result[,c(1,seq(2,length(model_name)*2,by=2))]
  colnames(result)<-c("time",model_name)
  for (i in 3:length(time)){
    res_time<-(result[which(result[,"time"]==time[i-1]),2:7])
    # Set the init as the last observation
    param_and_init[(number_param_exept_init+1):(length(param_and_init))]<-as.numeric(res_time)
    C$time<-c(time[i-1],time[i])
    temp <- mlxR::simulx(model     = pk.model, output    = C,parameter = param_and_init)
    temp<-as.data.frame(temp)
    result<-rbind(result,as.numeric(temp[2,c(1,seq(2,length(model_name)*2,by=2))]))
  }
  return(result)
}
