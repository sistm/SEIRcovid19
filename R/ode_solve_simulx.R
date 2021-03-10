#' solve R model ode with simulx
#'
#' @keywords internal
#'
#' @export
#' @importFrom mlxR simulx
ode_solve_simulx<-function(pk.model,time,param,init,model_name){
  .hiddenCall("lixoftConnectors::initializeLixoftConnectors(software='simulx',force=TRUE)")
  C <- list(name=c(model_name), time=time[1:2])
  param_and_init<-c(param,init,number_parameter=length(param))
  
  solution <- mlxR::simulx(model     = pk.model, output    = C,parameter = param_and_init)
  result<-as.data.frame(solution[[model_name]])
  
  for (i in 3:length(time)){
    res_time<-(result[which(result[,"time"]==time[i-1]),2:7])
    init<-c(S=res_time$S,
            E=res_time$E,
            I=res_time$I,
            R=res_time$R,
            A=res_time$A,
            H=res_time$H)
    param_and_init2<-c(param,init,number_parameter=length(param))
    param_and_init[(param_and_init[length(param_and_init)]+1):(length(param_and_init)-1)]<-as.numeric(res_time)
    C$time<-c(time[i-1],time[i])
    temp <- mlxR::simulx(model     = pk.model, output    = C,parameter = param_and_init)
    temp<-as.data.frame(temp[[model_name]])
    result<-rbind(result,as.numeric(temp[2,]))
  }
  return(result)
  
  
}