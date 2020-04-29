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
Estimate.OdeSystem <- function(ode, time,is_global=0)
{
  lixoftConnectorsState<-lixoftConnectors::getLixoftConnectorsState(quietly = TRUE)
  
  if (lixoftConnectorsState$software == "simulx"){ # => nothing to be done
  }else{
    lixoftConnectors::initializeLixoftConnectors(software="simulx",force=TRUE)
  }

  
  
  pk.model<-ode$ModelFileEstimation
  # Get parameter and initState
  param_and_init<-c(ode$parameter,ode$InitState)
  number_param_exept_init<-length(ode$parameter)
  if (is_global==1){
    # mlxR format
    C <- list(name=ode$ModelName, time=time)
    solution <- mlxR::simulx(model     = pk.model, output    = C,parameter = param_and_init)
    #Output format of mlxR is : time obs_1,time obs_2 ... time obs_n
    result<-as.data.frame(solution)
    result<-result[,c(1,seq(2,length(ode$ModelName)*2,by=2))]
    colnames(result)<-c("time",ode$ModelName)
  }else{
    #Initialisation of the solution
    C <- list(name=c(ode$ModelName), time=time[1:2])
    #Predict the estimation
    solution <- mlxR::simulx(model     = pk.model, output    = C,parameter = param_and_init)
    #Output format of mlxR is : time obs_1,time obs_2 ... time obs_n
    result<-as.data.frame(solution)
    result<-result[,c(1,seq(2,length(ode$ModelName)*2,by=2))]
    colnames(result)<-c("time",ode$ModelName)
    if (length(time)>2){
      for (i in 3:length(time)){
        res_time<-(result[which(result[,"time"]==time[i-1]),2:7])
        # Set the init as the last observation
        param_and_init[(number_param_exept_init+1):(length(param_and_init))]<-as.numeric(res_time)
        C$time<-c(time[i-1],time[i])
        temp <- mlxR::simulx(model     = pk.model, output    = C,parameter = param_and_init)
        temp<-as.data.frame(temp)
        result<-rbind(result,as.numeric(temp[2,c(1,seq(2,length(ode$ModelName)*2,by=2))]))
      }
    }
  }
  ode$solution<-result
  return(ode)
  
}