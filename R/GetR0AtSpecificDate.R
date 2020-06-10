#' @export

GetR0AtSpecificDate<-function(ode_list,dateR0,popsize_name,NationalName){
  R0<-as.data.frame(matrix(NA,length(ode_list)+1,2))
  colnames(R0)<-c("Reg","R0")
  indivParams <-read.table(paste(here::here(),'/MonolixFile/',"/outputMonolix/",ode_list[[1]]$nameproject,"/IndividualParameters/estimatedIndividualParameters.txt",sep=""),header=TRUE,sep=",")
  popsize_per_id<-rep(0,length(ode_list))
  R0$Reg<-c(as.character(indivParams$id),NationalName)
  dateR0<-as.Date(dateR0)
  Rt<-as.data.frame(matrix(NA,length(ode_list),1))
  Rtmin<-as.data.frame(matrix(NA,length(ode_list),1))
  Rtmax<-as.data.frame(matrix(NA,length(ode_list),1))
  for (id in 1:length(ode_list)){
    popsize_per_id[id]<-ode_list[[id]]$parameter[names(ode_list[[id]]$parameter)==popsize_name]
    Rt[id,1]<-ode_list[[id]]$R0[which(as.Date(ode_list[[id]]$R0[,"date"])==dateR0),"R0"]
    Rtmin[id,1]<-ode_list[[id]]$R0[which(as.Date(ode_list[[id]]$R0[,"date"])==dateR0),"R0_min"]
    Rtmax[id,1]<-ode_list[[id]]$R0[which(as.Date(ode_list[[id]]$R0[,"date"])==dateR0),"R0_max"]
    R0$R0[id]<-paste(format(round(Rt[id,1],2),nsmall=0)," [",
                     format(round(Rtmin[id,1],2),nsmall=0),";",
                     format(round(Rtmax[id,1],2),nsmall=0),"]",sep="")
  }
  R0$R0[length(ode_list)+1]<-paste(format(round(sum(Rt[,1]*(popsize_per_id))/sum(popsize_per_id),2),nsmall=2)," [",
                                   format(round(sum(Rtmin[,1]*(popsize_per_id))/sum(popsize_per_id),2),nsmall=2),";",
                                   format(round(sum(Rtmax[,1]*(popsize_per_id))/sum(popsize_per_id),2),nsmall=2),"]",sep="")
  return(R0)
}