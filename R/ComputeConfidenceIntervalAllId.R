ComputeConfidenceIntervalAllId<-function(ode_list,nb_mc,is_global){
  # Get the optimisation result
  
  indivParams <-read.table(paste(here::here(),'/MonolixFile/',"/outputMonolix/",ode_list[[1]]$nameproject,"/IndividualParameters/estimatedIndividualParameters.txt",sep=""),header=TRUE,sep=",")
  popParams<-read.table(paste(here::here(),'/MonolixFile/',"/outputMonolix/",ode_list[[1]]$nameproject,"/populationParameters.txt",sep=""),header=TRUE,sep=",")
  data<-read.table(ode_list[[1]]$DataInfo$File,sep=ode_list[[1]]$DataInfo$Sep,header=TRUE)
  InputNames<-colnames(data)
  timename<-InputNames[ode_list[[1]]$DataInfo$HeaderType=="time"]
  for (index_id in 1:length(indivParams$id)){
    # Compute CI for each ID and set result
    ode_list[[index_id]]<-ComputeConfidenceInterval(ode_list[[index_id]],indivParams,popParams,index_id,nb_mc,is_global,timename)
  }
  return (ode_list)
}