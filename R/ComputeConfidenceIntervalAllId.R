ComputeConfidenceIntervalAllId<-function(ode_list,time,nb_mc,is_global,regressor_value){
  # Get the optimisation result
  
  indivParams <-read.table(paste(here::here(),'/MonolixFile/',"/outputMonolix/",ode_list[[1]]$nameproject,"/IndividualParameters/estimatedIndividualParameters.txt",sep=""),header=TRUE,sep=",")
  popParams<-read.table(paste(here::here(),'/MonolixFile/',"/outputMonolix/",ode_list[[1]]$nameproject,"/populationParameters.txt",sep=""),header=TRUE,sep=",")
  
  for (index_id in 1:length(indivParams$id)){
    # Compute CI for each ID and set result
    ode_list[[index_id]]<-ComputeConfidenceInterval(ode_list[[index_id]],indivParams,popParams,index_id,time,nb_mc,is_global,regressor_value)
  }
  return (ode_list)
}