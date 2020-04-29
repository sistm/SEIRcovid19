ComputeEstimationAllId<-function(ode,time,ModeFilename,TimeSpecificEquation,SpecificInitBloc,ModelMathBloc,is_global){
  # Read individual parameter
  indivParams <-read.table(paste(here::here(),'/MonolixFile/',"/outputMonolix/",ode$nameproject,"/IndividualParameters/estimatedIndividualParameters.txt",sep=""),header=TRUE,sep=",")
  
  # Create a class for each id
  ode_id<-rep(list(ode),length(indivParams$id))
  
  # Read the dama
  data<-read.table(ode$DataInfo$File,sep=ode$DataInfo$Sep,header=TRUE)
  
  # Get the data header interessant name
  InputNames<-colnames(data)
  timename<-InputNames[ode$DataInfo$HeaderType=="time"]
  idname<-InputNames[ode$DataInfo$HeaderType=="id"]
  ObsIdName<-InputNames[ode$DataInfo$HeaderType=="obsid"]
  ObservationName<-InputNames[ode$DataInfo$HeaderType=="observation"]
  # Loop over id
  for (index_id in 1:length(indivParams$id)){
    # Update the parameter and inist state thanks monolix optimisation
    ode_id[[index_id]]<-UpdateOdeSystem(ode_id[[index_id]],index_id,SpecificInitBloc)
    # Write monolix model for estimation
    ode_id[[index_id]]<-WriteEstimationModel(ode_id[[index_id]],ModeFilename,TimeSpecificEquation,ModelMathBloc)
    # Estimation for time value
    ode_id[[index_id]]<-Estimate(ode_id[[index_id]], time,is_global)
    # Store the observation data
    ode_id[[index_id]]$ObsData<-data[which(data[,idname]==indivParams$id[index_id]),c(timename,ObservationName,ObsIdName)]
  }
  return(ode_id)
}