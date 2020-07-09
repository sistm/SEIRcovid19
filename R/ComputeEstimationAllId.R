ComputeEstimationAllId<-function(ode,ModeFilename,TimeSpecificEquation,SpecificInitBloc,ModelMathBloc,is_global,TimeDependantParameter){
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
  RegressorNames<-GetRegressorName(ode)
  
  
  EstimationReg<-c(names(ode$param[ode$IsRegressor$param>0]),names(ode$InitState[ode$IsRegressor$init>0]))
  
  # Loop over id
  for (index_id in 1:length(indivParams$id)){
    # Update the parameter and inist state thanks monolix optimisation
    ode_id[[index_id]]<-UpdateOdeSystem(ode_id[[index_id]],index_id,SpecificInitBloc)
    
    # Store the observation data
  
    ode_id[[index_id]]$ObsData<-data[which(data[,idname]==indivParams$id[index_id]),c(timename,ObservationName,ObsIdName,"date",RegressorNames)]
    
    # Get the regressor
    time<-seq(min(ode_id[[index_id]]$ObsData[,timename]),max(ode_id[[index_id]]$ObsData[,timename]),1)
    regressor_value<-list()
    for (ireg in 1:length(EstimationReg)){
      value<-rep(0,length(time))
      for (itime in 1:length(time)){
        value[itime] <- ode_id[[index_id]]$ObsData[which(ode_id[[index_id]]$ObsData[,timename]==time[itime]),EstimationReg[[ireg]]][1]
      }
      regressor_value[[ireg]]<-value
    }
    names(regressor_value)<-EstimationReg
    
    # Write monolix model for estimation
    ode_id[[index_id]]<-WriteEstimationModel(ode_id[[index_id]],ModeFilename,TimeSpecificEquation,ModelMathBloc,SpecificInitBloc)
    # Estimation for time value
    ode_id[[index_id]]<-Estimate(ode_id[[index_id]], time,is_global,regressor_value,TimeDependantParameter)
  }
  return(ode_id)
}