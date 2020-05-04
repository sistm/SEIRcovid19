#' UpdateOdeSystem generic
#'
#' @param index_id Id of the region
#' @param SpecificInitBloc Specific init to set, also write in monolix model
#' @export
UpdateOdeSystem <- function(obj ,index_id,SpecificInitBloc)
{
  UseMethod("UpdateOdeSystem",obj)
}

#' @export
UpdateOdeSystem.default <- function(obj, index_id,SpecificInitBloc)
{
  print("No method implemented for this class")
  return(obj)
}

#' @describeIn UpdateOdeSystem Set Path Model for an object of class \code{OdeSystem}
#' @export
UpdateOdeSystem.OdeSystem <-function(ode,index_id=1,SpecificInitBloc){
  # Read Optimisation result from Monolix => Used in order to update optimizable paramter/initState
  indivParams <-read.table(paste(here::here(),'/MonolixFile/',"/outputMonolix/",ode$nameproject,"/IndividualParameters/estimatedIndividualParameters.txt",sep=""),header=TRUE,sep=",")
  # Read the input data => Used in order to update regressor value
  data<-read.table(ode$DataInfo$File,sep=ode$DataInfo$Sep,header=TRUE)
  InputNames<-colnames(data)
  timename<-InputNames[ode$DataInfo$HeaderType=="time"]
  idname<-InputNames[ode$DataInfo$HeaderType=="id"]

  # Set the regressor value thanks to data input
  RegressorName<-GetRegressorName(ode)
  RegressorValue<-as.list(rep(NA,length(RegressorName)))
  names(RegressorValue)<-(RegressorName)
  for (j in 1:length(RegressorName)){
    RegressorValue[j]<-data[which(data[idname]==as.character(indivParams$id[index_id]) & (data[timename]==0)) ,RegressorName[j]][1]
  }
  # Uptade the class
  ode<-SetSomeInit(ode,RegressorValue)
  ode<-SetSomeParameter(ode,RegressorValue)

  # Set the parameter name thanks to monolix
  optimize_param_name<-c(names(ode$parameter[ode$Variability$param>0]),names(ode$InitState[ode$Variability$init>0]))
  OptimizeParam<-as.list(rep(NA,length(optimize_param_name)))
  names(OptimizeParam)<-optimize_param_name
  for (j in 1:length(optimize_param_name)){
    optimize_monolix_name<-paste(optimize_param_name[j],"_mode",sep="")
    OptimizeParam[j]<-indivParams[index_id,optimize_monolix_name]
  }
  # Update the class
  ode<-SetSomeParameter(ode,OptimizeParam)
  ode<-SetSomeInit(ode,OptimizeParam)
  ode<-SetSpecificInitState(ode,SpecificInitBloc)
  return(ode)
}
