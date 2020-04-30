#' LaunchMonolix generic
#'
#' @param obj Object to set
#' @param ProjectName Output Name
#' @param ObservationType 
#' @param Mapping 
#' @export
LaunchMonolix <- function(obj, ProjectName, ObservationType, Mapping,runToBeDone=TRUE)
{
  UseMethod("LaunchMonolix",obj)
}
#' @describeIn default
LaunchMonolix.default <- function(obj,  ProjectName, ObservationType, Mapping)
{
  print("No method implemented for this class")
  return(obj)
}
#' @describeIn Launch monolix scenario for an object of class \code{OdeSystem}
#' @export
LaunchMonolix.OdeSystem <- function(ode, ProjectName, ObservationType, Mapping,runToBeDone=TRUE)
{
  # Initialize the connection
  lixoftConnectors::initializeLixoftConnectors(software="monolix")
  
  # Function to set Distribution and Parameter
  mlxProject.setIndividualParameterDistribution <- function(a) {
    eval.parent(parse(text =paste0('r <- lixoftConnectors::setIndividualParameterDistribution(',a,'= "normal")' )))
  }
  mlxProject.setIndividualParameterVariability <- function(a) {
    eval.parent(parse(text =paste0('r <- lixoftConnectors::setIndividualParameterVariability(',a,'= FALSE)' )))
  }
  
  # Create the project
  lixoftConnectors::newProject(modelFile = ode$ModelFile,
                               data = list(dataFile = ode$DataInfo$File,
                                           headerTypes =ode$DataInfo$HeaderType,
                                           observationTypes = ObservationType,
                                           mapping = Mapping))
  # Set the distrution and variability 
  parameter_with_no_random_effect<-(c(names(ode$parameter[ode$Variability$param==1]),names(ode$InitState[ode$Variability$init==1])))
  if (length(parameter_with_no_random_effect)>0){
    for (i in 1:length(parameter_with_no_random_effect)){
      #mlxProject.setIndividualParameterDistribution(var_name)
      mlxProject.setIndividualParameterVariability(parameter_with_no_random_effect[i])
    }
  }
  parameter_with_no_normal_dist<-(c(names(ode$parameter[ode$Distribution$param=="normal"]),names(ode$InitState[ode$Distribution$init=="normal"])))
  if (length(parameter_with_no_normal_dist)>0){
    for (i in 1:length(parameter_with_no_normal_dist)){
      #mlxProject.setIndividualParameterDistribution(var_name)
      mlxProject.setIndividualParameterDistribution(parameter_with_no_normal_dist[i])
    }
  }
  # Set all task to True (default for us)
  scenario <- lixoftConnectors::getScenario()
  for (itask in 1:length(scenario$tasks)){
    scenario$tasks[itask]<-TRUE
  }
  scenario$linearization<-FALSE
  lixoftConnectors::setScenario(scenario)
  #Save the project
  lixoftConnectors::saveProject(projectFile = paste(here::here(),'/MonolixFile/',ProjectName,".mlxtran",sep=""))
  if(runToBeDone){
    lixoftConnectors::runScenario()
    dir.create(paste(here::here(),'/MonolixFile/outputMonolix/',ProjectName,sep=""))
    dir.create(paste(here::here(),'/MonolixFile/outputMonolix/',ProjectName,'/IndividualParameters/',sep=""))
    # Indiv Param
    indiv<-lixoftConnectors::getEstimatedIndividualParameters()    
    indiv_mode<-indiv$conditionalMode
    indiv_sd<-indiv$conditionalSD
    new_names_mode<-rep("",length(names(indiv_mode)))
    new_names_sd<-rep("",length(names(indiv_mode)))
    for (i in 1:length(names(indiv_mode))){
      if (i ==1){
        new_names_mode[i]<-names(indiv_mode)[i]
        new_names_sd[i]<-names(indiv_sd)[i]
      }else{
        new_names_mode[i]<-paste(names(indiv_mode)[i],"_mode",sep="")
        new_names_sd[i]<-paste(names(indiv_sd)[i],"_sd",sep="")
      }
    }
    names(indiv_mode)<-new_names_mode
    names(indiv_sd)<-new_names_sd
    indiv_param<-cbind(indiv_mode,indiv_sd[,new_names_sd[2:length(new_names_sd)]])
    write.table(indiv_param,file=paste(here::here(),'/MonolixFile/outputMonolix/',ProjectName,'/IndividualParameters/',"estimatedIndividualParameters.txt",sep=""),sep=",")
    # Pop standard error
    pop<-lixoftConnectors::getEstimatedStandardErrors()
    write.table(pop,file=paste(here::here(),'/MonolixFile/outputMonolix/',ProjectName,'/',"populationParameters.txt",sep=""),sep=",")
    # Log Likehood
    likehood<-lixoftConnectors::getEstimatedLogLikelihood()
    write.table(likehood,file=paste(here::here(),'/MonolixFile/outputMonolix/',ProjectName,'/',"logLikelihood.txt",sep=""),sep=",")
    # Covariance Fisher info
    corr<-lixoftConnectors::getCorrelationOfEstimates()
    se<-lixoftConnectors::getEstimatedStandardErrors()
    dimension<-dim(corr$stochasticApproximation)
    covariance<-corr
    for (i in 1:dimension[1]){
      for (j in 1:dimension[2]){
        covariance$stochasticApproximation[i,j]<-corr$stochasticApproximation[i,j]*se$stochasticApproximation[i]*se$stochasticApproximation[j]
      }
    }
    write.table(covariance,file=paste(here::here(),'/MonolixFile/outputMonolix/',ProjectName,'/',"covarianceEstimatesSA.txt",sep=""),sep=",",col.names = FALSE)

  }
  return(ode)
}