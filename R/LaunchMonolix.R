#' LaunchMonolix generic
#'
#' @param obj Object to set
#' @param ProjectName Output Name
#' @param ObservationType Observation type
#' @param Mapping mapping
#' @export
LaunchMonolix <- function(obj, ProjectName, ObservationType, Mapping,runToBeDone=TRUE,prior_mean=list(),prior_std=list(),PopInitValue=list())
{
  UseMethod("LaunchMonolix",obj)
}

#' @export
LaunchMonolix.default <- function(obj,  ProjectName, ObservationType, Mapping,runToBeDone=TRUE,prior_mean=list(),prior_std=list(),PopInitValue=list())
{
  print("No method implemented for this class")
  return(obj)
}
#' @describeIn LaunchMonolix Launch monolix scenario for an object of class \code{OdeSystem}
#' @export
LaunchMonolix.OdeSystem <- function(ode, ProjectName, ObservationType, Mapping,runToBeDone=TRUE,prior_mean=list(),prior_std=list(),PopInitValue=list())
{
  # Initialize the connection
  lixoftConnectors::initializeLixoftConnectors(software="monolix")

  # Function to set Distribution and Parameter
  mlxProject.setIndividualParameterDistribution <- function(a,b) {
    eval.parent(parse(text =paste0('r <- lixoftConnectors::setIndividualParameterDistribution(',a,'= "',b,'")' )))
  }
  mlxProject.setIndividualParameterVariability <- function(a) {
    eval.parent(parse(text =paste0('r <- lixoftConnectors::setIndividualParameterVariability(',a,'= FALSE)' )))
  }
  mlxProject.setPopulationParameterInitValue <- function(a,b) {
    eval.parent(parse(text =paste0('r <- lixoftConnectors::setPopulationParameterInformation(',a,'_pop= list(initialValue =',b,'))' )))
  }
  mlxProject.setPopulationParameterPriorInfo <- function(name,value_mean,value_sd) {
    eval.parent(parse(text =paste0('r <- lixoftConnectors::setPopulationParameterInformation(',name,'_pop= list( method = "MAP" , priorValue =',value_mean,
                                   ', priorSD = ',value_sd,') )' )))
  }
  # Create the project
  lixoftConnectors::newProject(modelFile = ode$ModelFile,
                               data = list(dataFile = ode$DataInfo$File,
                                           headerTypes =ode$DataInfo$HeaderType,
                                           observationTypes = ObservationType,
                                           mapping = Mapping))
  # Set initial value of the optimizable parameter
  # first take the value of the system
  parameter_with_normal_dist<-(c(names(ode$parameter[ode$Distribution$param=="normal"]),names(ode$InitState[ode$Distribution$ini=="normal"])))
  if (length(parameter_with_normal_dist)>0){
    dist<-(c(ode$Distribution$param[ode$Distribution$param=="normal"],(ode$Distribution$init[ode$Distribution$init=="normal"])))
    for (i in 1:length(parameter_with_normal_dist)){
      #mlxProject.setIndividualParameterDistribution(var_name)
      mlxProject.setIndividualParameterDistribution(parameter_with_normal_dist[i],dist[i])
    }
  }
  optimizable_param<-c(names(ode$InitState[ode$Variability$init>0]),names(ode$parameter[ode$Variability$param>0]))
  for (i in 1:length(optimizable_param)){
    init_value<-c(ode$InitState[names(ode$InitState)==optimizable_param[i]],ode$parameter[names(ode$parameter)==optimizable_param[i]])
    mlxProject.setPopulationParameterInitValue(optimizable_param[[i]],init_value[[1]])
  }
  # Now set with the value specified by the user
  if (length(PopInitValue)>0){
    PopName<-names(PopInitValue)
    for (i in 1:length(PopInitValue)){
      mlxProject.setPopulationParameterInitValue(PopName[[i]],PopInitValue[[i]])
    }
  }
  parameter_with_logitnormal_dist<-(c(names(ode$parameter[ode$Distribution$param=="logitNormal"]),names(ode$InitState[ode$Distribution$ini=="logitNormal"])))
  if (length(parameter_with_logitnormal_dist)>0){
    dist<-(c(ode$Distribution$param[ode$Distribution$param=="logitNormal"],(ode$Distribution$init[ode$Distribution$init=="logitNormal"])))
    for (i in 1:length(parameter_with_logitnormal_dist)){
      #mlxProject.setIndividualParameterDistribution(var_name)
      mlxProject.setIndividualParameterDistribution(parameter_with_logitnormal_dist[i],dist[i])
    }
  }
  # Set the prior, mean, std, and method
  if (length(prior_mean)>0){
    prior_param_name<-names(prior_mean)
    for (i in 1:length(prior_mean)){
      mlxProject.setPopulationParameterPriorInfo(prior_param_name[[i]],prior_mean[[i]],prior_std[[i]])
    }
  }
  # Set the distrution and variability
  parameter_with_no_random_effect<-(c(names(ode$parameter[ode$Variability$param==1]),names(ode$InitState[ode$Variability$init==1])))
  if (length(parameter_with_no_random_effect)>0){
    for (i in 1:length(parameter_with_no_random_effect)){
      #mlxProject.setIndividualParameterDistribution(var_name)
      mlxProject.setIndividualParameterVariability(parameter_with_no_random_effect[i])
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
  info = lixoftConnectors::getPopulationParameterInformation()
  info
  indivModel = lixoftConnectors::getIndividualParameterModel()
  
  indivModel$variability
  lixoftConnectors::saveProject(projectFile = paste(here::here(),'/MonolixFile/',ProjectName,".mlxtran",sep=""))
  if(runToBeDone){
    lixoftConnectors::runScenario()
    dir.create(paste(here::here(),'/MonolixFile/outputMonolix/',sep=""))
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
