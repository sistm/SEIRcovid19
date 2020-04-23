#' LaunchMonolix generic
#'
#' @param obj Object to set
#' @param ProjectName Output Name
#' @param ObservationType 
#' @param Mapping 
#' @export
LaunchMonolix <- function(obj, ProjectName, ObservationType, Mapping)
{
  UseMethod("LaunchMonolix",obj)
}
#' @describeIn default
LaunchMonolix.default <- function(obj,  ProjectName, ObservationType, Mapping)
{
  print("No method implemented for this class")
  return(obj)
}
#' @export
#' @describeIn Launch monolix scenario for an object of class \code{OdeSystem}
LaunchMonolix.OdeSystem <- function(ode, ProjectName, ObservationType, Mapping)
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
  number_parameter_with_random_effect<-length(c(names(ode$parameter[ode$Variability$param==2]),names(ode$InitState[ode$Variability$init==2])))
  if (length(number_parameter_with_random_effect)>0){
    for (i in 1:number_parameter_with_random_effect){
      var_name<-paste("beta",as.character(i),sep="")
      mlxProject.setIndividualParameterDistribution(var_name)
      mlxProject.setIndividualParameterVariability(var_name)
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
  #lixoftConnectors::runScenario()
  return(ode)
}