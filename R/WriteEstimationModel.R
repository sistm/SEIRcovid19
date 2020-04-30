#' WriteEstimationModel generic
#'
#' @param obj Object to set
#' @param ModeFilename Output Name
#' @param TimeSpecificEquation List of string defining time-dependant equation
#' @param ModelMathChunck List of string defining the mathematical system





WriteEstimationModel <- function(obj,ModeFilename, TimeSpecificEquation, ModelMathChunck)
{
  UseMethod("WriteEstimationModel",obj)
}
#' @describeIn default
WriteEstimationModel.default <- function(obj,  ModeFilename, TimeSpecificEquation, ModelMathChunck)
{
  print("No method implemented for this class")
  return(obj)
}

#' @describeIn Write estimation model for an object of class an object of class \code{OdeSystem}
#' @export
WriteEstimationModel.OdeSystem <- function(ode, ModeFilename, TimeSpecificEquation, ModelMathChunck)
{
  WriteEmptyLine<-function(ModelFile){
    write("\n",file=ModelFile,append=TRUE)
  }
  ModelFile<-paste(here::here(),'/MonolixFile/',ModeFilename,sep="")
  #Longitidunale
  write(paste("[LONGITUDINAL]","\n",sep=""), file=ModelFile)
  # Input => All parameter and InistState
  input_line<-"input = {"
  ## Etats initiaux et parametres optimisable
  mlx_input<-paste(c(names(ode$paramete),names(ode$InitState)),collapse=',')
  input_line<-paste(input_line,mlx_input,'}',sep='')
  write(input_line, file=ModelFile,append=TRUE)
  # Eqaution
  write(paste("\nEQUATION:\n","odeType = stiff\n",sep=""), file=ModelFile,append=TRUE)
  # Etats initiaux
  init_line<-""
  # Regressor
  init_name<-names(ode$InitState)
  for (i in 1:length(init_name)){
    variable_name<-substr(init_name[i], nchar(init_name[i]), nchar(init_name[i]))
    init_line<-paste(init_line,variable_name,'_0=',names(ode$InitState[names(ode$InitState)==init_name[i]]),'\n',sep='')
  }
  write(init_line,file=ModelFile,append=TRUE)
  
  # Time dependant equation
  write(paste(TimeSpecificEquation,collapse = "\n"),file=ModelFile,append=TRUE)
  WriteEmptyLine(ModelFile)
  # Math model chucnk
  write(paste(ModelMathBloc,collapse = "\n"),file=ModelFile,append=TRUE)
  
  ode$ModelFileEstimation<-ModelFile
  return(ode)
}