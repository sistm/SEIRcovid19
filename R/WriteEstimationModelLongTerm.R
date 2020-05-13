#' WriteEstimationModelLongTerm generic
#'
#' @param obj Object to set
#' @param ModeFilename Output Name
#' @param TimeSpecificEquation List of string defining time-dependant equation
#' @param ModelMathChunck List of string defining the mathematical system
#' @export
WriteEstimationModelLongTerm <- function(obj,ModeFilename, TimeSpecificEquation, ModelMathChunck,RegressorInfo)
{
  UseMethod("WriteEstimationModelLongTerm",obj)
}

#' @export
WriteEstimationModelLongTerm.default <- function(obj,  ModeFilename, TimeSpecificEquation, ModelMathChunck,RegressorInfo)
{
  print("No method implemented for this class")
  return(obj)
}

#' @describeIn WriteEstimationModelLongTerm Write estimation model long term for an object of class an object of class \code{OdeSystem}
#' @export
WriteEstimationModelLongTerm.OdeSystem <- function(ode, ModeFilename, TimeSpecificEquation, ModelMathChunck,RegressorInfo)
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
  
  regressor_line<-""
  if (length(RegressorInfo)>0){
    for (ireg in 1:length(RegressorInfo)){
      regressor_line<-paste(RegressorInfo[[ireg]]$name," = {use = regressor}",sep='')
      write(regressor_line, file=ModelFile,append=TRUE)
    }
  }
  
  # Eqaution
  write(paste("\nEQUATION:\n","odeType = stiff\n",sep=""), file=ModelFile,append=TRUE)
  # Etats initiaux
  init_line<-""
  init_regressor_optim<-names(ode$InitState)
  for (i in 1:length(init_regressor_optim)){
    variable_name<-substr(init_regressor_optim[i], nchar(init_regressor_optim[i]), nchar(init_regressor_optim[i]))
    init_line<-paste(init_line,variable_name,'_0=',names(ode$InitState[names(ode$InitState)==init_regressor_optim[i]]),'\n',sep='')
  }
  write(init_line,file=ModelFile,append=TRUE)
  
  # Time dependant equation
  write(paste(TimeSpecificEquation,collapse = "\n"),file=ModelFile,append=TRUE)
  WriteEmptyLine(ModelFile)
  # Math model chucnk
  write(paste(ModelMathBloc,collapse = "\n"),file=ModelFile,append=TRUE)
  
  ode$ModelFileEstimationLongTerm<-ModelFile
  return(ode)
}
