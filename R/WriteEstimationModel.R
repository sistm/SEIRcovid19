#' WriteEstimationModel generic
#'
#' @param obj Object to set
#' @param ModeFilename Output Name
#' @param TimeSpecificEquation List of string defining time-dependant equation
#' @param ModelMathChunck List of string defining the mathematical system
#' @export
WriteEstimationModel <- function(obj,ModeFilename, TimeSpecificEquation, ModelMathChunck,SpecificInitChunck)
{
  UseMethod("WriteEstimationModel",obj)
}

#' @export
WriteEstimationModel.default <- function(obj,  ModeFilename, TimeSpecificEquation, ModelMathChunck,SpecificInitChunck)
{
  print("No method implemented for this class")
  return(obj)
}

#' @describeIn WriteEstimationModel Write estimation model for an object of class an object of class \code{OdeSystem}
#' @export
WriteEstimationModel.OdeSystem <- function(ode, ModeFilename, TimeSpecificEquation, ModelMathChunck, SpecificInitChunck)
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
  regressor<-(paste(c(names(ode$InitState[ode$IsRegressor$init>0]),names(ode$parameter[ode$IsRegressor$param>0])),collapse=','))
  regressor<-unlist(strsplit(regressor, ","))
  if (length(regressor)>0){
    for (ireg in 1:length(regressor)){
      regressor_line<-paste(regressor[[ireg]]," = {use = regressor}",sep='')
      write(regressor_line, file=ModelFile,append=TRUE)
    }
  }

  # Eqaution
  write(paste("\nEQUATION:\n","odeType = stiff\n",sep=""), file=ModelFile,append=TRUE)
  # Etats initiaux
  init_line<-""
  # Non Specific
  init_regressor_optim<-names(ode$InitState[(ode$IsSpecificInit==0 ) ])
  for (i in 1:length(init_regressor_optim)){
    variable_name<-substr(init_regressor_optim[i], nchar(init_regressor_optim[i]), nchar(init_regressor_optim[i]))
    init_line<-paste(init_line,variable_name,'_0=',names(ode$InitState[names(ode$InitState)==init_regressor_optim[i]]),'\n',sep='')
  }
  write(init_line,file=ModelFile,append=TRUE)
  
  # Init specific Chunk
  
  write(paste(SpecificInitChunck,collapse = "\n"),file=ModelFile,append=TRUE)
  WriteEmptyLine(ModelFile)
  # Time dependant equation
  write(paste(TimeSpecificEquation,collapse = "\n"),file=ModelFile,append=TRUE)
  WriteEmptyLine(ModelFile)
  # Math model chucnk
  write(paste(ModelMathBloc,collapse = "\n"),file=ModelFile,append=TRUE)

  ode$ModelFileEstimation<-ModelFile
  return(ode)
}
