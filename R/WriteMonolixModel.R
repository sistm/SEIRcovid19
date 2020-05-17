#' WriteMonolixModel generic
#'
#' @param obj Object to set
#' @param ModelFile Output Name
#' @param SpecificInitChunck List of string defining special init formula
#' @param ModelStatChunck List of string defining the statistic system
#' @param ModelMathChunck List of string defining the mathematical system
#' @param ModelObservationChunck List of string defining the observation system
#' @export
WriteMonolixModel <- function(obj, ModelFile, SpecificInitChunck, ModelStatChunck, ModelMathChunck, ModelObservationChunck)
{
  UseMethod("WriteMonolixModel",obj)
}

#' @export
WriteMonolixModel.default <- function(obj,  ModelFile, SpecificInitChunck, ModelStatChunck, ModelMathChunck, ModelObservationChunck)
{
  print("No method implemented for this class")
  return(obj)
}

#' @describeIn WriteMonolixModel Write optimisation model of an object of class an object of class \code{OdeSystem}
#' @export
WriteMonolixModel.OdeSystem <- function(ode, ModelFile, SpecificInitChunck, ModelStatChunck, ModelMathChunck, ModelObservationChunck)
{
  WriteEmptyLine<-function(ModelFile){
    write("\n",file=ModelFile,append=TRUE)

  }
  #Description
  write(paste("DESCRIPTION: FIT ",paste(ode$ModelName, collapse = '')," MODEL","\n","\n",sep=""),file=ModelFile)
  #Longitidunale
  write(paste("[LONGITUDINAL]","\n","INPUT:",sep=""), file=ModelFile,append=TRUE)
  # Parameter : optimiser par monolix
  param_line<-"parameter = {"
  ## Etats initiaux et parametres optimisable
  mlx_param<-paste(c(names(ode$InitState[ode$Variability$init>0]),names(ode$parameter[ode$Variability$param>0])),collapse=',')
  param_line<-paste(param_line,mlx_param,'}',sep='')
  ## Ceux avec une distribution
  write(param_line, file=ModelFile,append=TRUE)


  #Regressor
  regressor_line<-"regressor = {"
  RegressorName<-GetRegressorName(ode)
  regressor_line<-paste(regressor_line,paste(RegressorName,collapse = ','),'}',sep='')
  write(regressor_line,file=ModelFile,append=TRUE)

  # Eqaution
  write(paste("\nEQUATION:\n","odeType = stiff\n","t0 = 0  ; t0 is a reserved keyword (initiation of therapy) \n",sep=""), file=ModelFile,append=TRUE)

  # Parametres constants
  scalar_parameter<-names(ode$parameter[(ode$Variability$param==0 & ode$IsRegressor$param==0)])
  scalar_line<-""
  for (i in 1:length(scalar_parameter)){
    scalar_line<-paste(scalar_line,scalar_parameter[i],'=',as.character( ode$parameter[names(ode$parameter)==scalar_parameter[i]]),'\n',sep='')
  }
  write(scalar_line,file=ModelFile,append=TRUE)


  # Etats initiaux
  init_line<-""
  # Regressor
  init_regressor_optim<-names(ode$InitState[(ode$IsRegressor$init==1 ) ])
  if (length(init_regressor_optim)>0){
    for (i in 1:length(init_regressor_optim)){
      variable_name<-substr(init_regressor_optim[i], nchar(init_regressor_optim[i]), nchar(init_regressor_optim[i]))
      init_line<-paste(init_line,variable_name,'_0=',names(ode$InitState[names(ode$InitState)==init_regressor_optim[i]]),'\n',sep='')
    }
    write(init_line,file=ModelFile,append=TRUE)
  }


  # Init monolix parameter
  init_parameter<-names(ode$InitState[ode$Variability$init>0])
  init_line<-""
  if (length(init_parameter)>0){
  for (i in 1:length(init_parameter)){
    variable_name<-substr(init_parameter[i], nchar(init_parameter[i]), nchar(init_parameter[i]))
    init_line<-paste(init_line,variable_name,'_0=',names(ode$InitState[names(ode$InitState)==init_parameter[i]]),'\n',sep='')
  }
  write(init_line,file=ModelFile,append=TRUE)
  }
  # Non Specific
  init_scalar<-names(ode$InitState[(ode$IsRegressor$init==0 ) & (ode$IsSpecificInit==0 ) & (ode$Variability$init==0 )])
  init_line<-""
  if (length(init_scalar)>0){
  for (i in 1:length(init_scalar)){
    variable_name<-substr(init_scalar[i], nchar(init_scalar[i]), nchar(init_scalar[i]))
    init_line<-paste(init_line,variable_name,'_0=',ode$InitState[names(ode$InitState)==init_scalar[i]],'\n',sep='')
  }
  write(init_line,file=ModelFile,append=TRUE)
  }
  # Init specific Chunk

  write(paste(SpecificInitChunck,collapse = "\n"),file=ModelFile,append=TRUE)
  WriteEmptyLine(ModelFile)
  #Model Stats
  write(paste(ModelStatChunck,collapse = "\n"),file=ModelFile,append=TRUE)
  WriteEmptyLine(ModelFile)
  # Math model chucnk
  write(paste(ModelMathBloc,collapse = "\n"),file=ModelFile,append=TRUE)
  WriteEmptyLine(ModelFile)

  #Observation chucnk
  write(paste(ModelObservationChunck,collapse = "\n"),file=ModelFile,append=TRUE)
  WriteEmptyLine(ModelFile)

  ## TO do remplacer par un set
  ode<-SetNumberObservation(ode,length(ModelObservationChunck))

  CutObservation<-strsplit(ModelObservationChunck,'=')
  def_line<-""
  output_line<-"output = {"
  for (i in 1:length(ModelObservationChunck)){

    variable_name<-CutObservation[[i]][1]
    def_line<-paste(def_line,"Y",as.character(i),"= {type = count, log(P(Y",as.character(i),"=k)) = -",variable_name,"+ k*log(",variable_name,") - factln(k) }\n",sep="")
    if  (i==length(ModelObservationChunck)){
      output_line<-paste(output_line,"Y",as.character(i),"}",sep="")
    }else{
      output_line<-paste(output_line,"Y",as.character(i),", ",sep="")
    }

  }

  write("\nDEFINITION:\n",file=ModelFile,append=TRUE)
  write(def_line,file=ModelFile,append=TRUE)

  write("\nOUTPUT:\n",file=ModelFile,append=TRUE)
  write(output_line,file=ModelFile,append=TRUE)

  ode<-SetModelFile(ode,ModelFile)
  #ode$ModelFile<-ModelFile
  return(ode)
}
