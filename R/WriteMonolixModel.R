#' WriteMonolixModel generic
#'
#' @param obj Object to set
#' @param ModelFile List of new boolean
#' @export





WriteMonolixModel <- function(obj, ModelFile)
{
  UseMethod("WriteMonolixModel",obj)
}
#' @describeIn default
WriteMonolixModel.default <- function(obj, ModelFile)
{
  print("No method implemented for this class")
  return(obj)
}

#' @describeIn Set Observable info for state of an object of class an object of class \code{OdeSystem}
WriteMonolixModel.OdeSystem <- function(ode, ModelFile)
{
  #Description
  write(paste("DESCRIPTION: FIT ",paste(ode$ModelName, collapse = '')," MODEL","\n","\n",sep=""),file=ModelFile)
  #Longitidunale
  write(paste("[LONGITUDINAL]","\n","INPUT:",sep=""), file=ModelFile,append=TRUE)
  # Parameter : optimiser par monolix
  param_line<-"parameter = {"
  ## Etats initiaux et parametres optimisable
  mlx_param<-paste(c(names(ode$InitState[ode$IsOptimizable$init==1]),names(ode$parameter[ode$IsOptimizable$param==1])),collapse=',')
  ## Ceux avec une distribution
  number_parameter_with_distribution<-length(names(ode$parameter[ode$IsOptimizable$param==2]))
  if (number_parameter_with_distribution > 0){
    param_line<-paste(param_line,mlx_param,',',sep='')
    for (i in 1:number_parameter_with_distribution){
      if (i==number_parameter_with_distribution){
        param_line<-paste(param_line,"b",as.character(i),',',"beta",as.character(i),'}',sep='')
      }else{
        param_line<-paste(param_line,"b",as.character(i),',',"beta",as.character(i),',',sep='')
      }
    }
  }else{
    param_line<-paste(param_line,mlx_param,'}',sep='')
  }
  
  write(param_line, file=ModelFile,append=TRUE)
  # Regressor : vu dans les données
  regressor_line<-"regressor = {"
  #Etats initiaux et parametre
  mlx_regressor<-paste(c(names(ode$InitState[ode$IsRegressor$init==1]),names(ode$parameter[ode$IsRegressor$param==1])),collapse=',')
  regressor_line<-paste(regressor_line,mlx_regressor,'}',sep='')
  write(regressor_line,file=ModelFile,append=TRUE)
  
  # Eqaution
  write(paste("\nEQUATION:\n","odeType = stiff\n","t0 = 0  ; t0 is a reserved keyword (initiation of therapy) \n",sep=""), file=ModelFile,append=TRUE)
  
  # Parametres constants
  scalar_parameter<-names(ode$parameter[(ode$IsRegressor$param==0 )& (ode$IsOptimizable$param==0)])
  scalar_line<-""
  for (i in 1:length(scalar_parameter)){
    scalar_line<-paste(scalar_line,scalar_parameter[i],'=',as.character( ode$parameter[names(ode$parameter)==scalar_parameter[i]]),'\n',sep='')
  }
  write(scalar_line,file=ModelFile,append=TRUE)
  # Etats initiaux
  init_line<-""
  # Regressor
  init_regressor_optim<-names(ode$InitState[(ode$IsRegressor$init==1 ) | (ode$IsOptimizable$init==1 ) ])
  for (i in 1:length(init_regressor_optim)){
    variable_name<-substr(init_regressor_optim[i], nchar(init_regressor_optim[i]), nchar(init_regressor_optim[i]))
    init_line<-paste(init_line,variable_name,'_0=',names(ode$InitState[names(ode$InitState)==init_regressor_optim[i]]),'\n',sep='')
  }
  # Autres
  init_scalar<-names(ode$InitState[(ode$IsRegressor$init==0 ) & (ode$IsOptimizable$init==0 ) ])
  for (i in 1:length(init_scalar)){
    variable_name<-substr(init_scalar[i], nchar(init_scalar[i]), nchar(init_scalar[i]))
    init_line<-paste(init_line,variable_name,'_0=','\n',sep='')
  }
  write(init_line,file=ModelFile,append=TRUE)
  
  #Distribution parameter
  number_parameter_with_distribution<-length(names(ode$parameter[ode$IsOptimizable$param==2]))
  parameter_distribution<-""
  if (number_parameter_with_distribution > 0){
    for (i in 1:number_parameter_with_distribution){
      parameter_distribution<-paste(parameter_distribution,names(ode$parameter[ode$IsOptimizable$param==2])[i],'=b',as.character(i),"*exp(beta",as.character(i),')\n',sep='')
    }
  }
  write(parameter_distribution,file=ModelFile,append=TRUE)
  #Observation
  observable_line<-""
  def_line<-""
  output_line<-"output = {"
  state_observable<-names(ode$InitState[(ode$IsStateObservable==1 )])
  for (i in 1:length(state_observable)){
    variable_name<-substr(state_observable[i], nchar(state_observable[i]), nchar(state_observable[i]))
    observable_line<-paste(observable_line,variable_name,'sim=','\n',sep='')
    def_line<-paste(def_line,"Y",as.character(i),"= {type = count, log(P(Y",as.character(i),"=k)) = -",variable_name,"sim+ k*log(",variable_name,"sim) - factln(k) }\n",sep="")
    if  (i==length(state_observable)){
      output_line<-paste(output_line,"Y",as.character(i),"}",sep="")
    }else{
      output_line<-paste(output_line,"Y",as.character(i),", ",sep="")
    }
    
  }
  write(observable_line,file=ModelFile,append=TRUE)
  
  write("\nDEFINITION:\n",file=ModelFile,append=TRUE)
  write(def_line,file=ModelFile,append=TRUE)
  
  write("\nOUTPUT:\n")
  write(output_line,file=ModelFile,append=TRUE)
  
  return(ode)
}