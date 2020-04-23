#' Ode Class definition
#'
#' @export

OdeSystem <- function(func,param,init,modname=c("S","E","I","R","A","H"),
                      variabilty=list(),
                      isregressor=list(),
                      distribution=list(),
                      Data=list(),
                      isspecificinit=rep(0,length(init)))
{
  # Set all variability info to 0
  if (length(variabilty)==0){
    variabilty$param<-rep(0,length(param))
    variabilty$init<-rep(0,length(init))
  }
  # Set all regressor info to 0
  if (length(isregressor)==0){
    isregressor$param<-rep(0,length(param))
    isregressor$init<-rep(0,length(init))
  }
  # Set all distribution to logNormal (default)
  if (length(distribution)==0){
    distribution$param<-rep("logNormal",length(param))
    distribution$init<-rep("logNormal",length(init))
  }
  # Struct of the data Input
  Data$File=""
  Data$HeaderType=""
  Data$Sep=""
  ModelFile=""
  
  # Class
  ode <- list(
    ode_def=func,
    ModelFile=ModelFile,
    NumberObservation=0,
    parameter = param,
    InitState = init,
    ncomp= length(modname),
    Variability= variabilty,
    ModelName= modname,
    IsRegressor=isregressor,
    Distribution=distribution,
    DataInfo=Data,
    IsSpecificInit=isspecificinit, 
    ParamRandomEffect=list()
    )
  
  class(ode) <- append(class(ode),"OdeSystem")
  return(ode)
}