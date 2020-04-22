#' Sum of vector elements.
#'
#' @export

OdeSystem <- function(func,param,init,modname=c("S","E","I","R","A","H"),
                      variabilty=list(),
                      isregressor=list(),
                      distribution=list(),
                      Data=list(),
                      isspecificinit=rep(0,length(init)))
{
  if (length(variabilty)==0){
    variabilty$param<-rep(0,length(param))
    variabilty$init<-rep(0,length(init))
  }
  if (length(isregressor)==0){
    isregressor$param<-rep(0,length(param))
    isregressor$init<-rep(0,length(init))
  }
  if (length(distribution)==0){
    distribution$param<-rep("logNormal",length(param))
    distribution$init<-rep("logNormal",length(init))
  }
  Data$File=""
  Data$HeaderType=""
  Data$Sep=""
  ode <- list(
    ode_def=func,
    Numberobservation=0,
    parameter = param,
    InitState = init,
    ncomp= length(modname),
    Variability= variabilty,
    ModelName= modname,
    IsRegressor=isregressor,
    Distribution=distribution,
    DataInfo=Data,
    IsSpecificInit=isspecificinit
    )
  
  class(ode) <- append(class(ode),"OdeSystem")
  return(ode)
}