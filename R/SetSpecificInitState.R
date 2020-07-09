#' SetSpecificInitState generic
#'
#' @param obj Object to set
#' @param SpecificInitBloc List
#' @export
#'

SetSpecificInitState <- function(obj, SpecificInitBloc)
{
  UseMethod("SetSpecificInitState",obj)
}

#' @export
SetSpecificInitState.default <- function(obj, SpecificInitBloc)
{
  print("No method implemented for this class")
  return(obj)
}

#' @describeIn SetSpecificInitState Set Some specific InitState for an object of class \code{OdeSystem} through exp
#' @export
SetSpecificInitState.OdeSystem<-function(ode,SpecificInitBloc){
  SetInitWithExp<-function(ode,exp,name){
    with(as.list(c(ode$InitState,ode$parameter)),{
      init<-(eval(parse(text=exp)))
      names(init)<-name
      ode$InitState[names(ode$InitState)==names(init)]<-init[1]
      return(ode)
    })
  }
  StateIdentifBloc<-"_0"
  PackageInitBloc<-SpecificInitBloc
  for (i in 1:length(SpecificInitBloc)){
    PackageInitBloc[i]<-""
    index<-pracma::strfind(SpecificInitBloc[i],StateIdentifBloc)
    state<-rep(0,length(index))
    for (j in 1:length(index)){
      state[j]<-substr(SpecificInitBloc[i],index[j]-1,index[j]-1)
      state[j]<-paste("init",state[j],sep="")
      if (j<length(index)){
        PackageInitBloc[i]<-paste(PackageInitBloc[i],state[j],substr(SpecificInitBloc[i],index[j]+1+length(StateIdentifBloc),index[j+1]-length(StateIdentifBloc)-1),sep="")
      }else{
        PackageInitBloc[i]<-paste(PackageInitBloc[i],state[j],substr(SpecificInitBloc[i],index[j]+1+length(StateIdentifBloc),nchar(SpecificInitBloc[i])),sep="")
      }
    }
    ode<-SetInitWithExp(ode,PackageInitBloc[i],state[1])
  }
  return(ode)
}
