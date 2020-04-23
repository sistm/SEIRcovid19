#' SetParamRandomEffect generic
#'
#' @param obj Object to set
#' @param Nrand List of parameter wtih Random Effect
#' @export
#' 

SetParamRandomEffect <- function(obj, Nrand)
{
  UseMethod("SetParamRandomEffect",obj)
}

SetParamRandomEffect.default <- function(obj, Nrand)
{
  print("No method implemented for this class")
  return(obj)
}

#' @describeIn Set parameter with random effect for an object of class \code{OdeSystem}
SetParamRandomEffect.OdeSystem <- function(ode, Nrand)
{
  exp<-"param_random <-list("
  for (i in 1:Nrand){
    if (i==Nrand){
      exp<-paste(exp,"beta",as.character(i),"=0)",sep="")
    }else{
      exp<-paste(exp,"beta",as.character(i),"=0,",sep="")
    }
  }
  #a<-expression(list(beta1=0,beta2=0))
  eval(parse(text=exp))
  ode$ParamRandomEffect<-param_random
  return(ode)
  
}