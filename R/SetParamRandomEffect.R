#' SetParamRandomEffect generic not used any more
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
#' @export
SetParamRandomEffect.OdeSystem <- function(ode, Nrand)
{
  # If List => Set value
  if (is.list(Nrand)){
    for (p in 1:length(Nrand)){
      ode$ParamRandomEffect[names(ode$ParamRandomEffect)==names(Nrand[p])]<-Nrand[[p]]
    }
  }else { # If number => Init the list with name betai
    exp<-"param_random <-list("
    for (i in 1:Nrand){
      if (i==Nrand){
        exp<-paste(exp,"beta",as.character(i),"=0)",sep="")
      }else{
        exp<-paste(exp,"beta",as.character(i),"=0,",sep="")
      }
    }
    eval(parse(text=exp))
    ode$ParamRandomEffect<-param_random
  }

  return(ode)
  
}