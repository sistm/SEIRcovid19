SetSomeParameter <- function(obj, parameter)
{
  UseMethod("SetSomeParameter",obj)
}

SetSomeParameter.default <- function(obj, parameter)
{
  print("No method implemented for this class")
  return(obj)
}


SetSomeParameter.OdeSystem <- function(ode, param)
{
  print("Set some parameter for OdeSytem")
  for (p in 1:length(param)){
    ode$parameter[names(ode$parameter)==names(param[p])]<-param[p]
  }
  return(ode)
}