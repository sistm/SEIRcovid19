#'@export

SetSomeInit <- function(obj, init)
{
  UseMethod("SetSomeInit",obj)
}

SetSomeInit.default <- function(obj, init)
{
  print("No method implemented for this class")
  return(obj)
}


SetSomeInit.OdeSystem <- function(ode, init)
{
  print("Set some Init for OdeSytem")
  for (p in 1:length(init)){
    print(init[p])
    ode$InitState[names(ode$InitState)==names(init[p])]<-init[[p]]
  }
  return(ode)
}