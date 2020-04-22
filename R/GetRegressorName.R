#' GetRegressorName generic
#'
#' @param obj Object to set
#' @export





GetRegressorName <- function(obj)
{
  UseMethod("GetRegressorName",obj)
}
#' @describeIn default
GetRegressorName.default <- function(obj)
{
  print("No method implemented for this class")
  return(obj)
}

#' @describeIn Set Data Input info of an object of class an object of class \code{OdeSystem}
GetRegressorName.OdeSystem <- function(ode)
{
  Input<-read.csv2(ode$DataInfo$File,sep=ode$DataInfo$Sep)
  InputNames<-colnames(Input)
  RegressorNameOrdered<-InputNames[ode$DataInfo$HeaderType=="regressor"]
  return(RegressorNameOrdered)
}