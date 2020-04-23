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

#' @describeIn Get the regressor name  of an object of class \code{OdeSystem} based on the Headernames
GetRegressorName.OdeSystem <- function(ode)
{
  #Read Input Data
  Input<-read.csv2(ode$DataInfo$File,sep=ode$DataInfo$Sep)
  InputNames<-colnames(Input)
  #Find header define as regressor for monolix
  RegressorNameOrdered<-InputNames[ode$DataInfo$HeaderType=="regressor"]
  return(RegressorNameOrdered)
}