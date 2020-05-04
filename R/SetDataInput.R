#' SetDataInput generic
#'
#' @param obj Object to set
#' @param path path to the file
#' @param header header of the file
#' @param sep separator
#' @export
SetDataInput <- function(obj, path,header,sep)
{
  UseMethod("SetDataInput",obj)
}

#' @export
SetDataInput.default <- function(obj, path,header,sep)
{
  print("No method implemented for this class")
  return(obj)
}

#' @describeIn SetDataInput Set Data Input info of an object of class \code{OdeSystem}
#' @export
SetDataInput.OdeSystem <- function(ode, path,header,sep)
{
  ode$DataInfo$File<-path
  ode$DataInfo$HeaderType<-header
  ode$DataInfo$Sep<-sep
  return(ode)
}
