#' SetModelFile generic
#'
#' @param obj Object to set
#' @param path_file Number of observation
#' @export
#' 

SetModelFile <- function(obj, path_file)
{
  UseMethod("SetModelFile",obj)
}

SetModelFile.default <- function(obj, path_file)
{
  print("No method implemented for this class")
  return(obj)
}

#' @describeIn Set Path Model for an object of class \code{OdeSystem}
SetModelFile.OdeSystem <- function(ode, path_file)
{
  if (length(path_file)==1 & is.character(path_file)){
    ode$ModelFile<-path_file
    return(ode)
  }else{
    stop("Argument should be a string")
  }
  
}