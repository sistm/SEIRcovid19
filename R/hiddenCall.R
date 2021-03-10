#' .hiddenCall
#'
#' @description Parse a text as an R command. Useful for LixoftConnector package.
#'
#' @param command A text string.
#'
#' @return The result of the text string command
#'
#' @examples
#' 
#' a <- 6
#' .hiddenCall(command = "print(a)")
#' 
.hiddenCall <- function(command){
  
  eval.parent(parse(text = command))
  
}
