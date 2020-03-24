#' Expit function
#'
#' @examples
#' logit(expit(10))
#'
#' @export

logit <- function(p){
  log(p/(1-p))
}


#' Expit function
#'
#' @examples expit(logit(0.1))
#'
#' @export
expit <- function(x){
  exp(x)/(1+exp(x))
}
