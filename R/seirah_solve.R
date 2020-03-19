#' SEIRAH solve function
#'
#' @importFrom deSolve lsoda
#'
#' @examples
#' # Set parameter values
#' b<-1
#' r<-1
#' alpha<-1
#' De<-5.2
#' Di<-2.3
#' Dq<-2
#' Dh<-30
#' popSize<-10000000
#' dailyMove<-0

#' init<-c(9999467, 346, 80, 0, 80, 27)
#' t<-seq(0,365)
#' par <- c(b, r, alpha, De, Di, Dq, Dh, popSize, dailyMove)
#'
#' # Solve system using lsoda
#' sol <- seirah_solve(init, t, par)
#'
#' # Plot solution
#' plot(sol)
#' @export
seirah_solve <- function(init, t, par){

  solution <- deSolve::lsoda(init, t, seirah_ode, par)

  class(solution) <-  "seirah_solve"
  colnames(solution) <- c("time", "S", "E", "I", "A", "H", "R")

  return(solution)
}
