#' OLS Estimation of parameters in SIERAH models
#'
#' @import deSolve
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

#' init<-c(9999467,346,80,0,80,27)
#' t<-seq(0,365)
#' par<-c(b,r,alpha,De,Di,Dq,Dh,popSize,dailyMove)
#' # Solve system using lsoda
#' sol <- deSolve::lsoda(init,t,seirah_ode,par)

#' # Plot solution
#' plot(t,sol[,3],type="l",col="blue",ylim=c(0,100000),ylab="Proportion")
#' lines(t,sol[,4],col="green")
#' lines(t,sol[,5],col="orange")
#' lines(t,sol[,6],col="red")
#' legend(1, 90000, legend=c("Infectious","Not-reported","Hospitalized","Recovered"),
#'        col=c("blue","orange","red","green"), lty=1, cex=0.8)
#'
#' @export
ols_sierah <- function(data){

  

}
