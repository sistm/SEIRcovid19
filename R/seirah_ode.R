#' SEIRAH derivatives functions
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
#' plot_sierah(sol)
#' @export
seirah_ode <- function(t,Y,par){
  S<-Y[1]
  E<-Y[2]
  I<-Y[3]
  R<-Y[4]
  A<-Y[5]
  H<-Y[6]


  b<-par[1]
  r<-par[2]
  alpha<-par[3]
  De<-par[4]
  Di<-par[5]
  Dq<-par[6]
  Dh<-par[7]
  popSize<-par[8]
  dailyMove<-par[9]

  dYdt<-vector(length=6)
  dYdt[1]=-b*S*(I+alpha*A)/popSize+dailyMove-dailyMove*S/(popSize-I-H)
  dYdt[2]=b*S*(I+alpha*A)/popSize-E/De-dailyMove*E/(popSize-I-H)
  dYdt[3]=r*E/De-I/Dq-I/Di
  dYdt[4]=(I+A)/Di+H/Dh-dailyMove*R/(popSize-I-H)
  dYdt[5]=(1-r)*E/De-A/Di-dailyMove*A/(popSize-I-H)
  dYdt[6]=I/Dq-H/Dh

  return(list(dYdt))
}

plot_sierah<-function(solution,locator.legend=F){
  ylimmax<-max(rbind(solution[,4],solution[,5],solution[,6],solution[,7]))+1
  plot(solution[,1],solution[,4],type="l",col="blue",ylim=c(0,ylimmax),ylab="Proportion")
  lines(solution[,1],solution[,5],col="green")
  lines(solution[,1],solution[,6],col="orange")
  lines(solution[,1],solution[,7],col="red")
  if(locator.legend){
    legend(locator(1), legend=c("Infectious","Not-reported","Hospitalized","Recovered"), col=c("blue","orange","red","green"), lty=1, cex=0.8)
  }else{
    legend(1, 0.9*ylimmax, legend=c("Infectious","Not-reported","Hospitalized","Recovered"), col=c("blue","orange","red","green"), lty=1, cex=0.8)
  }
}
