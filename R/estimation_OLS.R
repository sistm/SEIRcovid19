#' OLS Estimation of parameters in SIERAH models
#'
#' @import deSolve
#'
#' @examples
#' 

#'                 
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


#data<-get_data_covid19(maille = "France",
#                 source = "Santé publique France")
objectiveFunction<-function(transmission,ascertainment){
  alpha<-1
  De<-5.2
  Di<-2.3
  Dq<-10
  Dh<-30
  popSize<-65000000
  dailyMove<-1000000
  E0<- data[1,"cas_confirmes_incident"]*2#Twice the number of cases (346 ref)
  I0<-data[1,"cas_confirmes_incident"] #Numbers of cases (80 ref)
  R0<- 0#(0 ref)
  A0<-I0 # A=I (80 ref)
  H0<-I0*0.50 #all at the begining H=50%I (27 ref)
  S0<- popSize-E0-I0-A0-H0-R0 #N-E-I-A-H-R (9999467 ref)
  init<-c(S0,E0,I0,R0,A0,H0)
  t<-seq(0,365)
  par<-c(transmission,ascertainment,alpha,De,Di,Dq,Dh,popSize,dailyMove)
  sol <- deSolve::lsoda(init,t,seirah_ode,par)
  plot_sierah(sol,locator.legend=F)
  selectedsol<-sol[which(sol[,"time"]%in%data$day),4]
  objectiveFunction<-sum((data[,"cas_confirmes_incident"]-selectedsol)**2)/length(data[,"cas_confirmes_incident"])
}


#optim(c(1.75,0.41), objectiveFunction, NULL, method = "BFGS", hessian = TRUE)


