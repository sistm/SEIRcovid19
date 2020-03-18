#' OLS Estimation of parameters in SIERAH models
#'
#' @import deSolve
#'
#' @examples
#' dataFR<-get_data_covid19(maille = "France",
#'              source = "Santé publique France")
#' optimresult<-optim(c(log(1.75),log(0.41)), ols_sierah)
#'
#'
#'
#' @export
ols_sierah <- function(b){
  transmission<-exp(b[1])
  ascertainment<-exp(b[2])
  print(transmission)
  print(ascertainment)
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
  #if(max(sol[,"time"]!=365)){
  #  objectiveFunction<-100
  #}else{
  #plot_sierah(sol,locator.legend=F)
  selectedsol<-sol[which(sol[,"time"]%in%data$day),4]
  ols_sierah<-sum((data[,"cas_confirmes_incident"]-selectedsol)**2)/length(data[,"cas_confirmes_incident"])
  print(ols_sierah)
}





# objectiveFunctionPLOT<-function(b){
#   transmission<-exp(b[1])
#   ascertainment<-exp(b[2])
#   print(transmission)
#   print(ascertainment)
#   alpha<-1
#   De<-5.2
#   Di<-2.3
#   Dq<-10
#   Dh<-30
#   popSize<-65000000
#   dailyMove<-1000000
#   E0<- data[1,"cas_confirmes_incident"]*2#Twice the number of cases (346 ref)
#   I0<-data[1,"cas_confirmes_incident"] #Numbers of cases (80 ref)
#   R0<- 0#(0 ref)
#   A0<-I0 # A=I (80 ref)
#   H0<-I0*0.50 #all at the begining H=50%I (27 ref)
#   S0<- popSize-E0-I0-A0-H0-R0 #N-E-I-A-H-R (9999467 ref)
#   init<-c(S0,E0,I0,R0,A0,H0)
#   t<-seq(0,365)
#   par<-c(transmission,ascertainment,alpha,De,Di,Dq,Dh,popSize,dailyMove)
#   sol <- deSolve::lsoda(init,t,seirah_ode,par)
#   #if(max(sol[,"time"]!=365)){
#   #  objectiveFunction<-100
#   #}else{
#   #plot_sierah(sol,locator.legend=F)
#   selectedsol<-sol[which(sol[,"time"]%in%data$day),4]
#   objectiveFunction<-sum((data[,"cas_confirmes_incident"]-selectedsol)**2)/length(data[,"cas_confirmes_incident"])
#   print(objectiveFunction)
#   #}
#   #if(comparison){
#     ylimmax<-max(rbind(selectedsol,data[,"cas_confirmes_incident"]))+1
#      plot(sol[which(sol[,"time"]%in%data$day),1],selectedsol,type="l",col="blue",ylim=c(0,ylimmax),ylab="Proportion")
#       points(data[,"day"],data[,"cas_confirmes_incident"],col="black")
#   # }
# }
# 
# 