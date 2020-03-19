#' Prediction of Epidemics and ICU utilization
#'
#' @import deSolve
#'
#' @examples
#' pred<-prediction_sierah(c(0.1912957, -0.5439555),data=dataFR,threesholdICU=5000)
#' print(pred)
#'
#'
#' @export
prediction_sierah <- function(b,data,threesholdICU,alpha=1,De=5.2,Di=2.3,Dq=10,Dh=30,popSize=65000000,dailyMove=1000000,log.print=TRUE,plot.comparison=FALSE){
  transmission<-exp(b[1])
  ascertainment<-exp(b[2])
  if(log.print)print(paste("transmission",transmission))
  if(log.print)print(paste("ascertainment",ascertainment))
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
  plot_sierah(sol)
  sol<-as.data.frame(sol)
  print("----------")
  Jpic<-sol[which(sol$`3`==max(sol$`3`)),"time"]
  print(paste("Jour Pic Epidemique",Jpic))
  Dpic<-as.Date(as.character(data[1,"date"]))+sol[which(sol$`3`==max(sol$`3`)),"time"]
  print(paste("Date Pic Epidemique",Dpic))
  print("----------")
  maxlit<-round(max(sol$`6`),0)
  print(paste("Max Lit hospitalisation",maxlit))
  Jmaxlit<-sol[which(sol$`6`==max(sol$`6`)),"time"]
  print(paste("Jour Max Lit hospitalisation",Jmaxlit))
  Dmaxlit<-as.Date(as.character(data[1,"date"]))+sol[which(sol$`6`==max(sol$`6`)),"time"]
  print(paste("Date Max Lit hospitalisation",Dmaxlit))  
  print("----------")
  Jdepasselit<-min(sol$time[which(sol$`6`>threesholdICU)])
  print(paste("Jour depasse capacite Lit hospitalisation",Jdepasselit))
  Ddepasselit<-as.Date(as.character(data[1,"date"]))+min(sol$time[which(sol$`6`>threesholdICU)])
  print(paste("Date depasse capacite Lit hospitalisation",Ddepasselit))
  print("----------")
  prediction_sierah<-as.data.frame(t(c(data$maille_nom[1],Jpic,Dpic,maxlit,Jmaxlit,Dmaxlit,Jdepasselit,Ddepasselit)))
  names(prediction_sierah)<-c("Location","Jpic","Dpic","maxlit","Jmaxlit","Dmaxlit","Jdepasselit","Ddepasselit")
  return(prediction_sierah)
}

