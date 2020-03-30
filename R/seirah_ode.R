#' SEIRAH derivative functions
#'
#' @keywords internal
#'
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
  timeconf<-par[10]
  lengthconf<-par[11]
  newdailyMove<-par[12]
  factorreductrans<-par[13]
  
  if((t>timeconf)&&(t<(timeconf+lengthconf))){
    dailyMove<-newdailyMove
    b<- b/factorreductrans
  }
  
  dYdt<-vector(length=6)
  dYdt[1]=-b*S*(I+alpha*A)/popSize+dailyMove-dailyMove*S/(popSize-I-H)
  dYdt[2]=b*S*(I+alpha*A)/popSize-E/De-dailyMove*E/(popSize-I-H)
  dYdt[3]=r*E/De-I/Dq-I/Di
  dYdt[4]=(I+A)/Di+H/Dh-dailyMove*R/(popSize-I-H)
  dYdt[5]=(1-r)*E/De-A/Di-dailyMove*A/(popSize-I-H)
  dYdt[6]=I/Dq-H/Dh
  
  # for (i in 1:6){
  #   if(init[i]+dYdt[i]<1)dYdt[i]=-init[i]
  # }
  res <- list(dYdt)
  
  return(res)
}
