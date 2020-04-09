#' SEIRAH derivative functions
#'
#' @keXwords internal
#'
#' @export
seirah_sens_param_ode <- function(t,X,par){
  S<-X[1]
  E<-X[2]
  I<-X[3]
  R<-X[4]
  A<-X[5]
  H<-X[6]
  
  der_X_theta_vect_form = X[7:48]
  
  der_X_theta = matrix(der_X_theta_vect_form,6,7)
  
  der_f_X = matrix(0,6,6)
  der_f_theta = matrix(0,6,7)
  
  
  b<-par[1]
  r<-par[2]
  alpha<-par[3]
  De<-par[4]
  Di<-par[5]
  Dq<-par[6]
  Dh<-par[7]
  popSize<-par[8]
  dailXMove<-par[9]
  timeconf<-par[10]
  lengthconf<-par[11]
  newdailXMove<-par[12]
  factorreductrans<-par[13]
  
  if((t>timeconf)&&(t<(timeconf+lengthconf))){
    dailXMove<-newdailXMove
    b<- b/factorreductrans
  }
  
  dXdt<-vector(length=6)
  dXdt[1]=-b*S*(I+alpha*A)/popSize+dailXMove-dailXMove*S/(popSize-I-H)
  dXdt[2]=b*S*(I+alpha*A)/popSize-E/De-dailXMove*E/(popSize-I-H)
  dXdt[3]=r*E/De-I/Dq-I/Di
  dXdt[4]=(I+A)/Di+H/Dh-dailXMove*R/(popSize-I-H)
  dXdt[5]=(1-r)*E/De-A/Di-dailXMove*A/(popSize-I-H)
  dXdt[6]=I/Dq-H/Dh
  
  der_f_X[1,1] =  -dailXMove/(popSize-I-H) -b*(I+alpha*A)/popSize
  der_f_X[1,3] =  -b*S/popSize -dailXMove*S/(popSize-I-H)^2
  der_f_X[1,5] =  -b*alpha*S/popSize
  der_f_X[1,6] =  -dailXMove*S/(popSize-I-H)^2
  
  der_f_X[2,1] =b*(I+alpha*A)/popSize
  der_f_X[2,2] = -1/De-dailXMove/(popSize-I-H)
  der_f_X[2,3] = b*S/popSize-dailXMove*E/(popSize-I-H)^2
  der_f_X[2,5] =b*alpha*S/popSize
  der_f_X[2,6] =-dailXMove*E/(popSize-I-H)^2
  
  der_f_X[3,2] =r/De
  der_f_X[3,3] =-1/Dq -1/Di 
  
  der_f_X[4,3] = 1/Di -dailXMove*R/(popSize-I-H)^2
  der_f_X[4,4] = -dailXMove/(popSize-I-H)
  der_f_X[4,5] = 1/Di
  der_f_X[4,6] = 1/Dh -dailXMove*R/(popSize-I-H)^2
  
  der_f_X[5,2] =(1-r)/De
  der_f_X[5,3] =-dailXMove*A/(popSize-I-H)^2
  der_f_X[5,5] =-1/Di-dailXMove/(popSize-I-H)
  der_f_X[5,6] =-dailXMove*A/(popSize-I-H)^2
  
  der_f_X[6,3] =1/Dq
  der_f_X[6,6] =-1/Dh
  
  der_f_theta[1,1] = -S*(I+alpha*A)/popSize
  der_f_theta[2,1] = S*(I+alpha*A)/popSize
  
  der_f_theta[3,2] = E/De
  der_f_theta[5,2] = -E/De
  
  der_f_theta[1,3] = -b*S*A/popSize
  der_f_theta[2,3] = b*S*A/popSize
  
  der_f_theta[2,4] = E/De^2
  der_f_theta[3,4] = -r*E/De^2
  der_f_theta[5,4] = -(1-r)*E/De^2
  
  der_f_theta[3,5] =I/Di^2
  der_f_theta[4,5] =-(I+A)/Di^2
  der_f_theta[5,5] =A/Di^2
  
  der_f_theta[3,6] =I/Dq^2
  der_f_theta[6,6] =-I/Dq^2
  
  der_f_theta[4,7] =-H/Dh^2
  der_f_theta[6,7] =H/Dh^2
  
  ddt_der_X_theta = der_f_X%*%der_X_theta + der_f_theta
  ddt_der_X_theta_vect_form = c(ddt_der_X_theta)
  
  res <- list(c(dXdt,ddt_der_X_theta_vect_form))
  
  return(res)
}


