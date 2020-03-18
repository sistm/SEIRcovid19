# Load deSolve library
#install.packages("deSolve")
library(deSolve)

# Function to return derivatives of SEIR model
seirah_ode<-function(t,Y,par){
  S<-Y[1]
  E<-Y[2]
  I<-Y[3]
  R<-Y[4]
  A<-Y[5]
  H<-Y[6]  
  
  beta
  sigma
  gamma<-par[3]
  mu<-
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
  dYdt[1]=b*S*(I+alpha*A)/popSize+dailyMove-dailyMove*S/(popSize-I-H)
  dYdt[2]=b*S*(I+alpha*A)/popSize-E/De-dailyMove*E/(popSize-I-H)
  dYdt[3]=r*E/De-I/Dq-I/Di
  dYdt[4]=(I+A)/Di+H/Dh-dailyMove*R/(popSize-I-H)
  dYdt[5]=(1-r)*E/De-A/Di-dailyMove*A/(popSize-I-H)
  dYdt[6]=I/Dq-H/Dh
  return(list(dYdt))
}

# Set parameter values
b<-1
r<-1
alpha<-1
De<-5.2
Di<-2.3
Dq<-2
Dh<-30
popSize<-10000000
dailyMove<-0

init<-c(9999467,346,80,0,80,27)
t<-seq(0,365)
par<-c(b,r,alpha,De,Di,Dq,Dh,popSize,dailyMove)
# Solve system using lsoda
sol<-lsoda(init,t,seirah_ode,par)

# Plot solution
plot(t,sol[,3],type="l",col="blue",ylim=c(0,100000),ylab="Proportion")
lines(t,sol[,4],col="green")
lines(t,sol[,5],col="orange")  
lines(t,sol[,6],col="red")  
legend(1,90000,legend=c("Infectious","Not-reported","Hospitalized","Recovert"),col=c("blue","orange","red","green"), lty=1, cex=0.8)
