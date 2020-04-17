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
  
  r<-as.numeric(par[2])
  alpha<-as.numeric(par[3])
  De<-as.numeric(par[4])
  Di<-as.numeric(par[5])
  Dq<-as.numeric(par[6])
  Dh<-as.numeric(par[7])
  popSize<-as.numeric(par[8])
  dailyMove<-as.numeric(par[9])
  timeconf<-as.numeric(par[10])
  lengthconf<-as.numeric(par[11])
  newdailyMove<-as.numeric(par[12])
  typecov<- par[13]
 
  if(typecov=="constant"){
   b<-as.numeric(par[1])
    if((t>=timeconf)&(t<(timeconf+8))){
      dailyMove<-newdailyMove
      b<- as.numeric(par[1])*exp(as.numeric(par[14]))
    }
   if((t>=timeconf+8)&(t<(timeconf+lengthconf))){
     dailyMove<-newdailyMove
     b<- as.numeric(par[1])*exp(as.numeric(par[14]))*exp(as.numeric(par[15]))
   }
    if(t>=timeconf+lengthconf){
      dailyMove<-newdailyMove
    }
  }
  if(typecov=="parametric"){
    b<-as.numeric(par[1])
    if((t>=timeconf)&(t<(timeconf+lengthconf))){
      if(t<timeconf+as.numeric(par[15])){
        dailyMove<-newdailyMove
        b<- as.numeric(par[1])*exp(as.numeric(par[14])*(t-timeconf))
      }else{
        dailyMove<-newdailyMove
        b<- as.numeric(par[1])*exp(as.numeric(par[14])*as.numeric(par[15]))
        }
    }
    if(t>=timeconf+lengthconf){
      dailyMove<-newdailyMove
    }
  }
  if(typecov=="splines"){
    splinescoeff1<-as.numeric(par[14])
    splinescoeff2<-as.numeric(par[15])
    splinescoeff3<-as.numeric(par[16])
    ns1<-as.numeric(par[17])
    ns2<-as.numeric(par[18])
    b<- as.numeric(par[1])

    if((t>=timeconf)&(t<(timeconf+lengthconf))){
      dailyMove<-newdailyMove
      b<- as.numeric(par[1])*exp(splinescoeff1+splinescoeff2*ns1+splinescoeff3*ns2)
      print(paste("t",t,sep=" "))
      print(paste("all",exp(splinescoeff1+splinescoeff2*ns1+splinescoeff3*ns2),sep=" "))
      print(paste("splinescoeff1",splinescoeff1,sep=" "))
      print(paste("splinescoeff2",splinescoeff2,sep=" "))
      print(paste("splinescoeff3",splinescoeff3,sep=" "))
      print(paste("ns1",ns1,sep=" "))
      print(paste("ns2",ns2,sep=" "))
      pause <- readLines(n=1)
    }
    if(t>=timeconf+lengthconf){
      dailyMove<-newdailyMove
    }
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
