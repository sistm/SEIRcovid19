#' seirah estimation for simulx
#'
#' @keywords internal
#'
#' @export
seirah_solve_simulx<-function(time,parameter_and_init){
  seirah_definition <- function(t,Y,par){
    with(as.list(c(Y, par)), {
      if((t>=timeconf)&(t<(timeconf+lengthconf))){
        dailyMove<-newdailyMove
        b<- par[1]/factorreductrans
      }
      if(t>=timeconf+lengthconf){
        dailyMove<-newdailyMove
      }
      dYdt<-vector(length=6)
      dYdt[1]=-b*S*(I+alpha*A)/popSize+dailyMove-dailyMove*S/(popSize-I-H)
      dYdt[2]=b*S*(I+alpha*A)/popSize-E/De-dailyMove*E/(popSize-I-H)
      dYdt[3]=r*E/De-I/Dq-I/Di
      dYdt[4]=(I+A)/Di+H/Dh-dailyMove*R/(popSize-I-H)
      dYdt[5]=(1-r)*E/De-A/Di-dailyMove*A/(popSize-I-H)
      dYdt[6]=I/Dq-H/Dh
      res <- list(dYdt)
      return(res)
    })
  }
  time <- time[[1]]
  parameter<-parameter_and_init[1:parameter_and_init[length(parameter_and_init)]]
  init<-parameter_and_init[(parameter_and_init[length(parameter_and_init)]+1):(length(parameter_and_init)-1)]
  solution <- deSolve::ode(init, time, seirah_definition, parameter)
  solution <- as.data.frame(solution)
  colnames(solution) <- c("time", "S", "E", "I", "R", "A", "H")
  C<-data.frame(solution)
  r <- list(C=C)
  return(r)
}
