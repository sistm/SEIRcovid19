#' SEIDRAH derivative functions
#'
#' @keywords internal
#'
#' @export
seidrah_ode <- function(t,Y,par){

  stopifnot(ncol(Y) == 7)
  S <- Y[1]
  E <- Y[2]
  I <- Y[3]
  D <- Y[4]
  R <- Y[5]
  A <- Y[6]
  H <- Y[7]

  stopifnot(length(par) == 14)
  b <- par[1]
  r <- par[2]
  dr <- par[3]
  alpha <- par[4]
  De <- par[5]
  Di <- par[6]
  Dq <- par[7]
  Dh <- par[8]
  popSize <- par[9]
  dailyMove <- par[10]
  timeconf <- par[11]
  lengthconf <- par[12]
  newdailyMove <- par[13]
  factorreductrans <- par[14]

  if((t>timeconf)&&(t<(timeconf+lengthconf))){
    dailyMove <- newdailyMove
    b <- b/factorreductrans
  }

  dYdt <- numeric(length = 7)
  dYdt[1] <- -b*S*(I+alpha*A)/popSize + dailyMove*S/(S+E+A+R) - dailyMove*S/(popSize-I-H-D) #S
  dYdt[2] <- b*S*(I+alpha*A)/popSize-E/De - dailyMove*E/(popSize-I-H-D) + dailyMove*E/(S+E+A+R) #E
  dYdt[3] <- r*E/De - I/Dq - I/Di #I
  dYdt[4] <- dr*H/Dh
  dYdt[5] <- (I+A)/Di + (1-dr)*H/Dh - dailyMove*R/(popSize-I-H-D) + dailyMove*R/(S+E+A+R)#R
  dYdt[6] <- (1-r)*E/De - A/Di - dailyMove*A/(popSize-I-H-D) + dailyMove*A/(S+E+A+R)#A
  dYdt[7] <- I/Dq - H/Dh #H

  # for (i in 1:6){
  #   if(init[i]+dYdt[i]<1)dYdt[i]=-init[i]
  # }
  res <- list(dYdt)

  return(res)
}


