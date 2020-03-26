#' SEIDRAUH derivative functions
#'
#' @keywords internal
#'
#' @export
seidrauh_ode <- function(t,Y,par){

  stopifnot(ncol(Y) == 8)
  S <- Y[1]
  E <- Y[2]
  I <- Y[3]
  D <- Y[4]
  R <- Y[5]
  A <- Y[6]
  U <- Y[7]
  H <- Y[8]

  stopifnot(length(par) == 18)
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
  Mh <- par[15]
  Mi <- par[16]
  Mu <- par[17]
  Du <- par[18]

  if((t>timeconf)&(t<(timeconf+lengthconf))){
    dailyMove <- newdailyMove
    b <- b/factorreductrans
  }

  dYdt <- numeric(length = 7)
  dYdt[1] <- -b*S*(I+alpha*A)/popSize + dailyMove - dailyMove*S/(popSize-I-H-D-U) #S
  dYdt[2] <- b*S*(I+alpha*A)/popSize-E/De - dailyMove*E/(popSize-I-H-D-U) #E
  dYdt[3] <- r*E/De - I/Dq - I/Di -I/Mi #I
  dYdt[4] <- U/Mu + I/Mi #D
  dYdt[5] <- (I+A)/Di + H/Dh +U/Du - dailyMove*R/(popSize-I-H-D-U) #R
  dYdt[6] <- (1-r)*E/De - A/Di - dailyMove*A/(popSize-I-H-D-U) #A
  dYdt[7] <- H/Mh -U/Du-U/Mu #U
  dYdt[8] <- I/Dq - H/Dh -H/Mh #H

  # for (i in 1:6){
  #   if(init[i]+dYdt[i]<1)dYdt[i]=-init[i]
  # }
  res <- list(dYdt)

  return(res)
}


