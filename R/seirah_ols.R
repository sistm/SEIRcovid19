#' OLS Estimation of parameters in SIERAH models
#'
#' @import deSolve
#'
#' @export
seirah_ols <- function(b, data,
                       alpha=1,De=5.2,Di=2.3,Dq=10,Dh=30,
                       popSize=65000000,dailyMove=1000000,
                       verbose = TRUE){

  transmission <- exp(b[1])
  ascertainment <- exp(b[2])

  if(verbose){
    message("transmission: ", transmission)
    message("ascertainment: ", ascertainment)
  }

  # Initialization
  E0 <- data[1, "cas_confirmes_incident"]*2 # Twice the number of cases (346 ref)
  I0 <- data[1, "cas_confirmes_incident"] # Numbers of cases (80 ref)
  R0 <- 0 #(0 ref)
  A0 <- I0 # A=I (80 ref)
  H0 <- I0*0.50 #all at the begining H=50%I (27 ref)
  S0 <- popSize - E0 - I0 - A0 - H0 - R0 #N-E-I-A-H-R (9999467 ref)
  init <- c(S0, E0, I0, R0, A0, H0)
  t <- seq(0,365)
  par <- c(transmission, ascertainment, alpha,
           De, Di, Dq, Dh, popSize, dailyMove)

  #solve ode
  sol <- seirah_solve(init, t, par)
  #if(max(sol[,"time"]!=365)){
  #  objectiveFunction<-100
  #}else{
  #  plot(sol)
  #}

  selectedsol <- sol[which(sol[,"time"] %in% data$day), "I"]
  ss <- sum( (data[, "cas_confirmes_incident"] - selectedsol) **2) /
    length(data[, "cas_confirmes_incident"])

  if(verbose){
    message("Sum of squares(objective function): ", ss, "\n")
  }

  return(ss)
}
