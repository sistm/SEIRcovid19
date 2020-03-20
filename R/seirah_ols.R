#' OLS Estimation of parameters in SIERAH models
#'
#' @import deSolve
#'
#' @export
seirah_ols <- function(b, stateinit,data,
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
  init <- stateinit
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
