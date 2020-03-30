#' OLS Estimation of parameters in SIERAH models
#'
#' @import deSolve
#'
#' @export
seirah_ols <- function(b, stateinit,data,
                       alpha=1,De=5.2,Di=2.3,Dq=10,Dh=30,
                       popSize=65000000,dailyMove=1000000,
                       timeconf=1000,lengthconf=1000,newdailyMove=0.00001*popSize,
                       factorreductrans=3,
                       verbose = TRUE,obs="1Y"){

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
           De, Di, Dq, Dh, popSize, dailyMove,timeconf,lengthconf,newdailyMove,
           factorreductrans)

  #solve ode
  sol <- seirah_solve(init, t, par)
  #if(max(sol[,"time"]!=365)){
  #  objectiveFunction<-100
  #}else{
  #  plot(sol)
  #}

  if(obs=="1Y"){
    selectedsol <- sol[which(sol[,"time"] %in% data$day), "I"]
    ss <- sum( (data[, "I"] - selectedsol) **2) /
    length(data[, "I"])
  } else{
    if(obs=="2Y"){
      selectedsol <- sol[which(sol[,"time"] %in% data$day), c("I","H")]
      ss <- sum( (data[, "I"] - selectedsol[,"I"]) **2) /
        length(data[, "I"])  +
        sum( (data[, "H"] - selectedsol[,"H"]) **2,na.rm=TRUE) /
        length(data[, "H"])  
    }
  }

  if(verbose){
    message("Sum of squares(objective function): ", ss, "\n")
  }

  return(ss)
}
