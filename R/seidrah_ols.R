#' OLS Estimation of parameters in SIEDRAH models
#'
#' @import deSolve
#'
#' @examples
#'
#'
#' @export
seidrah_ols <- function(unknowns, stateinit, data,
                       alpha=1,De=5.2,Di=2.3,Dq=10,Dh=30,
                       popSize=65000000,dailyMove=1000000,
                       timeconf=1000,lengthconf=1000,newdailyMove=0.00001*popSize,
                       factorreductrans=3,
                       verbose = TRUE,
                       obs =c("S", "E", "I", "D", "R", "A", "H")){

  transmission <- exp(unknowns[1])
  ascertainment <- expit(unknowns[2])
  deathrate <- expit(unknowns[3])

  if(verbose){
    message("transmission: ", transmission)
    message("ascertainment: ", ascertainment)
    message("death rate: ", deathrate)
  }

  # Initialization
  init <- stateinit
  t <- seq(from = 0, to = 365, by = 1)
  par <- c(transmission, ascertainment, deathrate,
           alpha,
           De, Di, Dq, Dh, popSize, dailyMove, timeconf, lengthconf,
           newdailyMove,
           factorreductrans)

  #solve ode
  sol <- seidrah_solve(init, t, par)
  #if(max(sol[,"time"]!=365)){
  #  objectiveFunction<-100
  #}else{
  #  plot(sol)
  #}

  ss <- list()
  for(comp in obs){
    nobscomp <- sum(!is.na(data[, comp]))
    if(nobscomp == 0){
      warning("No observation in compartment", comp, " declared observed")
    }
    selectedsol <- sol[which(sol[,"time"] %in% data$day), comp]
    ss[[comp]] <- sum( (data[, comp] - selectedsol) **2, na.rm=TRUE)/nobscomp
  }

  sss <- do.call(sum, ss)
  if(verbose){
    message("Sum of squares(objective function): ", sss, "\n")
  }

  return(sss)
}
