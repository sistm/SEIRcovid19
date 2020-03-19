#' OLS Estimation of parameters in SIERAH models
#'
#' @import deSolve
#'
#' @examples
#' dataFR<-get_data_covid19(maille = "France",
#'              source = "Santé publique France")
#' #Perform OLS            
#' optimresult<-optim(c(log(1.75),log(0.41)), ols_sierah,data=dataFR)
#' #Plot fitted vs. observed 
#' ols_sierah(optimresult$par,data=dataFR,plot.comparison = TRUE)
#'
#'
#'GrandEST<-get_data_covid19(maille = "Grand-Est",
#'               source = "ARS Grand-Est")
#'optimresult<-optim(c(log(1.75),log(0.41)), ols_sierah,data=GrandEST,popSize=5518000 ,dailyMove=0.1*5518000)
#'ols_sierah(optimresult$par,data=GrandEST,popSize=5518000 ,dailyMove=0.1*5518000,plot.comparison = TRUE)
#'
#'
#'NouvelleAquitaine<-get_data_covid19(maille = "Nouvelle-Aquitaine",
#'               source = "ARS Nouvelle-Aquitaine")
#'NouvelleAquitaine<-NouvelleAquitaine[41:54,]
#'NouvelleAquitaine$day<-NouvelleAquitaine$day-40
#'optimresult<-optim(c(log(1.75),log(0.41)), ols_sierah,data=NouvelleAquitaine,popSize=5987000 ,dailyMove=0.1*5987000)
#'ols_sierah(optimresult$par,data=NouvelleAquitaine,plot.comparison = TRUE,popSize=5987000 ,dailyMove=0.1*5987000)
#' @export
sierah_ols <- function(b,data,alpha=1,De=5.2,Di=2.3,Dq=10,Dh=30,popSize=65000000,dailyMove=1000000,
                       verbose = TRUE, plot_comparison=FALSE){

  transmission <- exp(b[1])
  ascertainment <- exp(b[2])

  if(verbose){
    message("transmission:", transmission)
    message("ascertainment:", ascertainment)
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

  selectedsol <- sol[which(sol[,"time"]%in%data$day), 4]
  res <- sum( (data[, "cas_confirmes_incident"] - selectedsol) **2) /
    length(data[, "cas_confirmes_incident"])

  if(verbose){
    message("Objective function:", res)
  }
  if(plot_comparison){
    ylimmax<-max(rbind(selectedsol,data[,"cas_confirmes_incident"]))+1
    plot(sol[which(sol[,"time"]%in%data$day),1],selectedsol,type="l",col="blue",ylim=c(0,ylimmax),ylab="Proportion")
    points(data[,"day"], data[,"cas_confirmes_incident"],col="black")
  }

  class(res) <- "seirah_estim"
  return(res)
}


