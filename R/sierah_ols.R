#' OLS Estimation of parameters in SIERAH models
#'
#' @import deSolve
#'
#' @examples
#'
#' data_FRA <- get_data_covid19(maille_cd = "FRA",
#'                            source_ch = "sante-publique-france")
#' #Perform OLS
#' optimresult_FRA <- optimx(c(log(1.75), log(0.41)),
#'                       fn = seirah_ols, data=data_FRA,
#'                       method = "Nelder-Mead")
#' #Plot fitted vs. observed
#' fit_FRA <- seirah_ols(b = c(optimresult_FRA$p1, optimresult_FRA$p2),
#'                   data = data_FRA, fit = TRUE)
#' plot(fit_FRA)
#'
#'
#'data_GE <- get_data_covid19(maille_cd = "REG-44",
#'                            source_ch = "agences-regionales-sante")
#'#optimresult_GE <- optim(par = c(log(1.75),log(0.41)), fn = seirah_ols,
#'                        data = data_GE, popSize=5518000 ,dailyMove=0.1*5518000)
#'#plot(seirah_ols(optimresult_GE$par, data=data_GE, popSize=5518000,dailyMove=0.1*5518000, fit = TRUE))
#'optimxresult_GE <- optimx(c(log(1.75),log(0.41)),
#'                      fn = seirah_ols, data=data_GE, popSize=5518000 ,dailyMove=0.1*5518000,
#'                      method = "Nelder-Mead")
#'fit_GE <- seirah_ols(b = c(optimxresult_GE$p1, optimxresult_GE$p2),
#'                   data=data_GE, popSize=5518000 ,dailyMove=0.1*5518000, fit = TRUE)
#'plot(fit_GE)
#'
#'
#'
#'
#'data_NA <- get_data_covid19(maille_cd = "REG-75",
#'                            source_ch = "agences-regionales-sante")
#'data_NA <- data_NA[-c(1:40), ]
#'data_NA$day <- data_NA$day-40
#'
#'#optimresult_NA<-optim(c(log(1.75),log(0.41)), seirah_ols, data=data_NA, popSize=5987000 ,dailyMove=0.1*5987000)
#'#plot(seirah_ols(optimresult$par,data=data_NA, fit = TRUE, popSize=5987000 ,dailyMove=0.1*5987000))
#'
#'optimxresult_NA <- optimx(c(log(1.75), log(0.41)),
#'                         fn = seirah_ols, data=data_NA, popSize=5987000 ,dailyMove=0.1*5987000,
#'                         method = "Nelder-Mead")
#' #Plot fitted vs. observed
#' fit_FRA <- seirah_ols(b = c(optimxresult_NA$p1, optimxresult_NA$p2),
#'                       data = data_NA, popSize=5987000 ,dailyMove=0.1*5987000, fit=TRUE)
#' plot(fit_FRA)
#'
#' @export
seirah_ols <- function(b, data,
                       alpha=1,De=5.2,Di=2.3,Dq=10,Dh=30,
                       popSize=65000000,dailyMove=1000000,
                       verbose = TRUE, fit=FALSE){

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

  selectedsol <- sol[which(sol[,"time"] %in% data$day), "I"]
  ss <- sum( (data[, "cas_confirmes_incident"] - selectedsol) **2) /
    length(data[, "cas_confirmes_incident"])

  if(verbose){
    message("Sum of  function:", ss)
  }

  if(fit){
    res <- list("solution" = sol,
                "data" = data)
    class(res) <- "seirah_estim"
  }else{
    res <- ss
  }
  return(res)
}


#' Plotting method for an SEIRAH OLS fit object
#'
#' @import ggplot2
#'
#' @export
plot.seirah_estim <- function(x){

  sol_obstime <- x$solution[which(x$solution[,"time"] %in% x$data$day), ]
  ylimmax <- max(rbind(sol_obstime[,"I"], x$data[,"cas_confirmes_incident"]))+1
  data2plot <- cbind.data.frame(x$data, sol_obstime)
  ggplot(data2plot, aes(x=date)) +
    geom_point(aes(y = cas_confirmes_incident, color = "Observed")) +
    geom_line(aes(y = I, color = "SEIRAH")) +
    theme_classic() +
    ylab("Number of incident cases") +
    scale_color_manual("", values=c("black", "blue"))
}
