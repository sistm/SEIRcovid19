#' Fitting a SEIRAH by OLS
#'
#' @export
#'
#' @importFrom stats optim
#'
#' @examples
#' data_44 <- get_data_covid19(maille_cd = "REG-44",
#'                              source_ch = "agences-regionales-sante")
#'
#' colnames(data_44)[5] <- "I"
#' colnames(data_44)[6] <- "D"
#' colnames(data_44)[7] <- "H"
#' data_44[data_44$date > "2020-03-16", "I"] <- NA
#' fit_44 <- seidrah_estim(binit = c(1.75, 0.7, 0.01),
#'                         popSize = 5.5*10^6,
#'                         data = data_44, obs=c("I", "D", "H"))
#'plot(fit_44)
#'
#' data_FRA <- get_data_covid19(maille_cd = "FRA",
#'                              source_ch = "sante-publique-france")
#' colnames(data_FRA)[5] <- "I"
#' colnames(data_FRA)[6] <- "D"
#' colnames(data_FRA)[7] <- "H"
#' fit_FRA <- seidrah_estim(binit = c(1.75, 0.7, 0.01),
#'                         popSize = 65*10^6,
#'                         data = data_FRA, obs=c("I", "D", "H"))
#'plot(fit_FRA)
#'
#'
#' simul_xihong<- seirah_estim(binit=c(1.75, 0.41),data=NULL,stateinit=c(9999467,346,80,0,80,27),
#'                             initwithdata=FALSE,alpha=1,De=5.2,Di=2.3,Dq=10,Dh=30,popSize=10000000,dailyMove=500000,verbose = TRUE,optim_ols = FALSE)
#' plot_list.seirah_estim(list(simul_xihong))
seidrah_estim <- function(binit, data=NULL, stateinit=NULL, initwithdata=TRUE,
                          alpha=1, De=5.2, Di=2.3, Dq=10, Dh=30,
                          popSize=65000000, dailyMove=0.1*popSize,
                          verbose = TRUE, optim_ols = TRUE, timeconf=1000,
                          lengthconf=1000,
                          newdailyMove=0.00001, factorreductrans=3,
                          obs = c("S", "E", "I", "D", "R", "A", "H")){

  if(is.null(stateinit) & is.null(data)){
    stop("Initial states or data need to be provided")
  }
  if(is.null(stateinit) & !initwithdata){
    stop("Initial states values need to be provided")
  }
  if(is.null(data) & initwithdata){
    stop("Data need to be provided")
  }
  if(is.null(data)){
    optim_ols <- FALSE
  }
  if("S" %in% obs){
    warning("Observed 'S' is not taken into account")
  }
  stopifnot("I" %in% obs)


  init <- numeric(7)
  names(init) <- c("S", "E", "I", "D", "R", "A", "H")

  if(initwithdata){
    for(comp in obs){
      init[comp] <- data[1 , comp]
      if(is.na(init[comp])){
        init[comp] <- 0
      }
    }

    if(!("H" %in% obs)){
      init["H"] <- init["I"]*0.5
    }
    if(!("A" %in% obs)){
      init["A"] <- init["I"] # A = I
    }
    if(!("E" %in% obs)){
      init["E"] <- init["I"]*2 # Twice the number of confirmed cases
    }
    if(!("D" %in% obs)){
      init["D"] <- 0
    }
    if(!("R" %in% obs)){
      init["R"] <- 0
    }

    init["I"] <- init["I"] - init["H"] # the number of confirmed cases minus hospitalized
    init["S"] <- popSize - init["E"] - init["I"] - init["A"] - init["H"] - init["R"] #N-E-I-A-H-R

  }else{
    init <-stateinit
  }

  if(optim_ols){
    param_optimal <- optim(c(log(binit[1]), logit(binit[2]), logit(binit[3])),
                           fn = seidrah_ols,
                           stateinit=init, data=data,
                           alpha=alpha, De=De, Di=Di, Dq=Dq, Dh=Dh,
                           popSize=popSize, dailyMove=dailyMove,
                           timeconf=timeconf, lengthconf=lengthconf, newdailyMove=newdailyMove,
                           factorreductrans=factorreductrans,
                           verbose = verbose, obs=obs)
    transmission <- exp(param_optimal$par[1])
    ascertainment <- expit(param_optimal$par[2])
    deathrate <- expit(param_optimal$par[3])

  }else{
    param_optimal <- list()
    param_optimal[["par"]] <- binit
    transmission <- param_optimal$par[1]
    ascertainment <- param_optimal$par[2]
    deathrate <- deathrate$par[3]
  }

  # Optimal solution


  t <- seq(0,365)
  par <- c(transmission, ascertainment, deathrate,
           alpha, De, Di, Dq, Dh, popSize, dailyMove, timeconf, lengthconf,
           newdailyMove,factorreductrans)
  res_optimal <- seidrah_solve(init, t, par)

  res <- list("solution" = res_optimal,
              "parameters" = list(
                "transmission" = transmission,
                "ascertainment" = ascertainment,
                "deathrate" = deathrate,
                "alpha" = alpha,
                "De" = De,
                "Di" = Di,
                "Dq" = Dq,
                "Dh" = Dh,
                "popSize" = popSize,
                "dailyMove" = dailyMove,
                "timeconf"=timeconf,
                "lengthconf"=lengthconf,
                "newdailyMove"=newdailyMove,
                "factorreductrans"=factorreductrans
              ),
              "obs" = obs,
              "initvalues" = init,
              "data" = data)

  class(res) <- "seidrah_estim"

  return(res)
}




#' Plotting method for a SEIDRAH fit object
#'
#' @import ggplot2
#' @importFrom reshape2 melt
#'
#' @export
plot.seidrah_estim <- function(x, log=FALSE){

  sol_obstime <- x$solution[which(x$solution[,"time"] %in% x$data$day), ]
  sol_obstime$origin = "SEIDRAH model"
  sol_obstime$date <- x$data$date
  data2plot_sol <- reshape2::melt(sol_obstime, id.vars = c("time", "date", "origin"))
  colnames(data2plot_sol)[1] <- "day"
  data2plot_sol <- data2plot_sol %>% filter(variable %in% x$obs)

  x$data$origin = "Observed"
  data2plot_obs <- reshape2::melt(x$data[, -c(2:3)], id.vars = c("day", "date", "origin"))

  data2plot_obs <- data2plot_obs %>% filter(variable %in% x$obs)

  data2plot <- rbind.data.frame(data2plot_obs, data2plot_sol)

    p <- ggplot(data2plot_obs, aes(x=date, y=value, group=variable)) +
      geom_point(aes(color = origin)) +
      geom_line(data = data2plot_sol, aes(color = origin)) +
      scale_color_manual("", values=c("black", "blue")) +
      theme_classic() +
      ylab("Number of incident cases") +
      ylim(0, NA) +
      facet_wrap(~variable, scales = "free_y")

  if(log){
    p <- p + scale_y_log10()
  }

  return(p)
}
