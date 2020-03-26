#' Fitting a SEIRAH by OLS
#'
#' @export
#'
#' @importFrom stats optim
#'
#' @examples
#'
#' 
#' load("./data/opencovid19_consolidated_reg.RData") 
#'data_region<-as.data.frame(opencovid19_consolidated_reg[[4]])
#'
#'data_region<-as.data.frame(data_region)
#'data_region$dateinit<-NA
#'data_region$dateinit[1]<-data_region$date[1]
#'data_region$day<-NA
#'data_region$day[1]<-0
#'for(i in 2:length(data_region[,"maille_code"])){
#'  if(data_region[i,"maille_code"]==data_region[i-1,"maille_code"]){
#'    data_region$dateinit[i]<-data_region$dateinit[i-1]
#'    data_region$day[i]<-data_region$date[i]-data_region$dateinit[i]
#'  }else{
#'    data_region$dateinit[i]<-data_region$date[i]
#'    data_region$day[i]<-0
#'  }
#'} 
#' temp<-seidrauh_estim(binit, data=data_region, stateinit=NULL, initwithdata=TRUE,alpha=1, De=5.2, Di=2.3, Dq=10, Dh=30,popSize=2795301, dailyMove=0.01*popSize,verbose = TRUE, optim_ols = TRUE, timeconf=1000,lengthconf=1000,newdailyMove=0.00001, factorreductrans=3,obs = c("I", "D", "R", "U", "H"))

#' 
#' 
#' binit<-c(1.0,0.8,10,10,10,10,10)
#' data=data_region
#' stateinit=NULL
#' initwithdata=TRUE
#' alpha=7
#' De=5.2
#' Di=2.3
#' Dq=10
#' Dh=30
#' popSize=2795301
#' dailyMove=0.01*popSize
#' verbose = TRUE
#' optim_ols = TRUE
#' timeconf=1000
#' lengthconf=1000
#' newdailyMove=0.00001
#' factorreductrans=3
#' obs = c("I", "D", "R", "U", "H")
#' 


seidrauh_estim <- function(binit, data=NULL, stateinit=NULL, initwithdata=TRUE,
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


  init <- numeric(8)
  names(init) <- c("S", "E", "I", "D", "R", "A","U", "H")

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
    if(!("U" %in% obs)){
      init["U"] <- 0
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
    init["S"] <- popSize - init["E"] - init["I"] - init["A"] - init["H"] - init["D"]- init["U"]- init["R"] #N-E-I-A-H-R

  }else{
    init <-stateinit
  }

  if(optim_ols){
    param_optimal <- optim(c(log(binit[1]), logit(binit[2]), log(binit[3]),log(binit[4]),log(binit[5]),log(binit[6]),log(binit[7])),
                           fn = seidrauh_ols,
                           stateinit=init, data=data,
                           alpha=alpha, De=De, Di=Di, Dq=Dq, Dh=Dh,
                           popSize=popSize, dailyMove=dailyMove,
                           timeconf=timeconf, lengthconf=lengthconf, newdailyMove=newdailyMove,
                           factorreductrans=factorreductrans,
                           verbose = verbose, obs=obs)
    
    
    transmission <- exp(param_optimal$par[1])
    ascertainment <- expit(param_optimal$par[2])
    hospitalrate <- exp(param_optimal$par[3])
    Mh <- exp(param_optimal$par[4])
    Mi <- exp(param_optimal$par[5])
    Mu <- exp(param_optimal$par[6])
    Du <- exp(param_optimal$par[7])
    
  }else{
    param_optimal <- list()
    param_optimal[["par"]] <- binit
    transmission <- param_optimal$par[1]
    ascertainment <- param_optimal$par[2]
    deathrate <- param_optimal$par[3]
    Mh <- param_optimal$par[4]
    Mi <- param_optimal$par[5]
    Mu <- param_optimal$par[6]
    Du <- param_optimal$par[7]
  }

  # Optimal solution


  t <- seq(0,365)
  par <- c(transmission, ascertainment, hospitalrate,
           alpha, De, Di, Dq, Dh, popSize, dailyMove, timeconf, lengthconf,
           newdailyMove,factorreductrans,Mh,Mi,Mu,Du)
  res_optimal <- seidrauh_solve(init, t, par)

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

  class(res) <- "seidrauh_estim"

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
  sol_obstime$origin = "SEIDRAUH model"
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
