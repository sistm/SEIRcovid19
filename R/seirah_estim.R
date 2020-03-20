#' Fitting a SEIRAH by OLS
#'
#' @export
#'
#' @importFrom stats optim
#'
#' @examples
#' data_FRA <- get_data_covid19(maille_cd = "FRA",
#'                              source_ch = "sante-publique-france")
#' fit_FRA <- seirah_estim(binit = c(1.75, 0.41),
#'                         data = data_FRA)
#' plot(fit_FRA)
#'
#' data_GE <- get_data_covid19(maille_cd = "REG-44",
#'                             source_ch = "agences-regionales-sante")
#' fit_GE <- seirah_estim(binit = c(1.75, 0.41),
#'                        data=data_GE, popSize = 5518000, dailyMove = 0.1*5518000)
#' plot(fit_GE)
#'
#'data_NA <- get_data_covid19(maille_cd = "REG-75",
#'                            source_ch = "agences-regionales-sante")
#'fit_NA <- seirah_estim(binit = c(1.75, 0.41),
#'                       data = data_NA,
#'                       popSize = 5987000, dailyMove = 0.1*5987000)
#' plot(fit_NA)
#' 
#' 
#' simul_xihong<- seirah_estim(binit=c(1.75, 0.41),data=NULL,stateinit=c(9999467,346,80,0,80,27),
#' initwithdata=FALSE,alpha=1,De=5.2,Di=2.3,Dq=10,Dh=30,popSize=65000000,dailyMove=0.1*65000000,verbose = TRUE,optim_ols = FALSE)
#' plot(simul_xihong)


seirah_estim <- function(binit, data=NULL,stateinit=NULL,initwithdata=TRUE,
                         alpha=1,De=5.2,Di=2.3,Dq=10,Dh=30,
                         popSize=65000000, dailyMove=0.1*popSize,
                         verbose = TRUE, optim_ols = TRUE){

  if((is.null(stateinit))&((is.null(data)))){
    stop("Initial states or data need to be provided")
  }
  if((is.null(stateinit))&((!initwithdata))){
    stop("Initial states values need to be provided")
  }
  if((is.null(data))&((initwithdata))){
    stop("Data need to be provided")
  }  
  if(is.null(data))optim_ols<-FALSE
  
  if(initwithdata){
    E0 <- data[1, "cas_confirmes_incident"]*2 # Twice the number of cases
    I0 <- data[1, "cas_confirmes_incident"] # Numbers of cases
    R0 <- 0 #(0 ref)
    A0 <- I0 # A=I
    H0 <- I0*0.50 #all at the begining H=50%I
    S0 <- popSize - E0 - I0 - A0 - H0 - R0 #N-E-I-A-H-R
    init <- c(S0, E0, I0, R0, A0, H0)
  }else{
    init <-stateinit
  }
  
  
  if(optim_ols){
    param_optimal <- optim(log(binit),
                           fn = seirah_ols,
                           stateinit=init,data=data,
                           alpha=alpha,De=De,Di=Di,Dq=Dq,Dh=Dh,
                           popSize=popSize,dailyMove=dailyMove,
                           verbose = verbose)
    transmission <- exp(param_optimal$par[1])
    ascertainment <- exp(param_optimal$par[2])
    }else{
      param_optimal <- list()
      param_optimal[["par"]] <- binit
      transmission <- param_optimal$par[1]
      ascertainment <- param_optimal$par[2]
    }

  # Optimal solution

 
  t <- seq(0,365)
  par <- c(transmission, ascertainment, alpha,
           De, Di, Dq, Dh, popSize, dailyMove)
  res_optimal <- seirah_solve(init, t, par)

  res <- list("solution" = res_optimal,
              "parameters" = list(
                "transmission" = transmission,
                "ascertainment" = ascertainment,
                "alpha" = alpha,
                "De" = De,
                "Di" = Di,
                "Dq" = Dq,
                "Dh" = Dh,
                "popSize" = popSize,
                "dailyMove" = dailyMove,
                "S0" = init[1],
                "E0" = init[2],
                "I0" = init[3],
                "R0" = init[4],
                "A0" = init[5],
                "H0" = init[6]
              ),
              "data" = data)

  class(res) <- "seirah_estim"

  return(res)
}

#' Plotting method for a SEIRAH fit object
#'
#' @import ggplot2
#'
#' @export
plot.seirah_estim <- function(x){

  sol_obstime <- x$solution[which(x$solution[,"time"] %in% x$data$day), ]

  data2plot <- cbind.data.frame(x$data, sol_obstime)

  ggplot(data2plot, aes(x=date)) +
    geom_point(aes(y = cas_confirmes_incident, color = "Observed")) +
    geom_line(aes(y = I, color = "SEIRAH")) +
    theme_classic() +
    ylab("Number of incident cases") +
    scale_color_manual("", values=c("black", "blue"))

}

#' Plotting method for a list of SEIRAH fit objects
#'
#' @import ggplot2
#'
#' @export
plot_list.seirah_estim <- function(x){

  
  if(is.null(x$data)){
    ggplot(x$solution, aes(x=time)) +
      geom_line(aes(y = S, color = "S")) +
      geom_line(aes(y = E, color = "E")) +
      geom_line(aes(y = I, color = "I")) +
      geom_line(aes(y = R, color = "R")) +
      geom_line(aes(y = A, color = "A")) +
      geom_line(aes(y = H, color = "H")) +
      theme_classic() +
      ylab("Number of incident cases") +
      scale_color_manual("", values=c("black", "orange","blue","green","purple","red"))
  }else{
    browser()
    ldf <- lapply(x, function(l){
      temp <- as.data.frame(l$solution)
      temp$maille_code <- names(l)
      temp$date <- l$data$date[1]+temp$time
      return(temp)
    })
    all_preds <- do.call(rbind.data.frame, ldf)
    
    
    sol_obstime <- x$solution[which(x$solution[,"time"] %in% x$data$day), ]
    data2plot <- cbind.data.frame(x$data, sol_obstime)
    
     ggplot(data2plot, aes(x=date)) +
        geom_point(aes(y = cas_confirmes_incident, color = "Observed")) +
        geom_line(aes(y = I, color = "SEIRAH")) +
        theme_classic() +
        ylab("Number of incident cases") +
        scale_color_manual("", values=c("black", "blue"))
  }
}
