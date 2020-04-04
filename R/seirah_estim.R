#' Fitting a SEIRAH by OLS
#'
#' @export
#'
#' @importFrom stats optim
#'
#' @examples
#' 
#' 
# data<-read.table("./monolix2Y/data_region_2y_cumullissage20200329.txt",header=TRUE)
# data<-data[which(data$observationid==1),]
# data$I<-data$lissageI
# data$H<-data$lissageH
# data1<-data[which(data$maille_code=="REG-11"),]
# 
# temp<-seirah_estim(binit = c(1.0, 0.4),initwithdata=TRUE,obs="2Y",De=0.0001,popSize=data1$popsize[1], data = data1,optim_ols =TRUE)
# plot(temp)


#' data_FRA <- get_data_covid19(maille_cd = "FRA",
#'                              source_ch = "sante-publique-france")
#' fit_FRA <- seirah_estim(binit = c(1.0, 0.99),stateinit=c(64999999,1,1,0,5,1),initwithdata=FALSE,obs="2Y",
#'                         data = data_FRA,optim_ols =TRUE)
#' print(plot(fit_FRA))
#' 
#' 
#' H_0=1
# I_0=1
# E_0=1
# R_0=0
# A_0=5
# S_0=p
#' data_75 <- get_data_covid19(maille_cd = "REG-75",
#'                              source_ch = "sante-publique-france")
#' fit_75<- seirah_estim(binit = c(1.75, 0.41),
#'                         data = data_75)
#' print(plot(fit_FRA))
#' fit_FRA <- seirah_estim(binit = c(1.75, 0.41),
#'                         data = data_FRA,obs="2Y")
#' print(plot(fit_FRA,type=1))                    
#' print(plot(fit_FRA,type=2)) 
#'    
#' plot.seirah_solve(fit_FRA$solution)
#' fit_FRA <- seirah_estim(binit = c(fit_FRA$parameters$transmission  , fit_FRA$parameters$ascertainment  ),
#'                         data = data_FRA,dailyMove=0.1*popSize,timeconf=15,newdailyMove=0,factorreductrans=2,optim_ols=FALSE)
#' plot.seirah_solve(fit_FRA$solution)                        
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
#'                             initwithdata=FALSE,alpha=1,De=5.2,Di=2.3,Dq=10,Dh=30,popSize=10000000,dailyMove=500000,verbose = TRUE,optim_ols = FALSE)
#' plot_list.seirah_estim(list(simul_xihong))
seirah_estim <- function(binit, data=NULL,stateinit=NULL,initwithdata=TRUE,
                         alpha=1,De=5.2,Di=2.3,Dq=10,Dh=30,
                         popSize=65000000, dailyMove=0.1*popSize,
                         verbose = TRUE, optim_ols = TRUE,timeconf=1000,lengthconf=1000,
                         newdailyMove=0.00001,factorreductrans=3,obs="1Y",E0given=NULL,A0given=NULL,b2=NULL,pred=FALSE){

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
    if(obs=="2Y"){
     # if((data[1, "hospitalisation_incident"]==0)|(is.na(data[1, "hospitalisation_incident"]))){
     
        H0 <- data[1, "init_H0"]#data[1, "hospitalisation_incident"]
      
        E0 <- ifelse(is.null(E0given),2*data[1, "init_I0"],E0given)#data[1, "cas_confirmes_incident"]*2 # Twice the number of cases
        I0 <- data[1, "init_I0"]#data[1, "cas_confirmes_incident"]-H0 # Numbers of cases
        R0 <- 0 #(0 ref)
        A0 <- ifelse(is.null(A0given),data[1, "init_I0"],A0given) #I0 # A=I
        S0 <- popSize - E0 - I0 - A0 - H0 - R0 #N-E-I-A-H-R
        init <- c(S0, E0, I0, R0, A0, H0)
    }else{
      if(obs=="1Y"){
       H0 <- 0.5*data[1, "cas_confirmes_incident"]
       E0 <- data[1, "cas_confirmes_incident"]*2 # Twice the number of cases
       I0 <- 0.5*data[1, "cas_confirmes_incident"] # Numbers of cases
       R0 <- 0 #(0 ref)
       A0 <- I0 # A=I
       S0 <- popSize - E0 - I0 - A0 - H0 - R0 #N-E-I-A-H-R
       init <- c(S0, E0, I0, R0, A0, H0)
      }else{
        stop("Unrecognized observation model")
      }
    }
  }else{
    init <-stateinit
  }


  if(optim_ols){
    param_optimal <- optim(c(log(binit[1]),logit(binit[2])),
                           fn = seirah_ols,
                           stateinit=init,data=data,
                           alpha=alpha,De=De,Di=Di,Dq=Dq,Dh=Dh,
                           popSize=popSize,dailyMove=dailyMove,
                           timeconf=timeconf,lengthconf=lengthconf,newdailyMove=newdailyMove,
                           factorreductrans=factorreductrans,
                           verbose = verbose,obs=obs)
    print(param_optimal$convergence)
    transmission <- exp(param_optimal$par[1])
    ascertainment <- expit(param_optimal$par[2])
  }else{
    param_optimal <- list()
    param_optimal[["par"]] <- binit
    transmission <- param_optimal$par[1]
    ascertainment <- param_optimal$par[2]
  }

  # Optimal solution

  factorreductrans<-ifelse(is.null(b2),factorreductrans,transmission/b2)
  t <- seq(0,365)
  par <- c(transmission, ascertainment, alpha,
           De, Di, Dq, Dh, popSize, dailyMove,timeconf,lengthconf,newdailyMove,factorreductrans)
  res_optimal <- seirah_solve(init, t, par,pred)

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
                "timeconf"=timeconf,
                "lengthconf"=lengthconf,
                "newdailyMove"=newdailyMove,
                "factorreductrans"=factorreductrans,
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
#' 
plot.seirah_estim <- function(x,type=1){

sol_obstime <- x$solution[which(x$solution[,"time"] %in% x$data$day), ]
names(sol_obstime)<-c("day","Smod","Emod","Imod","Rmod","Amod","Hmod")
sol_obstime$Hmodest<-sol_obstime$Imod/x$parameters$Dq
sol_obstime$Imodest<-x$parameters$ascertainment*sol_obstime$Emod/x$parameters$De

temp<-x$data[which(x$data$obs_id==type),]
data2plot <- merge(temp, sol_obstime, by = "day")

if(type==1){
  p<-ggplot(data2plot, aes(x=day)) +
  geom_point(aes(y = obs, color = "Observed")) +
  geom_line(aes(y = Imodest, color = "SEIRAH")) +
  theme_classic() +
  ylab("Number of incident cases") +
  scale_color_manual("", values=c("black", "blue"))}

if(type==2){p<-ggplot(data2plot, aes(x=day)) +
  geom_point(aes(y = obs, color = "Observed")) +
  geom_line(aes(y = Hmodest, color = "SEIRAH")) +
  theme_classic() +
  ylab("Number of hospitalization") +
  scale_color_manual("", values=c("black", "blue"))}
return(p)

}
# plot.seirah_estim <- function(x,type=1){
# 
#   sol_obstime <- x$solution[which(x$solution[,"time"] %in% x$data$day), ]
#   names(sol_obstime)<-c("time","Smod","Emod","Imod","Rmod","Amod","Hmod")
#   
#   data2plot <- cbind.data.frame(x$data, sol_obstime)
# 
#   if(type==1){p<-ggplot(data2plot, aes(x=time)) +
#     geom_point(aes(y = log(I), color = "Observed")) +
#     geom_line(aes(y = log(Imod), color = "SEIRAH")) +
#     theme_classic() +
#     ylab("Number of incident cases") +
#     scale_color_manual("", values=c("black", "blue"))}
#   
#   if(type==2){p<-ggplot(data2plot, aes(x=time)) +
#           geom_point(aes(y = H, color = "Observed")) +
#           geom_line(aes(y = Hmod, color = "SEIRAH")) +
#           theme_classic() +
#           ylab("Number of hospitalization") +
#           scale_color_manual("", values=c("black", "blue"))}
#   return(p)
#   
# }

#' Plotting method for a list of SEIRAH fit objects
#'
#' @import ggplot2
#'
#' @export
#'
#'
plot_list.seirah_estim <- function(x, only_observed_dates = TRUE,
                                   compartments = c("S", "E", "I", "R", "A", "H")){



  if(is.null(names(x))){
    names(x) <- seq(from = 1, to = length(x), by = 1)
  }

  ldf <- lapply(names(x), function(r){
    l <- x[[r]]
    temp <- as.data.frame(l$solution)
    temp$maille_code <- r
    if(!is.null(l$data)){
      date_start <- l$data$date[1]
    }else{
      date_start <- 0
    }
    temp$date <- date_start + temp$time
    return(temp)
  })
  all_preds <- do.call(rbind.data.frame, ldf)

  ldf_obs <- lapply(x, "[[", "data")
  all_data <- do.call(rbind.data.frame, ldf_obs)


  preds2plot <- reshape2::melt(all_preds, id.vars=c("time", "maille_code", "date"))
  preds2plot <- preds2plot %>%
    filter(variable %in% compartments)
  ymax_pred <- max(preds2plot$value)

  p <- ggplot(preds2plot, aes(x = date))

  if(nrow(all_data)>0){
    ymax_obs <- max(all_data$cas_confirmes_incident)
    p <- p +
      geom_point(data = all_data,
                 aes(y = cas_confirmes_incident, shape="Observed"), color="black") +
      scale_shape_discrete("")

    if(only_observed_dates){
      ymax_pred <- preds2plot %>%
        filter(date <= max(all_data$date)) %>%
        select(value) %>%
        max()
      p <- p +
        xlim(min(all_data$date), max(all_data$date))
    }
    ymax_pred <- max(ymax_pred, ymax_obs)
  }

  p <- p +
    geom_line(aes(y = value, color=variable, group = variable)) +
    theme_classic() +
    ylab("Number") +
    #scale_color_manual(
    scale_color_discrete("Compartment", breaks = c("S", "E", "I", "R", "A", "H"),
                         labels = c("S (susceptible)", "E (exposed)",
                                    "I (Infected)", "R (removed)",
                                    "A (Not-reported)", "H (hospitalized)")) +
    #values=c("black", "orange","blue","green","purple","red")) +
    facet_wrap(~maille_code) +
    ylim(0, ymax_pred)

  return(p)
}
