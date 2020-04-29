#' SetConfidenceInverval generic
#'
#' @param obj Object to set
#' @param MonteCarloSolution monte carlo resultat of simulation
#' @param time time vector of the simulation
#' @export
#' 

SetConfidenceInverval <- function(obj,MonteCarloSolution,time)
{
  UseMethod("SetConfidenceInverval",obj)
}

SetConfidenceInverval.default <- function(obj,MonteCarloSolution,time)
{
  print("No method implemented for this class")
  return(obj)
}

#' @describeIn Calculate IC and set it for an object of class \code{OdeSystem} after MonteCarlo Simultion
SetConfidenceInverval.OdeSystem <- function(ode,MonteCarloSolution,time){
  # Prepare dataframe Result format
  df <- data.frame(matrix(ncol = 1+length(ode$ModelName)*2, nrow = length(time)))
  dfnames<-c("time")
  for (i in 1:length(ode$ModelName)){
    dfnames<-c(dfnames,paste(ode$ModelName[i],"_min",sep=""),paste(ode$ModelName[i],"_max",sep=""))
  }
  colnames(df) <- dfnames
  # Store time
  df$time<-time
  # Store proportion info
  for (i in 1:length(ode$ModelName)){
    df[,(i*2):(i*2+1)] <- t(rbind(apply(sapply(MonteCarloSolution, "[[", ode$ModelName[i]), 1, FUN=quantile, probs = c(0.025, 0.975),na.rm = TRUE)))
  }
  # Init dataframe ICmin and ICmax
  odemin <- data.frame(matrix(ncol = length(ode$ModelName), nrow = length(time)))
  odemax <- data.frame(matrix(ncol = length(ode$ModelName), nrow = length(time)))
  names(odemin)<-ode$ModelName
  names(odemax)<-ode$ModelName
  # Compute ICmin and ICmax
  for (i in 1:length(ode$ModelName)){
    odemin[,i]<-pmax(0,df[,((i)*2)]-1.96*sqrt(pmax(0,ode$solution[,i+1])))
    odemax[,i]<-pmax(0,df[,i*2+1]+1.96*sqrt(pmax(0,ode$solution[,i+1])))
  }
  # Store  the result in the class
  ode$ICmin<-odemin
  ode$ICmax<-odemax
  return(ode)
}