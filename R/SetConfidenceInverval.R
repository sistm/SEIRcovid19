#' SetConfidenceInverval generic
#'
#' @param obj Object to set
#' @param MonteCarloSolution monte carlo resultat of simulation
#' @param time time vector of the simulation
#' @export
SetConfidenceInverval <- function(obj,MonteCarloSolution,time,TimeDependantParameter,IsLongTerm=FALSE)
{
  UseMethod("SetConfidenceInverval",obj)
}

#' @export
SetConfidenceInverval.default <- function(obj,MonteCarloSolution,time,TimeDependantParameter,IsLongTerm=FALSE)
{
  print("No method implemented for this class")
  return(obj)
}

#' @describeIn SetConfidenceInverval Calculate IC and set it for an object of class \code{OdeSystem} after MonteCarlo Simultion
#' @export
SetConfidenceInverval.OdeSystem <- function(ode,MonteCarloSolution,time,TimeDependantParameter,IsLongTerm=FALSE){
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
    if (IsLongTerm){
      odemin[,i]<-pmax(0,df[,((i)*2)]-1.96*sqrt(pmax(0,ode$LongTerm[,i+1])))
      odemax[,i]<-pmax(0,df[,i*2+1]+1.96*sqrt(pmax(0,ode$LongTerm[,i+1])))
    }else{
      odemin[,i]<-pmax(0,df[,((i)*2)]-1.96*sqrt(pmax(0,ode$solution[,i+1])))
      odemax[,i]<-pmax(0,df[,i*2+1]+1.96*sqrt(pmax(0,ode$solution[,i+1])))
    }
    
  }
  # Store  the result in the class
  if (IsLongTerm){ #only store compartiment min max
    ode$LongTermMin<-odemin
    ode$LongTermMax<-odemax
    #Prepare dataframe for TimeDependantParameter
    if (length(TimeDependantParameter)>0){
      df <- data.frame(matrix(ncol = length(TimeDependantParameter)*2, nrow = length(time)))
      dfnames<-c()
      for (i in 1:length(TimeDependantParameter)){
        dfnames<-c(dfnames,paste(TimeDependantParameter[[i]],"_min",sep=""),paste(TimeDependantParameter[[i]],"_max",sep=""))
      }
      colnames(df) <- dfnames
      for (i in 1:length(TimeDependantParameter)){
        df[,((i-1)*2+1):(i*2)] <- t(rbind(apply(sapply(MonteCarloSolution, "[[", TimeDependantParameter[i]), 1, FUN=quantile, probs = c(0.025, 0.975),na.rm = TRUE)))
      }
      # Init dataframe ICmin and ICmax
      ParamICmin <- data.frame(matrix(ncol = length(TimeDependantParameter), nrow = length(time)))
      ParamICmax <- data.frame(matrix(ncol = length(TimeDependantParameter), nrow = length(time)))
      names(ParamICmin)<-TimeDependantParameter
      names(ParamICmax)<-TimeDependantParameter
      # Compute ICmin and ICmax
      for (i in 1:length(TimeDependantParameter)){
        ParamICmin[,i]<-pmax(0,df[,(i-1)*2+1])
        ParamICmax[,i]<-pmax(0,df[,i*2])
      }

      ode$LongParamICmin<-ParamICmin
      ode$LongParamICmax<-ParamICmax
    }
  }else{
    ode$ICmin<-odemin
    ode$ICmax<-odemax
    
    #Prepare dataframe for TimeDependantParameter
    if (length(TimeDependantParameter)>0){
      df <- data.frame(matrix(ncol = length(TimeDependantParameter)*2, nrow = length(time)))
      dfnames<-c()
      for (i in 1:length(TimeDependantParameter)){
        dfnames<-c(dfnames,paste(TimeDependantParameter[[i]],"_min",sep=""),paste(TimeDependantParameter[[i]],"_max",sep=""))
      }
      colnames(df) <- dfnames
      for (i in 1:length(TimeDependantParameter)){
        df[,((i-1)*2+1):(i*2)] <- t(rbind(apply(sapply(MonteCarloSolution, "[[", TimeDependantParameter[i]), 1, FUN=quantile, probs = c(0.025, 0.975),na.rm = TRUE)))
      }
      # Init dataframe ICmin and ICmax
      ParamICmin <- data.frame(matrix(ncol = length(TimeDependantParameter), nrow = length(time)))
      ParamICmax <- data.frame(matrix(ncol = length(TimeDependantParameter), nrow = length(time)))
      names(ParamICmin)<-TimeDependantParameter
      names(ParamICmax)<-TimeDependantParameter
      # Compute ICmin and ICmax
      for (i in 1:length(TimeDependantParameter)){
        ParamICmin[,i]<-pmax(0,df[,(i-1)*2+1])
        ParamICmax[,i]<-pmax(0,df[,i*2])
      }
      ode$ParamICmin<-ParamICmin
      ode$ParamICmax<-ParamICmax
    }
  }
  
  return(ode)
}
