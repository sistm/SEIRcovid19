#' SEIRAH solve function
#'
#' @importFrom deSolve lsoda
#'
#' @examples
#' # Set parameter values
#' b<-1
#' r<-1
#' alpha<-1
#' De<-5.2
#' Di<-2.3
#' Dq<-2
#' Dh<-30
#' popSize<-10000000
#' dailyMove<-0
#'
#' init<-c(9999467, 346, 80, 0, 80, 27)
#' t<-seq(0,365)
#'par <- c(1.75, 0.41, alpha, De, Di, Dq, Dh, popSize, dailyMove,30,30,0,3000000)
#'
#' # Solve system using lsoda
#' sol <- seirah_solve(init, t, par)
#' plot(sol)
#'
#' # Plot solution
#' plot(sol$time,sol$I,ylim=c(0,100))
#' @export



# seirah_solve <- function(init, t, par){
#
#   solution <- deSolve::ode(init, t, seirah_ode, par)
#   colnames(solution) <- c("time", "S", "E", "I", "R", "A", "H")
#   class(solution) <- c("seirah_solve", "deSolve", "matrix" )
#
#   return(solution)
# }

 seirah_solve <- function(init, t, par,pred=TRUE){
   solution <- deSolve::ode(init, c(0,t[2]), seirah_ode, par)
   result<-as.data.frame(solution)
   #print(result)
   if(pred){
      for(comp in 1:6){
     if(result[which(result[,"time"]==t[2]),comp]<1)result[which(result[,"time"]==t[2]),comp]=0
   }
   }
   for (i in 3:length(t)){
     # print(i)
     temp<- deSolve::ode(as.numeric(result[which(result[,"time"]==t[i-1]),2:7]), c(t[i-1],t[i]), seirah_ode, par)
     if(pred){
        for(comp in 1:6){
       if(temp[which(temp[,"time"]==t[i]),comp]<1)temp[which(temp[,"time"]==t[i]),comp]=0
        }
     }
     result<-rbind(result,temp[2,])
   }
   solution <- as.data.frame(result)
   #print(str(solution))
   colnames(solution) <- c("time", "S", "E", "I", "R", "A", "H")
   class(solution) <- c("seirah_solve", "deSolve", "data.frame" )
   #print(solution)
   return(solution)
 }

