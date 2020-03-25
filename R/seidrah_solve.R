#' SEIRAH solve function
#'
#' @importFrom deSolve lsoda
#'
#' @examples
#' # Set parameter values
#' b<-0
#' r<-1
#' alpha<-1
#' De<-5.2
#' Di<-2.3
#' Dq<-2
#' Dh<-30
#' popSize<-10000000
#' dailyMove<-0
#'
#' init <- c(9999467, 346, 80, 0, 0, 80, 27)
#' t<-seq(0,365)
#' par <- c(1, 0.7, 0.01, alpha, De, Di, Dq, Dh, popSize, dailyMove, 30,30,0,3000000)
#'
#' # Solve system using lsoda
#' sol <- seidrah_solve(init, t, par)
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



 seidrah_solve <- function(init, t, par){

   ncomp <- 7
   stopifnot(length(init) == ncomp)

   solution <- deSolve::ode(init, c(0, t[2]), seidrah_ode, par)
   result <- as.data.frame(solution)
   #print(result)

   for(comp in 1:ncomp){
     if(result[which(result[,"time"]==t[2]), comp] < 1){
        result[which(result[,"time"]==t[2]), comp] <- 0 #enforce 0 infectious
     }
   }

   for (i in 3:length(t)){
     #print(i)
     temp <- deSolve::ode(as.numeric(result[which(result[,"time"]==t[i-1]), 2:(ncomp + 1)]),
                         c(t[i-1],t[i]), seidrah_ode, par)
     for(comp in 1:ncomp){
       if(temp[which(temp[, "time"]==t[i]), comp] < 1){
          temp[which(temp[, "time"]==t[i]), comp] <- 0#enforce 0 infectious
       }
     }
     result<-rbind(result,temp[2,])
   }
   solution <- as.data.frame(result)
   #print(str(solution))
   colnames(solution) <- c("time", "S", "E", "I", "D", "R", "A", "H")
   class(solution) <- c("seidrah_solve", "deSolve", "data.frame" )
   #print(solution)
   return(solution)
 }

