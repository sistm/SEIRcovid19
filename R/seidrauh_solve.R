#' SEIRAH solve function
#'
#' @importFrom deSolve lsoda
#'
#' @examples
#' # Set parameter values
#' b<-1
#' r<-1
#' alpha<-5
#' De<-5.2
#' Di<-2.3
#' Dq<-2
#' Dh<-30
#' popSize<-65000000
#' dailyMove<-0.01*65000000
#' Du=10
#' Mi=10
#' Mu=10
#' Mh=10
#'
#' init <- c(9999467, 346, 80, 0,0, 80, 1, 27)
#' t<-seq(0,365)
#' par <- c(1, 0.7, 0.01, alpha, De, Di, Dq, Dh, popSize, dailyMove, 1000,1000,0,3,Mh,Mi,Mu,Du)
#'
#' # Solve system using lsoda
#' sol <- seidrauh_solve(init, t, par)
#' plot(sol)
#' @export

 seidrauh_solve <- function(init, t, par){

   ncomp <- 8
   stopifnot(length(init) == ncomp)

   solution <- deSolve::ode(init, c(0, t[2]), seidrauh_ode, par)
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
                         c(t[i-1],t[i]), seidrauh_ode, par)
     for(comp in 1:ncomp){
       if(temp[which(temp[, "time"]==t[i]), comp] < 1){
          temp[which(temp[, "time"]==t[i]), comp] <- 0#enforce 0 infectious
       }
     }
     result<-rbind(result,temp[2,])
   }
   solution <- as.data.frame(result)
   #print(str(solution))
   colnames(solution) <- c("time", "S", "E", "I", "D", "R", "A", "U","H")
   class(solution) <- c("seidrauh_solve", "deSolve", "data.frame" )
   #print(solution)
   return(solution)
 }

