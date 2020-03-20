#'Plotting results from SEIRAH
#'
#'@param x an object of class \code{seirah_solve}
#'
#'@export
plot.seirah_solve <- function(x, locator.legend=F){

  ylimmax <- max(rbind(x[,4], x[,5], x[,6], x[,7])) + 1

  plot(x[, "time"], x[, "I"],type="l",col="blue",ylim=c(0,ylimmax),ylab="Proportion")
  lines(x[, "time"], x[, "A"],col="green")
  lines(x[, "time"], x[, "H"],col="orange")
  lines(x[, "time"], x[, "R"],col="red")

  if(locator.legend){
    legend(locator(1), legend=c("Infectious","Not-reported","Hospitalized","Recovered"), col=c("blue","orange","red","green"), lty=1, cex=0.8)
  }else{
    legend(1, 0.9*ylimmax, legend=c("Infectious","Not-reported","Hospitalized","Recovered"), col=c("blue","orange","red","green"), lty=1, cex=0.8)
  }

}
