#'Plotting results from SEIRAH
#'
#'@param x an object of class \code{seirah_solve}
#'
#'@export
plot.seirah_solve <- function(x){

  par(mfrow=c(2,3))
  plot(x[,"time"],x[,"S"],type="l",xlab="Time",ylab="S",col="#56B4E9")
  plot(x[,"time"],x[,"E"],type="l",xlab="Time",ylab="E",col="#F0E442")
  plot(x[,"time"],x[,"I"],type="l",xlab="Time",ylab="I",col="#E69F00")
  plot(x[,"time"],x[,"R"],type="l",xlab="Time",ylab="R",col="#009E73")
  plot(x[,"time"],x[,"A"],type="l",xlab="Time",ylab="A",col="#CC79A7")
  plot(x[,"time"],x[,"H"],type="l",xlab="Time",ylab="H",col="#D55E00")
  abline(h=chiffres[which(chiffres$goodid==as.character(indivParams[i,1])),"ICUnb"],col="red")
  par(mfrow=c(1,1))

}
