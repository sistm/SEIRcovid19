#'Plotting results from SEIRAH solve
#'
#'@param x an object of class \code{seirah_solve}
#'
#'@importFrom reshape2 melt
#'
#'@export
plot.seirah_solve <- function(x, chiffres = NULL, split=TRUE){

  data2plot <- reshape2::melt(x, id.vars = "time", variable.name="Compartment")
  p <- ggplot(data2plot, aes(x = time, y=value)) +
    geom_line(aes(colour=Compartment))

  if(split){
    p <- p + facet_wrap(~Compartment, scales = "free_y") + xlim(0, NA)
  }

  p <- p +theme_bw()

  p
}

#'Plotting results from SEIDRAH solve
#'
#'@param x an object of class \code{seirah_solve}
#'
#'@importFrom reshape2 melt
#'
#'@export
plot.seidrah_solve <- function(x, chiffres = NULL, split=TRUE){
  plot.seirah_solve(x, chiffres = NULL, split=TRUE)
}

