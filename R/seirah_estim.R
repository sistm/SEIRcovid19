#' Fitting a SEIRAH by OLS
#'
#' @export
#'
#' @importFrom stats optim
#'
#' @examples
#' data_FRA <- get_data_covid19(maille_cd = "FRA",
#'                              source_ch = "sante-publique-france")
#' fit_FRA <- seirah_estim(binit = c(log(1.75), log(0.41)),
#'                   data = data_FRA)
#' plot(fit_FRA)
#'
#' data_GE <- get_data_covid19(maille_cd = "REG-44",
#'                            source_ch = "agences-regionales-sante")
#' fit_GE <- seirah_estim(binit = c(log(1.75), log(0.41)),
#'                   data=data_GE, popSize = 5518000, dailyMove = 0.1*5518000)
#' plot(fit_GE)
#'
#'data_NA <- get_data_covid19(maille_cd = "REG-75",
#'                            source_ch = "agences-regionales-sante")
#'data_NA <- data_NA[-c(1:40), ]
#'data_NA$day <- data_NA$day-40
#'fit_NA <- seirah_estim(binit = c(log(1.75), log(0.41)),
#'                         data = data_NA,
#'                         popSize = 5987000, dailyMove = 0.1*5987000)
#' plot(fit_NA)
seirah_estim <- function(binit, data,
                         alpha=1,De=5.2,Di=2.3,Dq=10,Dh=30,
                         popSize=65000000, dailyMove=0.1*popSize,
                         verbose = TRUE){

  param_optimal <- optim(binit,
                       fn = seirah_ols,
                       data = data,
                       popSize = popSize,
                       dailyMove = dailyMove,
                       verbose = verbose)


  # Optimal solution
  transmission <- exp(param_optimal$par[1])
  ascertainment <- exp(param_optimal$par[2])
  E0 <- data[1, "cas_confirmes_incident"]*2 # Twice the number of cases
  I0 <- data[1, "cas_confirmes_incident"] # Numbers of cases
  R0 <- 0 #(0 ref)
  A0 <- I0 # A=I
  H0 <- I0*0.50 #all at the begining H=50%I
  S0 <- popSize - E0 - I0 - A0 - H0 - R0 #N-E-I-A-H-R
  init <- c(S0, E0, I0, R0, A0, H0)
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
                "S0" = S0,
                "E0" = E0,
                "I0" = I0,
                "R0" = R0,
                "A0" = A0,
                "H0" = H0
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
