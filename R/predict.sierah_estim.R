#' Prediction of Epidemics and ICU utilization
#'
#' @param object an object of class \code{seirah_estim}
#'
#' @examples
#' data_FRA <- get_data_covid19(maille_cd = "FRA",
#'                              source_ch = "sante-publique-france")
#' fit_FRA <- seirah_estim(binit = c(log(1.75), log(0.41)),
#'                         data = data_FRA, verbose = FALSE)
#' pred <- predict(fit_FRA, verbose=TRUE)
#'
#' @export
predict.seirah_estim <- function(object, thresholdICU = 5000,
                                 verbose=TRUE,kICU=0.1){

  sol <- as.data.frame(object$solution)
  sol[,"H"]<-kICU*sol[,"H"]
  date_start <- object$data$date[1]

    Jpic <- sol %>%
    filter(I==max(I)) %>%
    pull(time)
  Dpic <-  date_start + Jpic

  maxlit <- sol %>%
    pull(H) %>%
    max()

  Jmaxlit <- sol %>%
    filter(H == maxlit) %>%
    pull(time)
  Dmaxlit <- date_start + Jmaxlit

  Jdepasselit_temp <- sol %>%
    filter(H > thresholdICU)
    if(nrow(Jdepasselit_temp) > 0){
      Jdepasselit <- Jdepasselit_temp %>%
        pull(time) %>%
        min()
    }else{
      Jdepasselit <- Inf
    }
  Ddepasselit <- date_start + Jdepasselit


  if(verbose){
    message("Transmission: ", object$parameters$transmission)
    message("Ascertainment: ", object$parameters$ascertainment)
    message("----------")
    message("Jour de pic épidemique : ", Jpic)
    message("Date de pic épidemique : ", Dpic)
    message("----------")
    message("Nombre maximum d'hospitalisations : ", round(maxlit))
    message("Jour du pic d'hospitalisation : ", Jmaxlit)
    message("Date du pic d'hospitalisation : ", Dmaxlit)
    message("----------")
    message("Jour de depassement de la capacité de lit d'hospitalisation : ", Jdepasselit)
    message("Date de depassement de la capacité de lit d'hospitalisation : ", Ddepasselit)
    message("----------")
  }

  prediction_seirah <- list("fit" = object,
                            "predictions" = list("Location" = as.character(object$data$maille_code[1]),
                                                 "Jpic" = Jpic,
                                                 "Dpic" = Dpic,
                                                 "maxlit" = maxlit,
                                                 "Jmaxlit" = Jmaxlit,
                                                 "Dmaxlit" = Dmaxlit,
                                                 "Jdepasselit" = Jdepasselit,
                                                 "Ddepasselit" = Ddepasselit)
                            )
  return(prediction_seirah)
}

