#' Scrap and format data from opencovid19-fr
#'
#' @import dplyr
#' @importFrom lubridate as_date
#' @importFrom tidyr replace_na
#'
#'
#'
#'@export
#'
#'@examples
#'get_data_covid19(maille = "Grand-Est",
#'                 source = "ARS Grand-Est")
#'
#'get_data_covid19(maille = "France",
#'                 source = "Santé publique France")
get_data_covid19 <- function(maille = "Nouvelle-Aquitaine",
                             source = "ARS Nouvelle-Aquitaine",
                             date_start = NULL,
                             date_end = NULL){

  alldata <- read.csv(file = "https://github.com/opencovid19-fr/data/raw/master/dist/chiffres-cles.csv")

  data_filtered <- alldata %>%
    dplyr::filter(maille_nom == maille, source_nom == source)
  data_filtered$date <- lubridate::as_date(data_filtered$date)

  if(is.null(date_start)){
    date_start <- min(data_filtered$date)
  }else{
    date_start <- max(date_start, min(data_filtered$date))
  }

  if(is.null(date_end)){
    date_end <- max(data_filtered$date)
  }else{
    date_end <- min(date_end, max(data_filtered$date))
  }


  data_filtered$cas_confirmes_incident <- data_filtered$cas_confirmes -
    dplyr::lag(data_filtered$cas_confirmes, default = 0)
  data_filtered$deces <- tidyr::replace_na(data_filtered$deces, 0)
  data_filtered$deces_incident <- data_filtered$deces -
    dplyr::lag(data_filtered$deces, default = 0)

  out_data <- data.frame("date" = seq.Date(from = date_start, by = 1, to = date_end),
                         "maille_nom" = maille)
  out_data$day <- as.numeric(difftime(out_data$date, out_data$date[1], units = "day"))
  out_data2 <- left_join(out_data, dplyr::select(data_filtered, date, cas_confirmes_incident, deces_incident),
            by="date")
  out_data3 <- tidyr::replace_na(out_data2, replace =list("cas_confirmes_incident" = 0,
                                                          "deces_incident"=0))
  return(out_data3)
}
