#' Scrap and format data from opencovid19-fr
#'
#'@param maille_cd a character sting indicating the code
#'
#'@param source_ch a character string either \code{"sante-publique-france"}
#'or \code{"agences-regionales-sante"} or \code{"ministere-sante"}.
#'Other sources are available but not so reliable...
#'
#'
#'@param epidemic_start a logical indicating wehter to left-truncate data at the
#'start of the epidemics. The start of the epidemics is the first date
#'
#'
#' @import dplyr
#' @importFrom lubridate as_date
#' @importFrom tidyr replace_na
#'
#'
#'@export
#'
#'@examples
#'
#'temp <- get_data_covid19_cumul_bylevel(level_geo = "departement")
#'write.table(temp, file = "data/SantePubliqueFranceData_fromOpencovid19FR_20200503.tsv",
#'            sep="\t", row.names=FALSE)

get_data_covid19_cumul_bylevel <- function(level_geo = c("region", "departement"),
                                           source_ch = "sante-publique-france-data",
                                           update_from_source_url = TRUE
                             ){

  if(update_from_source_url){
    opencovid19_FR <- read.csv("https://github.com/opencovid19-fr/data/raw/master/dist/chiffres-cles.csv")
    #save(opencovid19_FR, file="data/opencovid19_FR.RData")
  }else{
    data("opencovid19_FR")
  }

  data_filtered <- opencovid19_FR %>%
    dplyr::filter(granularite == level_geo)

  stopifnot(nrow(data_filtered)>0)

  data_filtered$date <- lubridate::as_date(data_filtered$date)

  out_data <- data_filtered %>%
    group_by(maille_code, maille_nom, date, source_nom) %>%
    summarise_at(c("deces", "reanimation", "hospitalises", "gueris"), mean) %>%
    rename(source = source_nom)

  return(as.data.frame(out_data))
}
