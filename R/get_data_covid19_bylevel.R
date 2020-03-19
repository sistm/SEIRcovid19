#' Get data from opencovid19-fr for all french region or dpt
#'
#'@param maille_cd a character sting indicating the code
#'
#'@param source_ch a character string either \code{"Santé publique France"}
#'or \code{"ARS XXX-XXX"} (where 'XXX-XXX' must be replaced by the region name).
#'Other sources are available but not so reliable...
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
#'get_data_covid19(maille_cd = "DPT-33",
#'                 source_ch = "ARS Nouvelle-Aquitaine")
#'
#'get_data_covid19(maille_cd = "REG-75",
#'                 source_ch = "ARS Nouvelle-Aquitaine")
#'
#'get_data_covid19(maille_cd = "REG-44",
#'                 source_ch = "ARS Grand-Est")
#'
#'get_data_covid19(maille_cd = "FRA",
#'                 source_ch = "Santé publique France")
#'
#'get_data_covid19(maille_cd = "WORLD",
#'                 source_ch = "Santé publique France")
get_data_covid19_bylevel <- function(level ="region",
                                     source3 = "SPF",
                                     date_start = NULL,
                                     date_end = NULL,
                                     update_from_source = FALSE){

  stopifnot(level %in%  c("region", "departement"))
  stopifnot(source %in% c("SPF", "ARS"))

  res <- list()

  if(update_from_source){
    alldata <- read.csv("https://github.com/opencovid19-fr/data/raw/master/dist/chiffres-cles.csv")
    #save(alldata, file="data/covid19fr_chiffres-cles.RData")
  }else{
    data("covid19fr_chiffres-cles")
  }

  s <- switch(source3,
                      "SPF" = "sante-publique-france",
                      "ARS" = "agences-regionales-sante")

  all_mailles_cd <- alldata %>%
    filter(granularite == level) %>%
    distinct(maille_code) %>%
    pull() %>%
    as.character()

  for(m in all_mailles_cd){
    res[[m]] <- get_data_covid19(maille_cd = m,
                                 source_ch = s,
                                 date_start = date_start,
                                 date_end = date_end,
                                 update_from_source = update_from_source)
  }


  return(res)

}




