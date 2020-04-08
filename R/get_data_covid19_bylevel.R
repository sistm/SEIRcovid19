#' Get data from opencovid19-fr for all french region or dpt
#'
#'@param maille_cd a character sting indicating the code
#'
#'@param source3 a character string either \code{"SPF"}
#'or \code{"ARS"}
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
#'# regions
#'temp <- get_data_covid19_bylevel()
#'
#'# departements
#'get_data_covid19_bylevel(level = "region", source3="ARS")
#'
#'#data_reg_consolide <- get_data_covid19_bylevel()
#'#for(x in names(data_reg_consolide)){
#'#  data_reg_consolide[[x]] <- data_reg_consolide[[x]] %>%
#'#    select(-deces_incident, -source_type)
#'#  temp <- get_data_covidé19(maille_cd = x, source_ch="opencovid19-fr",
#'#                            epidemic_start  =FALSE)
#'#  if(!is.null(temp)){
#'#  temp <- temp %>%
#'#    select(date, maille_code, deces_incident)
#'#  data_reg_consolide[[x]] <- left_join(data_reg_consolide[[x]], temp)
#'#  }else{
#'#    data_reg_consolide[[x]] <- NULL
#'#  }
#'#}

get_data_covid19_bylevel <- function(level ="region",
                                     source3 = "SPF",
                                     date_start = NULL,
                                     date_end = NULL,
                                     update_from_source = FALSE,
                                     epidemic_start = TRUE){

  stopifnot(level %in%  c("region", "departement"))
  stopifnot(source3 %in% c("SPF", "ARS", "OCF"))

  res <- list()

  if(update_from_source){
    opencovid19_FR <- read.csv("https://github.com/opencovid19-fr/data/raw/master/dist/chiffres-cles.csv")
    #save(alldata, file="data/covid19fr_chiffres-cles.RData")
  }else{
    data("opencovid19_FR")
  }

  s <- switch(source3,
              "SPF" = "sante-publique-france",
              "ARS" = "agences-regionales-sante",
              "OCF" = "opencovid19-fr")

  all_mailles_cd <- opencovid19_FR %>%
    filter(granularite == level, source_type == s) %>%
    distinct(maille_code) %>%
    pull() %>%
    as.character()

  for(m in all_mailles_cd){
    res[[m]] <- get_data_covid19(maille_cd = m,
                                 source_ch = s,
                                 date_start = date_start,
                                 date_end = date_end,
                                 update_from_source = update_from_source,
                                 epidemic_start = epidemic_start)
  }


  return(res)

}




