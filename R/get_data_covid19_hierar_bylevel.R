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
#'opencovid19_consolidated_reg <- get_data_covid19_hierar_bylevel()
#'#save(opencovid19_consolidated_reg, file = "data/opencovid19_consolidated_reg.RData")
#'
#'# departements
#'get_data_covid19_bylevel(level = "departement", metropole=FALSE)
#'
#'

get_data_covid19_hierar_bylevel <- function(level ="region",
                                     sources_hierarchy = c("sante-publique-france", "ministere-sante", "agences-regionales-sante", "opencovid19-fr"),
                                     date_start = NULL,
                                     date_end = NULL,
                                     update_from_source = FALSE,
                                     epidemic_start = TRUE,
                                     metropole=TRUE){

  stopifnot(level %in%  c("region", "departement"))

  res <- list()

  if(update_from_source){
    opencovid19_FR <- read.csv("https://github.com/opencovid19-fr/data/raw/master/dist/chiffres-cles.csv")
    #save(alldata, file="data/covid19fr_chiffres-cles.RData")
  }else{
    data("opencovid19_FR")
  }

  all_mailles_cd <- opencovid19_FR %>%
    filter(granularite == level) %>%
    distinct(maille_code) %>%
    pull() %>%
    as.character()

  if(metropole){
    if(level == "region"){
      all_mailles_cd <- intersect(all_mailles_cd,
                                  c("REG-11", "REG-75", "REG-84", "REG-27", "REG-32", "REG-44",
                                    "REG-24", "REG-28", "REG-52", "REG-53", "REG-76", "REG-93", "REG-94")
      )
    }else{
      warning("metropole = TRUE not supported yet at department level")
    }
  }

  for(m in all_mailles_cd){
    res[[m]] <- get_data_covid19_hierarchical(maille_cd = m,
                                              sources_hierarchy = sources_hierarchy,
                                              date_start = date_start,
                                              date_end = date_end,
                                              update_from_source = update_from_source,
                                              epidemic_start = epidemic_start)
  }

  return(res)

}







