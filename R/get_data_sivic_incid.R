#' Scrap and format data from SI-VIC on data.gouv from Santé Publique France
#'
#'@param level_geo a character sting indicating
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
#'sivic_incid_20200503 <- get_data_sivic_incid()
#'#save(sivic_incid_20200503, file = "data/sivic_incid_20200503.RData")
#'#write.table(sivic_incid_20200503, file = "data/SantePubliqueFranceData_fromDataGouv_20200503_incid.tsv",
#'#            sep="\t", row.names=FALSE)

get_data_sivic_incid <- function(update_from_source_url = TRUE){

  if(update_from_source_url){
    sivic <- read.delim("https://www.data.gouv.fr/fr/datasets/r/6fadff46-9efd-4c53-942a-54aca783c30c",
                        sep=";")
  }else{
    stop("you have to update from source - no data stored locally")
  }
  colnames(sivic) <- gsub("incid_", "", colnames(sivic))

  out_data <- sivic %>%
    mutate(jour = as.Date(jour)) %>%
    rename(departement = dep, date = jour, hospitalises = hosp, reanimation = rea, gueris = rad, decedes = dc) %>%
    group_by(departement, date) %>%
    arrange(departement, date)

  out_data$departement <- paste0("DEP-", out_data$departement)

  return(as.data.frame(out_data))
}
