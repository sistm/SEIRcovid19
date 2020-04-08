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
#'get_data_covid19(maille_cd = "FRA",
#'                 source_ch = "ministere-sante")
#'
#'get_data_covid19(maille_cd = "DEP-33",
#'                 source_ch = "agences-regionales-sante")
#'
#'get_data_covid19(maille_cd = "REG-75",
#'                 source_ch = "agences-regionales-sante")
#'
#'get_data_covid19(maille_cd = "REG-44",
#'                 source_ch = "agences-regionales-sante")
#'
#'get_data_covid19(maille_cd = "FRA",
#'                 source_ch = "sante-publique-france")
#'
#'get_data_covid19(maille_cd = "WORLD",
#'                 source_ch = "sante-publique-france")

get_data_covid19 <- function(maille_cd = "FRA",
                             source_ch = "sante-publique-france",
                             date_start = NULL,
                             date_end = NULL,
                             metropole = FALSE,
                             update_from_source_url = FALSE,
                             epidemic_start = TRUE){

  if(update_from_source_url){
    opencovid19_FR <- read.csv("https://github.com/opencovid19-fr/data/raw/master/dist/chiffres-cles.csv")
    #save(opencovid19_FR, file="data/opencovid19_FR.RData")
  }else{
    data("opencovid19_FR")
  }

  data_filtered <- opencovid19_FR %>%
    dplyr::filter(maille_code == maille_cd, source_type == source_ch)

  stopifnot(nrow(data_filtered)>0)

  data_filtered$date <- lubridate::as_date(data_filtered$date)

  data_filtered2 <- data_filtered %>%
    group_by(date) %>%
    summarise_at(c("cas_confirmes", "deces", "reanimation"), mean)

  data_filtered2$cas_confirmes_incident <- data_filtered2$cas_confirmes -
    dplyr::lag(data_filtered2$cas_confirmes, default = 0)
  data_filtered2$deces <- cummax(tidyr::replace_na(data_filtered2$deces, 0))
  data_filtered2$deces_incident <- data_filtered2$deces -
    dplyr::lag(data_filtered2$deces, default = 0)
  data_filtered2$reanimation_incident <- data_filtered2$reanimation -
    dplyr::lag(data_filtered2$reanimation, default = 0)

  if(epidemic_start){
    epidemic_start_date <- data_filtered2 %>%
      arrange(date) %>%
      filter(cas_confirmes_incident > 0 & lead(cas_confirmes_incident)>0 & lead(cas_confirmes_incident, n = 2)>0 & lead(cas_confirmes_incident, n = 3)>0) %>%
      pull(date)
    data_filtered3 <- data_filtered2 %>%
      filter(date >= epidemic_start_date[1])
  }else{
    data_filtered3 <- data_filtered2
  }

  if(is.null(date_start)){
    date_start <- min(data_filtered3$date)
  }else{
    date_start <- max(date_start, min(data_filtered3$date))
  }

  if(is.null(date_end)){
    date_end <- max(data_filtered3$date)
  }else{
    date_end <- min(date_end, max(data_filtered3$date))
  }

  out_data <- data.frame("date" = seq.Date(from = date_start, by = 1, to = date_end),
                         "maille_code" = maille_cd,
                         "source_type" = source_ch)
  out_data2 <- left_join(out_data, dplyr::select(data_filtered3, date, cas_confirmes_incident, deces_incident, reanimation_incident),
            by="date")
  out_data3 <- tidyr::replace_na(out_data2, replace =list("cas_confirmes_incident" = 0,
                                                          "deces_incident"=0))
  out_data3$maille_code <- as.character(out_data3$maille_code)

  sursaud_covid19_FRA <- sursaud_covid19 %>% #summing at the FRANCE level
    filter(grepl("REG-",maille_code)) %>%
    select(-maille_code) %>%
    group_by(date_de_passage) %>%
    summarise_all(sum, na.rm=TRUE) %>%
    tibble::add_column(maille_code = "FRA", .before = "date_de_passage")

  sursaud_covid19 <- bind_rows(sursaud_covid19, sursaud_covid19_FRA)

 sursaud_covid19_2join <- sursaud_covid19 %>%
   filter(maille_code == maille_cd, date_de_passage > min(out_data3$date)) %>%
   select(maille_code, date_de_passage, nbre_hospit_corona)

 out_data4 <- full_join(out_data3, sursaud_covid19_2join,
                        by = c("date" = "date_de_passage",
                               "maille_code" = "maille_code")) %>%
   arrange(date) %>%
   rename(hospitalisation_incident = nbre_hospit_corona)

 out_data4$day <- as.numeric(difftime(out_data4$date, out_data4$date[1], units = "day"))

  return(out_data4)
}
