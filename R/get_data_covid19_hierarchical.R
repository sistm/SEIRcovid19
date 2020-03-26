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
#'data_FR <- get_data_covid19_hierarchical(maille_cd = "FRA")
#'View(data_FR)
#'
#'data_NA <- get_data_covid19_hierarchical(maille_cd = "REG-75")
#'View(data_NA)
#'
get_data_covid19_hierarchical <- function(maille_cd = "FRA",
                                          sources_hierarchy = c("sante-publique-france", "ministere-sante", "agences-regionales-sante", "opencovid19-fr"),
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

  data_geo <- opencovid19_FR %>%
    dplyr::filter(maille_code == maille_cd)
  if(nrow(data_geo)<1){
    stop("No data for the geographical code '", maille_cd, "'")
  }

  data_geo$date <- lubridate::as_date(data_geo$date)
  data_geo <- data_geo %>%
    group_by(date)

  data_consolidated <- data_geo %>%
    select(date, maille_code, maille_nom) %>%
    group_by(date) %>%
    distinct() %>%
    arrange(date) %>%
    mutate(cas_confirmes = NA,
           deces = NA,
           reanimation = NA,
           hospitalises = NA,
           gueris = NA)
  for(r in 1:nrow(data_consolidated)){
    d <- data_consolidated$date[r]
    i <- 1
    while(is.na(data_consolidated$cas_confirmes[r]) & i <= length(sources_hierarchy)){
      temp <- data_geo %>%
        filter(source_type == sources_hierarchy[i], date == d) %>%
        summarise_at("cas_confirmes", mean) %>%
        pull(cas_confirmes)
      if(length(temp)>0){
        data_consolidated$cas_confirmes[r] <- temp
      }
      i <- i + 1
    }

    i <- 1
    while(is.na(data_consolidated$deces[r]) & i <= length(sources_hierarchy)){
      temp <- data_geo %>%
        filter(source_type == sources_hierarchy[i], date == d) %>%
        summarise_at("deces", mean) %>%
        pull(deces)
      if(length(temp)>0){
        data_consolidated$deces[r] <- temp
      }
      i <- i + 1
    }

    i <- 1
    while(is.na(data_consolidated$reanimation[r]) & i <= length(sources_hierarchy)){
      temp <- data_geo %>%
        filter(source_type == sources_hierarchy[i], date == d) %>%
        summarise_at("reanimation", mean) %>%
        pull(reanimation)
      if(length(temp)>0){
        data_consolidated$reanimation[r] <- temp
      }
      i <- i + 1
    }

    i <- 1
    while(is.na(data_consolidated$hospitalises[r]) & i <= length(sources_hierarchy)){
      temp <- data_geo %>%
        filter(source_type == sources_hierarchy[i], date == d) %>%
        summarise_at("hospitalises", mean) %>%
        pull(hospitalises)
      if(length(temp)>0){
        data_consolidated$hospitalises[r] <- temp
      }
      i <- i + 1
    }

    i <- 1
    while(is.na(data_consolidated$gueris[r]) & i <= length(sources_hierarchy)){
      temp <- data_geo %>%
        filter(source_type == sources_hierarchy[i], date == d) %>%
        summarise_at("gueris", mean) %>%
        pull(gueris)
      if(length(temp)>0){
        data_consolidated$gueris[r] <- temp
      }
      i <- i + 1
    }
  }


  data_consolidated$I <- pmax(data_consolidated$cas_confirmes -
    dplyr::lag(data_consolidated$cas_confirmes, default = 0), 0)

  data_consolidated$deces <- cummax(tidyr::replace_na(data_consolidated$deces, 0))
  data_consolidated$D <- data_consolidated$deces -
    dplyr::lag(data_consolidated$deces, default = 0)

  d_rea_first <- data_consolidated %>%
    dplyr::filter(!is.na(reanimation)) %>%
    ungroup() %>%
    top_n(-1, date) %>%
    pull(date)
  data_consolidated[data_consolidated$date < d_rea_first, "reanimation"] <- 0
  data_consolidated$U <- pmax(data_consolidated$reanimation -
    dplyr::lag(data_consolidated$reanimation, default = 0), 0)

  d_hospit_first <- data_consolidated %>%
    dplyr::filter(!is.na(hospitalises)) %>%
    ungroup() %>%
    top_n(-1, date) %>%
    pull(date)
  data_consolidated[data_consolidated$date < d_hospit_first, "hospitalises"] <- 0
  data_consolidated$H <- pmax(data_consolidated$hospitalises -
    dplyr::lag(data_consolidated$hospitalises, default = 0), 0)

  data_consolidated$gueris <- cummax(tidyr::replace_na(data_consolidated$gueris, 0))
  data_consolidated$R <- data_consolidated$gueris -
    dplyr::lag(data_consolidated$gueris, default = 0)

  if(epidemic_start){
    epidemic_start_date <- data_consolidated %>%
      ungroup() %>%
      arrange(date) %>%
      filter(I > 0 & lead(I)>0 & lead(I, n = 2)>0 & lead(I, n = 3)>0) %>%
      top_n(-1, date) %>%
      pull(date)
    data_consolidated2 <- data_consolidated %>%
      arrange(date) %>%
      filter(date >= epidemic_start_date)
  }else{
    data_consolidated2 <- data_consolidated
  }

  if(!is.null(date_start)){
    data_consolidated2 <- data_consolidated2 %>%
      filter(date >= date_start)
  }
  if(!is.null(date_end)){
    data_consolidated2 <- data_consolidated2 %>%
      filter(date <= date_end)
  }

  out_data <- data_consolidated2 %>%
    ungroup() %>%
    mutate(day = seq(from=0, to =n()-1, by=1)) %>%
    select(c(date, maille_code, maille_nom, day, I, H, D, U, R))

  return(out_data)
}
