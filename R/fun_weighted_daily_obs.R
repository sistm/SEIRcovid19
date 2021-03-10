#' fun_weighted_daily_obs
#'
#' @description Function giving daily weighted average weather data for a given area (INSEE code)
#'
#' @param data0 A dataframe with the following columns: code_insee (the insee code), nom (the insee area label), station (the meteorological station), code (the station code), total_pop_buff (the population inside the 10km buffer around the station)
#' @param which_insee The insee code evaluated.
#' @param daily_station_weather The meteorological data set with the following columns: code (the station code), date_day (the date), stat_ (multiples functions with the meteorological data collected).
#'
#' @return A dataframe with 9 columns:
#' \itemize{
#'   \item label_insee - The name of the region or the departement.
#'   \item code_insee - The insee code of the departement or the region.
#'   \item date_day - The date.
#'   \item stat_t.mean - The mean temperature over the day..
#'   \item stat_precip - The precipitation over the day.
#'   \item stat_RH.mean - The mean relative humidity over the day.
#'   \item stat_AH.mean - The mean absolute humidity over the day.
#'   \item stat_IPTCC.mean - The mean IPTCC index over the day.
#'   \item stat_ws.mean - The mean wind speed over the day.
#'   \item stat_dewpoint.mean - The mean dew point over the day.
#' }
#'
#' @examples
#' 
#' data0 <- data.frame(code_insee = c(1, 1, 2),
#'                    nom = c("Tatooine", "Tatooine", "Alderaan"),
#'                    station = c("Mos Eisley", "Mos Espa", "Alderaan city"),
#'                    code = c(118, 218, 104),
#'                    total_pop_buff = c(0.6, 0.4, 1.0))
#' daily_station_weather <- data.frame(
#'                code = rep(c(118, 218, 104), 2),
#'                date_day = lubridate::as_date(x = c(rep("2080-01-01", 3),
#'                                                    rep("2080-01-02", 3))),
#'                stat_temp = c(32, 34, 18, 36, 35, 15)
#'                )
#' which_insee <- 1
#' fun_weighted_daily_obs(data0 = data0,
#'                        which_insee = which_insee,
#'                        daily_station_weather = daily_station_weather)
#' 
#' 
fun_weighted_daily_obs <- function(data0, which_insee, daily_station_weather){
  
  list_stations <- data0 %>%
    dplyr::select(nom, code, code_insee, total_pop_buff) %>%
    filter(code_insee == which_insee) %>%
    mutate(weight_pop = total_pop_buff/sum(total_pop_buff))
  
  if(!any(daily_station_weather$code %in% list_stations$code)){
    
    warning(paste0("No data for insee code : ", which_insee))
    return(NULL)
    
  } else{
    
    reg_daily_station_weather <- daily_station_weather %>%
      filter(code %in% list_stations$code) %>%
      dplyr::select(!contains(".min") & !contains(".max")) %>%
      mutate(pop_buff = sapply(code, function(x){list_stations$total_pop_buff[which(list_stations$code == x)]})) %>%
      group_by(date_day) %>%
      summarise(t.mean = weighted.mean(stat_t.mean,pop_buff,na.rm = TRUE),
                precip = weighted.mean(stat_precip,pop_buff,na.rm = TRUE),
                RH.mean = weighted.mean(stat_RH.mean,pop_buff,na.rm = TRUE),
                AH.mean = weighted.mean(stat_AH.mean,pop_buff,na.rm = TRUE),
                IPTCC.mean = weighted.mean(stat_IPTCC.mean,pop_buff,na.rm = TRUE),
                ws.mean = weighted.mean(stat_ws.mean,pop_buff,na.rm = TRUE),
                dewpoint.mean = weighted.mean(stat_dewpoint.mean,pop_buff,na.rm = TRUE)) %>%
      mutate(admin_id = which_id, admin_name = unique(data0$region_name[data0$admin_id == which_id]),
             .before = 1)
    
    return(reg_daily_station_weather)
  }
}