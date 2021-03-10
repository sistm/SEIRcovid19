#' weather_data_from_NOAA
#'
#' @description Provide the weather data from NOAA. User should provide either 'Regions_stations_pop' or 'Dept_stations_pop' to the Regions_or_Dept_stations_pop parameter which are datasets extracted from QGIS available in SEIRcovid19 package.
#'
#' @param Regions_or_Dept_stations_pop The region or departement QGIS dataset available in SEIRcovid19 package.
#' @param years Years of interest. Default is 'c(2020, 2021)'.
#' @param n.cores Number of cores to download NOAA meteorological dataset.
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
#' @export
#' 
#' @import dplyr
#' @importFrom tidyr replace_na
#' @importFrom worldmet importNOAA
#' @importFrom lubridate as_date
#' @import hablar
#'
#' @examples
#' \dontrun{
#' 
#' weather_data_reg <- weather_data_from_NOAA(
#'      Regions_or_Dept_stations_pop = SEIRcovid19::Regions_stations_pop
#'      )
#' 
#' 
#' weather_data_dep <- weather_data_from_NOAA(
#'      Regions_or_Dept_stations_pop = SEIRcovid19::Dept_stations_pop
#'      )
#' }
#' 
weather_data_from_NOAA <- function(Regions_or_Dept_stations_pop,
                                   years = c(2020, 2021),
                                   n.cores = 1){

  stations_code <- Regions_or_Dept_stations_pop$code %>% unique
  
  Regions_or_Dept_stations_pop2 <- Regions_or_Dept_stations_pop %>%
    mutate_all(function(x){tidyr::replace_na(x,replace = 0)}) %>%
    mutate(total_pop_buff = fra_pop_su+glp_pop_su+mtq_pop_su+myt_pop_su+reu_pop_su) %>%
    select(code_insee,
           nom,
           station,
           code,
           total_pop_buff)
  
  # 2 Weather data
  ## 2.1 Import NOAA data
  
  ### last update 2020-03-03 ###
  
  # get NOAA weather data :
  weather_data <- worldmet::importNOAA(code = stations_code,
                                       year = years,
                                       hourly = TRUE,
                                       quiet = FALSE,
                                       n.cores = n.cores) %>%
    mutate(date_day = lubridate::as_date(date),.after = date) %>%
    dplyr::select(-elev, -latitude, -longitude) %>%
    mutate(code=as.character(code))
  
  ## 2.2 Daily summary per station
  
  # Identify days of observation for each weather station with less than
  # 5 missing hours ("complete day")
  weather_complete_days <- weather_data %>%
    mutate(date_day = lubridate::as_date(date), obs = 1) %>%
    group_by(code, date_day) %>%
    mutate(total_empties = sum(is.na(air_temp) & is.na(RH)),
           count_obs = sum(obs)) %>%
    distinct(code,date_day, .keep_all = TRUE) %>%
    dplyr::select(code, date_day, total_empties, count_obs) %>%
    mutate(complete_day = ifelse(count_obs-total_empties > 19, 1, 0))
  
  # Calculate daily summaries by station
  daily_station_weather <- weather_data %>%
    mutate(date_day=lubridate::as_date(date),
           AH=6.112*exp(17.67*air_temp/(air_temp+243.5))*RH*2.1674/(273.15+air_temp),
           IPTCC = 100*exp(-0.5*((air_temp-7.5)^2/196+(RH-75)^2/625+(AH-6)^2/2.89))) %>%
    group_by(code, station, date_day) %>%
    summarise(stat_t.mean=mean_(air_temp, ignore_na =TRUE),
              stat_t.pmin=min_(air_temp, ignore_na = TRUE),
              stat_t.pmax=max_(air_temp, ignore_na = TRUE),
              stat_precip=sum_(precip, ignore_na=TRUE),
              stat_RH.mean=mean_(RH, ignore_na=TRUE),
              stat_RH.min=min_(RH, ignore_na=TRUE),
              stat_RH.max=max_(RH, ignore_na=TRUE),
              stat_AH.mean=mean_(AH, ignore_na=TRUE),
              stat_AH.min=min_(AH, ignore_na=TRUE),
              stat_AH.max=max_(AH, ignore_na=TRUE),
              stat_IPTCC.mean=mean_(IPTCC, ignore_na=TRUE),
              stat_IPTCC.min=min_(IPTCC, ignore_na=TRUE),
              stat_IPTCC.max=max_(IPTCC, ignore_na=TRUE),
              stat_ws.mean=mean_(ws, ignore_na=TRUE),
              stat_dewpoint.mean=mean_(dew_point, ignore_na=TRUE),
              stat_dewpoint.min=min_(dew_point, ignore_na=TRUE),
              stat_dewpoint.max=max_(dew_point, ignore_na=TRUE)
    ) %>%
    
    left_join(weather_complete_days, by = c("code", "date_day"))  %>%
    filter(complete_day==1)
  
  ## 2.3 Daily weather data per region (weighted)
  
  ### List of daily weather data per region or departement
  vecInsee <- Regions_or_Dept_stations_pop2$code_insee %>% unique
  
  weather_data_reg_or_dep <- purrr::map(.x = vecInsee,
                                        .f = ~fun_weighted_daily_obs(data0 = Regions_or_Dept_stations_pop2,
                                                                     which_insee = .,
                                                                     daily_station_weather = daily_station_weather)) %>%
    bind_rows()
  
  return(weather_data_reg_or_dep)
  
}