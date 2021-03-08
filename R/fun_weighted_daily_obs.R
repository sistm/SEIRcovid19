#' fun_weighted_daily_obs
#'
#' @description Function giving daily weighted average weather data for a given area (admin_id)
#'
#' @param data0 
#' @param which_id 
#'
#' @return
#' @export
#'
#' @examples
fun_weighted_daily_obs <- function(data0,which_id){
  
  list_stations <- data0 %>%
    dplyr::select(admin_id, code, code_insee, total_pop_buff) %>%
    filter(admin_id == which_id) %>%
    mutate(weight_pop= total_pop_buff/sum(total_pop_buff))
  
  code_insee <- unique(list_stations$code_insee)
  
  reg_daily_station_weather <- daily_station_weather %>%
    filter(code %in% list_stations$code) %>%
    dplyr::select(!contains(".min") & !contains(".max")) %>%
    mutate(weight_pop = sapply(code,
                               function(x){list_stations$weight_pop[which(list_stations$code == x)]})) %>%
    mutate_at(.vars = vars(contains("stat_")),.funs = funs(.*weight_pop))  %>%
    group_by(date_day) %>%
    summarise_at(vars(contains("stat_")),.funs = sum) %>%
    mutate(code_insee = code_insee, .before = 1)
  
  
  return(reg_daily_station_weather)
  
}