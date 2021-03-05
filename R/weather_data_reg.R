#' Weather dataset per French region (2021-03-03)
#'
#'A dataset containing the weather parameters according to
#'National Oceanic and Atmospheric Administration as of 2021-03-03.
#'The variables are as follows:
#'
#' \itemize{
#'   \item admin_id artificial id, 1 per region
#'   \item code_insee insee code, 1 per region
#'   \item date_day Date of meteorological data
#'   \item stat_t.mean Mean temperature over the day
#'   \item stat_precip Precipitation over the day
#'   \item stat_RH.mean Mean relative humidity over the day
#'   \item stat_AH.mean Mean absolute humidity over the day
#'   \item stat_IPTCC.mean mean IPTCC index over the day
#'   \item stat_ws.mean mean wind speed over the day
#'   \item stat_dewpoint.mean Mean dew point ("point de rosée") over the day.
#' }
#' @format A data frame with 5024 rows and 10 variables
#'
#'@examples
#' ##### script generating meteo dataframe
#' \dontrun{
#' # 0.2 Load Packages
#' library(worldmet)
#' library(tidyverse)
#' library(lubridate)
#' 
#' # 0.3 Common variables
#' ### external inputs:
#' country_list=c("FR", "SP", "IT", "SZ","BE","LU","RE","MF","MB","GP","FG")
#' 
#' # FP          FRENCH POLYNESIA
#' # FS          FRENCH SOUTHERN AND ANTARCTIC LANDS
#' # SB          ST. PIERRE AND MIQUELON
#' # NC          NEW CALEDONIA
#' 
#' regions <- data.frame(id = 1:18,
#'                       code_insee = c(11,24,27,28,32,44,52,53,75,76,84,93,94,4,6,2,1,3),
#'                       name =  c("IDF","Centre","BFC","Normandie","HDF","GrandEst","PaysLoire",
#'                                 "Bretagne","NAquitaine","Occitanie" ,"AURA","PACA","Corse",
#'                                 "Reunion","Mayotte","Martinique","Guadeloupe","Guyanne"))
#' # 1 Country file
#' ## 1.1 Create list of stations by region or department
#' 
#' ### last update 2020-03-01 ###
#' 
#' ### Extract list of stations by country
#' 
#' # country_stations=list()
#' #
#' # for (i in c(1:length(country_list))) {
#' #
#' #   country_stations[[i]] <- getMeta(country = country_list[i],plot = FALSE)
#' #
#' # }
#' #
#' # country_station_motherfile=bind_rows(country_stations)
#' 
#' # write.csv(country_station_motherfile, file="country_stations_motherfile.csv")
#' 
#' 
#' ### This CSV has been imported in a GIS software to map stations
#' ### Buffers with a radius of 0.1° (~10km) were created around them
#' ### shapefiles of FR departments and regions were loaded along with population rasters
#' ### French population within buffers was calculated for weigthing
#' ### Buffers within or intersecting with region polygon were then linked to this region
#' 
#' ### loading attributes tables from QGIS resulting layers
#' 
#' # Regions_stations_pop <- read.csv("./shapefiles_regions_dpts/Regions_stations_pop.csv")
#' # Dept_stations_pop <- read.csv("./shapefiles_regions_dpts/departements_stations_pop.csv")
#' Regions_stations_pop <- read.csv("Regions_stations_pop.csv")
#' Dept_stations_pop <- read.csv("departements_stations_pop.csv")
#' 
#' ### Extracting unique stations' codes to request weather data ("importNOAA")
#' 
#' stations_code <- Regions_stations_pop$code %>% unique
#' 
#' ## 1.2 harmonizing ID / names
#' 
#' ### Replacing INSEE id and names by the study id and name
#' ### Removing additional station variables
#' 
#' Regions_stations_pop2 <- Regions_stations_pop %>%
#'   mutate(admin_id = sapply(Regions_stations_pop$code_insee,
#'                            FUN = function(x){which(regions$code_insee == x)}),
#'                            .before = 1) %>%
#'   mutate(region_name = regions$name[admin_id],.after = 1) %>%
#'   dplyr::select(-c(4:6,8:11,13:18)) %>%
#'   mutate_all(function(x){replace_na(x,replace = 0)}) %>%
#'   mutate(total_pop_buff = fra_pop_su+glp_pop_su+mtq_pop_su+myt_pop_su+reu_pop_su)
#' 
#' ### departments ===> à écrire
#' 
#' # 2 Weather data
#' ## 2.1 Import NOAA data
#' 
#' ### last update 2020-03-03 ###
#' 
#' # get NOAA weather data :
#' # weather_data <- importNOAA(code = stations_code, year = 2020:2021, hourly = TRUE,
#' #                            quiet = FALSE, n.cores=8)
#' #
#' #saveRDS(weather_data,paste0(today(),"_weather_data.rds"))
#' weather_data <- readRDS("2021-03-03_weather_data.rds")
#' 
#' weather_data <- weather_data %>%
#'   mutate(date_day=as_date(date),.after = date) %>%
#'   #filter(date_day<as_date("2020-05-15")) %>%
#'   dplyr::select(-elev, -latitude, -longitude) %>%
#'   mutate(code=as.character(code))
#' 
#' 
#' ## 2.2 Daily summary per station
#' 
#' # Identify days of observation for each weather station with less than
#' # 5 missing hours ("complete day")
#' weather_complete_days=weather_data %>%
#'   mutate(date_day=as_date(date),obs = 1) %>%
#'   group_by(code, date_day) %>%
#'   mutate(total_empties=sum(is.na(air_temp) & is.na(RH)),
#'          count_obs = sum(obs)) %>%
#'   distinct(code,date_day, .keep_all = TRUE) %>%
#'   dplyr::select(code, date_day, total_empties,count_obs) %>%
#'   mutate(complete_day=ifelse(count_obs-total_empties > 19, 1, 0))
#' 
#' # Calculate daily summaries by station
#' daily_station_weather=weather_data %>%
#'   mutate(date_day=as_date(date),
#'          AH=6.112*exp(17.67*air_temp/(air_temp+243.5))*RH*2.1674/(273.15+air_temp),
#'          IPTCC = 100*exp(-0.5*((air_temp-7.5)^2/196+(RH-75)^2/625+(AH-6)^2/2.89))) %>%
#'   group_by(code, station, date_day) %>%
#'   summarise(stat_t.mean=mean(air_temp, na.rm=TRUE),
#'             stat_t.min=min(air_temp, na.rm = TRUE),
#'             stat_t.max=max(air_temp, na.rm = TRUE),
#'             stat_precip=sum(precip, na.rm=TRUE),
#'             stat_RH.mean=mean(RH, na.rm=TRUE),
#'             stat_RH.min=min(RH, na.rm=TRUE),
#'             stat_RH.max=max(RH, na.rm=TRUE),
#'             stat_AH.mean=mean(AH, na.rm=TRUE),
#'             stat_AH.min=min(AH, na.rm=TRUE),
#'             stat_AH.max=max(AH, na.rm=TRUE),
#'             stat_IPTCC.mean=mean(IPTCC, na.rm=TRUE),
#'             stat_IPTCC.min=min(IPTCC, na.rm=TRUE),
#'             stat_IPTCC.max=max(IPTCC, na.rm=TRUE),
#'             stat_ws.mean=mean(ws, na.rm=TRUE),
#'             stat_dewpoint.mean=mean(dew_point, na.rm=TRUE),
#'             stat_dewpoint.min=min(dew_point, na.rm=TRUE),
#'             stat_dewpoint.max=max(dew_point, na.rm=TRUE)
#'   ) %>%
#'   
#'   left_join(weather_complete_days, by = c("code", "date_day"))  %>%
#'   filter(complete_day==1)
#' 
#' ## 2.3 Daily weather data per region (weighted)
#' 
#' ### Function giving daily weighted average weather data for a given area (admin_id)
#' 
#' fun_weighted_daily_obs <- function(data0,which_id){
#'   
#'   list_stations <- data0 %>%
#'     dplyr::select(admin_id, code, code_insee, total_pop_buff) %>%
#'     filter(admin_id == which_id) %>%
#'     mutate(weight_pop= total_pop_buff/sum(total_pop_buff))
#'   
#'   code_insee <- unique(list_stations$code_insee)
#'   
#'   reg_daily_station_weather <- daily_station_weather %>%
#'     filter(code %in% list_stations$code) %>%
#'     dplyr::select(!contains(".min") & !contains(".max")) %>%
#'     mutate(weight_pop = sapply(code,
#'              function(x){list_stations$weight_pop[which(list_stations$code == x)]})) %>%
#'     mutate_at(.vars = vars(contains("stat_")),.funs = funs(.*weight_pop))  %>%
#'     group_by(date_day) %>%
#'     summarise_at(vars(contains("stat_")),.funs = sum) %>%
#'     mutate(code_insee = code_insee, .before = 1)
#'   
#'   
#'   return(reg_daily_station_weather)
#'   
#' }
#' 
#' ### List of daily weather data per region
#' weather_data_reg <- map(.x = 1:12,.f = ~fun_weighted_daily_obs(Regions_stations_pop2,.)) %>%
#'   bind_rows(.id = "admin_id")
#' 
#' save(weather_data_reg, file = "weather_data_reg.rdata")
#' }
"weather_data_reg"