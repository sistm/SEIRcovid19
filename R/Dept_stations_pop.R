#' Meteorological stations dataset per French departement (2021-03-03)
#'
#'A dataset containing the stations parameters at the departement level.
#'The variables are as follows:
#'
#' \itemize{
#'   \item code_insee departement insee code
#'   \item nom region name
#'   \item nuts3 Nomenclature of Territorial Units
#'   \item wikipedia wikipedia id
#'   \item surf_km2 surface covered in km2
#'   \item fid Geographical unique id
#'   \item usaf United States Air Force Id
#'   \item wban Wireless Body Area Network 
#'   \item station Station name
#'   \item ctry country id
#'   \item st state id
#'   \item call 
#'   \item latitude Latitude of the station
#'   \item longitude Longitude of the station
#'   \item elev.m. Altitude of the station
#'   \item begin Date of beginning of activity
#'   \item end Date of end of activity
#'   \item code Station code
#'   \item fra_pop_su Metropolitan French population on the station surface
#'   \item glp_pop_su Guadeloup population on the station surface
#'   \item guf_pop_su Guyanne population on the station surface
#'   \item mtq_pop_su Martinique population on the station surface
#'   \item myt_pop_su Mayotte population on the station surface
#'   \item reu_pop_su Reunion population surface
#' }
#' @format A data frame with 328 rows and 24 variables
#'
#'@examples
#'##### script generating meteo dataframe
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
#' Dept_stations_pop <- read.csv("departements_stations_pop.csv")
#' save(Dept_stations_pop, file = "Dept_stations_pop.rdata")
#' }
"Dept_stations_pop"