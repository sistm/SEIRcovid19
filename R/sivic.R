#' sivic data
#'
#' A dataset containing the following data :
#'
#' \itemize{
#'   \item id region id
#'   \item nameid region name
#'   \item date the date
#'   \item obs no idea
#'   \item popsize the region population size
#'   \item epidemicsStart date of epidemic start
#'   \item day day since start of the epidemic
#'   \item lockdown is it currently lockdown
#'   \item timeSinceConf the time since lockdown
#'   \item timeSinceDeConf the time since the end of lockdown
#'   \item rsent no idea
#'   \item Dh no idea
#'   \item obs_id no idea
#' }
#' @format A data frame with 8850 rows and 13 variables
#'
#'@examples
#' ##### script generating biom_kalman dataframe
#' \dontrun{
#' sivic <- ImportSivic(file = "data/sivic_panel_reg_latest (7).csv")
#' save(sivic, file = "data/sivic.rdata")
#' }
"sivic"
