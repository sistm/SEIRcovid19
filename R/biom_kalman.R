#' Kalman data with epidemic start by region
#'
#' A dataset containing the epidemic start by region
#'
#' \itemize{
#'   \item id region id
#'   \item nameid region name
#'   \item epidemicsStart date of epidemic start
#'   \item Dh Length of hospitalisation
#' }
#' @format A data frame with 12 rows and 7 variables
#'
#'@examples
#' ##### script generating biom_kalman dataframe
#' \dontrun{
#' biom_kalman <- read.delim("R/DATABIOMKALMAN.txt",sep="\t",header=T) %>%
#'   select(id, nameid, epidemicsStart) %>%
#'   unique() %>%
#'   mutate(Dh = c(18.4, 19.5, 18.1, 20.3, 18.2, 17.7, 16.6, 19, 18, 17.2, 17, 17.8))
#'
#' save(biom_kalman, file = "data/biom_kalman.rdata")
#' }
"biom_kalman"
