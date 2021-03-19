#' getEpidemicsStarts
#'
#' @param dataset Dataset with 'date' and 'epidemicsStart' columns
#'
#' @return remove data prior to epidemicsStart and add 'day' a count from epidemicsStart
#' @export
#' @examples
#'
#' dfTest <- data.frame(date = seq.Date(from = as.Date("2020-01-01"),
#'                                      to = as.Date("2020-01-31"),
#'                                      by = 1),
#'                      epidemicsStart = as.Date("2020-01-10"))
#' getEpidemicsStarts(dfTest)
#'
getEpidemicsStarts<-function(dataset){

  dataset<-dataset[which(as.Date(dataset$date)>=dataset$epidemicsStart),]

  dataset$day<-as.numeric(as.Date(dataset$date)-as.Date(dataset$epidemicsStart))

  dataset$epidemicsStart<-as.Date(as.character(dataset$epidemicsStart))

  return(dataset)
}
