#' getlockdownInfo
#'
#' @description Add info about first lockdown to a dataset.
#'
#' @param dataset The dataset with date, day, epidemicsStart and nameid columns.
#'
#' @return Add lockdown, timeSInceConf, timeSinceDeConf and timelock columns.
#' @export
#' @examples
#'
#' vecDate <- seq.Date(from = as.Date("2020-03-01"),
#' to = as.Date("2020-06-01"),
#' by = 1)
#'
#' dfTest <- data.frame(date = vecDate,
#'                      day = c(0, seq_along(vecDate[-1])),
#'                      epidemicsStart = as.Date("2020-01-10"),
#'                      nameid = c("Tatooine"))
#' getlockdownInfo(dfTest)
#'
getlockdownInfo<-function(dataset){

  dataset$lockdown<-ifelse((as.Date(dataset$date)>=as.Date("2020-03-17"))&(as.Date(dataset$date)<as.Date("2020-05-11")),1,0)
  dataset$timeSinceConf<-pmax(0,as.Date(dataset$date)-as.Date("2020-03-16"))
  dataset$timeSinceDeConf<-pmax(0,as.Date(dataset$date)-as.Date("2020-05-10"))
  for (i in 1:length(dataset$lockdown)){
    dataset$timelock[i]<-dataset$day[which((dataset$timeSinceConf==1)&(dataset$nameid==dataset$nameid[i]))]
  }
  return(dataset)
}


