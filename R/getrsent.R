#' getrsent
#'
#' @description Get ascertainment rate for each region.
#'
#' @param dataset A dataset with the id column
#'
#' @return Add the rsent column to the dataset.
#' @export
#' @examples
#' dftest <- data.frame(id = c("REG-01", "REG-75"),
#'                      status = c("Ok", "Amazing"))
#' getrsent(dftest)
#'
getrsent<-function(dataset){
  dataset$rsent<-NA
  dataset$rsent[which(dataset$id=="REG-01")]<-0.033
  dataset$rsent[which(dataset$id=="REG-02")]<-0.033
  dataset$rsent[which(dataset$id=="REG-03")]<-0.033
  dataset$rsent[which(dataset$id=="REG-04")]<-0.033
  dataset$rsent[which(dataset$id=="REG-06")]<-0.033
  dataset$rsent[which(dataset$id=="REG-94")]<-0.033
  dataset$rsent[which(dataset$id=="REG-11")]<-0.043101077
  dataset$rsent[which(dataset$id=="REG-24")]<-0.028274001
  dataset$rsent[which(dataset$id=="REG-27")]<-0.070075931
  dataset$rsent[which(dataset$id=="REG-28")]<-0.020382084
  dataset$rsent[which(dataset$id=="REG-32")]<-0.017936282
  dataset$rsent[which(dataset$id=="REG-44")]<-0.051362363
  dataset$rsent[which(dataset$id=="REG-52")]<-0.009387621
  dataset$rsent[which(dataset$id=="REG-53")]<-0.018274502
  dataset$rsent[which(dataset$id=="REG-75")]<-0.032358432
  dataset$rsent[which(dataset$id=="REG-76")]<-0.026243179
  dataset$rsent[which(dataset$id=="REG-84")]<-0.025958432
  dataset$rsent[which(dataset$id=="REG-93")]<-0.053534731
  dataset$rsent[which(dataset$id=="DEP-33")]<-0.033
  dataset$rsent[which(dataset$id=="DEP-64")]<-0.033
  dataset$rsent[which(dataset$id=="DEP-17")]<-0.033
  dataset$rsent[which(dataset$id=="DEP-86")]<-0.033
  dataset$rsent[which(dataset$id=="DEP-24")]<-0.033
  dataset$rsent[which(dataset$id=="DEP-40")]<-0.033
  dataset$rsent[which(dataset$id=="DEP-87")]<-0.033
  dataset$rsent[which(dataset$id=="DEP-79")]<-0.033
  dataset$rsent[which(dataset$id=="DEP-16")]<-0.033
  dataset$rsent[which(dataset$id=="DEP-47")]<-0.033
  dataset$rsent[which(dataset$id=="DEP-19")]<-0.033
  dataset$rsent[which(dataset$id=="DEP-23")]<-0.033

  return(dataset)
}
