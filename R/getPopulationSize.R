#' getPopulationSize
#'
#' @description Add popsize column with the population for each region and
#' departement.
#'
#' @param dataset A dataset with an 'id' column.
#'
#' @return The dataset with the added popsize column.
#' @export
#' @examples
#' dftest <- data.frame(id = c("REG-01", "REG-75"),
#'                      status = c("Ok", "Amazing"))
#' getPopulationSize(dftest)
#'
getPopulationSize<-function(dataset){

  dataset$popsize<-NA
  dataset$popsize[which(dataset$id=="REG-01")]<-395700
  dataset$popsize[which(dataset$id=="REG-02")]<-376480
  dataset$popsize[which(dataset$id=="REG-03")]<-290691
  dataset$popsize[which(dataset$id=="REG-04")]<-859959
  dataset$popsize[which(dataset$id=="REG-06")]<-270372
  dataset$popsize[which(dataset$id=="REG-94")]<-344679
  dataset$popsize[which(dataset$id=="REG-11")]<-12278210
  dataset$popsize[which(dataset$id=="REG-24")]<-2559073
  dataset$popsize[which(dataset$id=="REG-27")]<-2783039
  dataset$popsize[which(dataset$id=="REG-28")]<-3303500
  dataset$popsize[which(dataset$id=="REG-32")]<-5962662
  dataset$popsize[which(dataset$id=="REG-44")]<-5511747
  dataset$popsize[which(dataset$id=="REG-52")]<-3801797
  dataset$popsize[which(dataset$id=="REG-53")]<-3340379
  dataset$popsize[which(dataset$id=="REG-75")]<-5999982
  dataset$popsize[which(dataset$id=="REG-76")]<-5924858
  dataset$popsize[which(dataset$id=="REG-84")]<-8032377
  dataset$popsize[which(dataset$id=="REG-93")]<-5055651
  dataset$popsize[which(dataset$id=="DEP-33")]<-1583384
  dataset$popsize[which(dataset$id=="DEP-64")]<-677309
  dataset$popsize[which(dataset$id=="DEP-17")]<-644303
  dataset$popsize[which(dataset$id=="DEP-86")]<-436876
  dataset$popsize[which(dataset$id=="DEP-24")]<-413606
  dataset$popsize[which(dataset$id=="DEP-40")]<-407444
  dataset$popsize[which(dataset$id=="DEP-87")]<-374426
  dataset$popsize[which(dataset$id=="DEP-79")]<-374351
  dataset$popsize[which(dataset$id=="DEP-16")]<-352335
  dataset$popsize[which(dataset$id=="DEP-47")]<-332842
  dataset$popsize[which(dataset$id=="DEP-19")]<-241464
  dataset$popsize[which(dataset$id=="DEP-23")]<-118638

  return(dataset)
}
