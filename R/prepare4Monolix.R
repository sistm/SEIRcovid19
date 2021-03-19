#' prepare4Monolix
#'
#' @description Perform some modification to a dataset to make it ready for monolix.
#'
#' @param dataset the dataset of interest
#' @param recuperation correspond to the observations (usually H and Hin)
#' @param nametokeep the variables to keep
#'
#' @return A dataframe prepared for monolix
#' @export
#' @examples
#' recuperation <- c("colA", "colB")
#' nametokeep <- c("ImportantVar", "VeryImportantVar")
#'
#' dfTest <- data.frame(colA = runif(6),
#'                      colB = runif(6),
#'                      ImportantVar = runif(6),
#'                      VeryImportantVar = runif(6),
#'                      id = c(1, 1, 1, 2, 2, 2),
#'                      nameid = "Tatooine",
#'                      date = as.Date(1:6, origin = "2020-01-01"),
#'                      obs = rnorm(n = 6),
#'                      obs_id = 1:6)
#'
#' prepare4Monolix(dataset = dfTest,
#'                 recuperation = recuperation,
#'                 nametokeep = nametokeep)
#'
prepare4Monolix<-function(dataset, recuperation, nametokeep){
  k<-1
  list_data<-list()
  for (i in recuperation){
    #Get incidences
    list_data[[k]]<-dataset[which(!is.na(dataset[,i])),c("id","nameid","date",i,nametokeep)]
    list_data[[k]]$obs_id<-k
    names(list_data[[k]])<-c("id","nameid","date","obs",nametokeep,"obs_id")
    k<-k+1
  }
  datasetMONOLIX<- do.call("rbind", list_data)
  datasetMONOLIX$obs<-round(datasetMONOLIX$obs,0)
  datasetMONOLIX <- datasetMONOLIX[order(datasetMONOLIX$id, datasetMONOLIX$obs_id, datasetMONOLIX$date),]

  return(datasetMONOLIX)
}

