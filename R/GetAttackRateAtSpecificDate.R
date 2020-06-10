#' @export

GetAttackRateAtSpecificDate<-function(ode_list,SpecificDateAttackRateEquation,dateAR,popsize_name,NationalName){
  GetAttackrateWithExp<-function(solution,parameter,exp){
    with(as.list(c(solution,parameter)),{
      AR<-data.frame((eval(parse(text=exp))))
      names(AR)<-"AR"
      return(AR)
    })
  }
  indivParams <-read.table(paste(here::here(),'/MonolixFile/',"/outputMonolix/",ode_list[[1]]$nameproject,"/IndividualParameters/estimatedIndividualParameters.txt",sep=""),header=TRUE,sep=",")
  popsize_per_id<-rep(0,length(ode_list))
  TableARInfinity<-as.data.frame(matrix(NA,length(ode_list)+1,2))
  colnames(TableARInfinity)<-c("Reg","AR")
  TableARInfinity$Reg<-c(as.character(indivParams$id),NationalName)
  dateAR<-as.Date(dateAR)
  AR<-as.data.frame(matrix(NA,length(ode_list),1))
  ARmin<-as.data.frame(matrix(NA,length(ode_list),1))
  ARmax<-as.data.frame(matrix(NA,length(ode_list),1))
  for (id in 1:length(ode_list)){
    popsize_per_id[id]<-ode_list[[id]]$parameter[names(ode_list[[id]]$parameter)==popsize_name]
    AR[id,1]<-GetAttackrateWithExp(ode_list[[id]]$LongTerm[which(ode_list[[id]]$LongTerm$date==dateAR),],ode_list[[id]]$parameter,SpecificDateAttackRateEquation)
    ARmin[id,1]<-GetAttackrateWithExp(ode_list[[id]]$LongTermMin[which(ode_list[[id]]$LongTerm$date==dateAR),],ode_list[[id]]$parameter,SpecificDateAttackRateEquation)
    ARmax[id,1]<-GetAttackrateWithExp(ode_list[[id]]$LongTermMax[which(ode_list[[id]]$LongTerm$date==dateAR),],ode_list[[id]]$parameter,SpecificDateAttackRateEquation)
    TableARInfinity$AR[id]<-paste(format(round(AR[id,1],2),nsmall=2)," [",
                                  format(round(ARmin[id,1],2),nsmall=2),";",
                                  format(round(ARmax[id,1],2),nsmall=2),"]",sep="")
  }
  TableARInfinity$AR[length(ode_list)+1]<-paste(format(round(sum(AR[,1]*(popsize_per_id))/sum(popsize_per_id),2),nsmall=2)," [",
                                format(round(sum(ARmin[,1]*(popsize_per_id))/sum(popsize_per_id),2),nsmall=2),";",
                                format(round(sum(ARmax[,1]*(popsize_per_id))/sum(popsize_per_id),2),nsmall=2),"]",sep="")

  LatexTable<-xtable::xtable(TableARInfinity[,c("Reg","AR")])
  print(LatexTable,include.rownames = FALSE,
        file = paste(here::here(),'/MonolixFile/',"/outputMonolix/",ode_list[[1]]$nameproject,"/TableAR_",as.character(dateAR),".txt",sep=""))
  return(ode_list)
}