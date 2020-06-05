#' @export

GetAttackRateAtSpecificDate<-function(ode_list,SpecificDateAttackRateEquation,dateAR){
  GetAttackrateWithExp<-function(solution,parameter,exp){
    with(as.list(c(solution,parameter)),{
      AR<-data.frame((eval(parse(text=exp))))
      names(AR)<-"AR"
      return(AR)
    })
  }
  TableARInfinity<-as.data.frame(matrix(NA,length(ode_list),2))
  colnames(TableARInfinity)<-c("Reg","AR")
  indivParams <-read.table(paste(here::here(),'/MonolixFile/',"/outputMonolix/",ode_list[[1]]$nameproject,"/IndividualParameters/estimatedIndividualParameters.txt",sep=""),header=TRUE,sep=",")
  TableARInfinity$Reg<-indivParams$id
  dateAR<-as.Date(dateAR)
  for (id in 1:length(ode_list)){
    AR<-GetAttackrateWithExp(ode_list[[id]]$LongTerm[which(ode_list[[id]]$LongTerm$date==dateAR),],ode_list[[id]]$parameter,SpecificDateAttackRateEquation)
    ARmin<-GetAttackrateWithExp(ode_list[[id]]$LongTermMin[which(ode_list[[id]]$LongTerm$date==dateAR),],ode_list[[id]]$parameter,SpecificDateAttackRateEquation)
    ARmax<-GetAttackrateWithExp(ode_list[[id]]$LongTermMax[which(ode_list[[id]]$LongTerm$date==dateAR),],ode_list[[id]]$parameter,SpecificDateAttackRateEquation)
    TableARInfinity$AR[id]<-paste(format(round(AR,2),nsmall=0)," [",
                                  format(round(ARmin,2),nsmall=0),";",
                                  format(round(ARmax,2),nsmall=0),"]",sep="")
  }
  LatexTable<-xtable::xtable(TableARInfinity[,c("Reg","AR")])
  print(LatexTable,include.rownames = FALSE,
        file = paste(here::here(),'/MonolixFile/',"/outputMonolix/",ode_list[[1]]$nameproject,"/TableAR_",as.character(dateAR),".txt",sep=""))
  return(ode_list)
}