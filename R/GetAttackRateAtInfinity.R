#' @export

GetAttackRateAtInfinity<-function(ode_list,AttackRateInfintyFormula){
  GetAttackrateWithExp<-function(solution,parameter,exp){
    with(as.list(c(solution,parameter)),{
      AR<-data.frame((eval(parse(text=exp))))
      names(AR)<-"AR"
      return(AR)
    })
  }
  TableARInfinity<-as.data.frame(matrix(NA,length(ode_list),2))
  colnames(TableARInfinity)<-c("Reg","ARInf")
  indivParams <-read.table(paste(here::here(),'/MonolixFile/',"/outputMonolix/",ode_list[[1]]$nameproject,"/IndividualParameters/estimatedIndividualParameters.txt",sep=""),header=TRUE,sep=",")
  TableARInfinity$Reg<-indivParams$id
  for (id in 1:length(ode_list)){
    AR<-GetAttackrateWithExp(ode_list[[id]]$LongTerm[dim(ode_list[[id]]$LongTerm)[1],],ode_list[[id]]$parameter,AttackRateInfintyFormula)
    ARmin<-GetAttackrateWithExp(ode_list[[id]]$LongTermMin[dim(ode_list[[id]]$LongTermMin)[1],],ode_list[[id]]$parameter,AttackRateInfintyFormula)
    ARmax<-GetAttackrateWithExp(ode_list[[id]]$LongTermMax[dim(ode_list[[id]]$LongTermMax)[1],],ode_list[[id]]$parameter,AttackRateInfintyFormula)
    TableARInfinity$ARInf[id]<-paste(format(round(AR,2),nsmall=0)," [",
                                     format(round(ARmin,2),nsmall=0),";",
                                     format(round(ARmax,2),nsmall=0),"]",sep="")
  }
  LatexTable<-xtable::xtable(TableARInfinity[,c("Reg","ARInf")])
  print(LatexTable,include.rownames = FALSE,
        file = paste(here::here(),'/MonolixFile/',"/outputMonolix/",ode_list[[1]]$nameproject,"/TableARInf.txt",sep=""))
  return(ode_list)
}