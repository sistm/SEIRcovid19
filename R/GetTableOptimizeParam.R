#' @export

GetTableOptimizeParam<-function(ode_list,popsize_name,NationalName,digits,nbinits){
  indivParams <-read.table(paste(here::here(),'/MonolixFile/',"/outputMonolix/",ode_list[[1]]$nameproject,"/IndividualParameters/estimatedIndividualParameters.txt",sep=""),header=TRUE,sep=",")
  popParams<-read.table(paste(here::here(),'/MonolixFile/',"/outputMonolix/",ode_list[[1]]$nameproject,"/populationParameters.txt",sep=""),header=TRUE,sep=",")
  optimize_param_name<-c(names(ode_list[[1]]$parameter[ode_list[[1]]$Variability$param>0]),names(ode_list[[1]]$InitState[ode_list[[1]]$Variability$init>0]))
  SdOptimizeParam<-as.data.frame(matrix(NA,length(ode_list),length(optimize_param_name)))
  colnames(SdOptimizeParam)<-optimize_param_name
  rownames(SdOptimizeParam)<-indivParams$id
  OptimizeParam<-as.data.frame(matrix(NA,length(ode_list),length(optimize_param_name)))
  colnames(OptimizeParam)<-optimize_param_name
  rownames(OptimizeParam)<-indivParams$id
  TableParam<-OptimizeParam
  popsize_per_id<-rep(0,length(ode_list))
  for (id in 1:length(ode_list)){
    popsize_per_id[id]<-ode_list[[id]]$parameter[names(ode_list[[id]]$parameter)==popsize_name]
    for (j in 1:length(optimize_param_name)){
      optimize_monolix_name<-paste(optimize_param_name[j],"_sd",sep="")
      SdOptimizeParam[id,j]<-indivParams[id,optimize_monolix_name]
      optimize_monolix_name<-paste(optimize_param_name[j],"_mode",sep="")
      OptimizeParam[id,j]<-indivParams[id,optimize_monolix_name]
      if (SdOptimizeParam[id,j]==0){
        optimize_pop_name<-paste(optimize_param_name[j],"_pop",sep="")
        SdOptimizeParam[id,j]<-popParams[optimize_pop_name,"stochasticApproximation"]
      }
      OptimizeParamMin<-format(round(OptimizeParam[id,j]-1.96*SdOptimizeParam[id,j], digits[j]), nsmall = digits[j])
      OptimizeParamMax<-format(round(OptimizeParam[id,j]+1.96*SdOptimizeParam[id,j], digits[j]), nsmall = digits[j])
      TableParam[id,j]<-paste(format(round(OptimizeParam[id,j],digits[j]), nsmall = digits[j])," [",OptimizeParamMin,";",OptimizeParamMax,"]",sep="")
    }
  }
  for (j in 1:(length(optimize_param_name)-nbinits)){
    TableParam[dim(OptimizeParam)[1]+1,j]<-paste(format(round(sum(OptimizeParam[,j]*(popsize_per_id))/sum(popsize_per_id),digits[j]),nsmall=digits[j])," [",
                                                 format(round(sum((OptimizeParam[,j]-1.96*SdOptimizeParam[,j])*(popsize_per_id))/sum(popsize_per_id),digits[j]),nsmall=digits[j]),";",
                                                 format(round(sum((OptimizeParam[,j]+1.96*SdOptimizeParam[,j])*(popsize_per_id))/sum(popsize_per_id),digits[j]),nsmall=digits[j]),"]",sep="")
  }
  for (j in (length(optimize_param_name)-nbinits+1):length(optimize_param_name)){
    TableParam[dim(OptimizeParam)[1]+1,j]<-paste(format(round(sum(OptimizeParam[,j],digits[j])),nsmall=digits[j])," [",
                                                 format(round(sum((OptimizeParam[,j]-1.96*SdOptimizeParam[,j])),digits[j]),nsmall=digits[j]),";",
                                                 format(round(sum((OptimizeParam[,j]+1.96*SdOptimizeParam[,j])),digits[j]),nsmall=digits[j]),"]",sep="")
  }
  
  rownames(TableParam)<-c(rownames(OptimizeParam),NationalName)
  
  TableParam$Reg<-row.names(TableParam)
  
  LatexTable<-xtable::xtable(TableParam[,c("Reg",optimize_param_name)])
  print(LatexTable,include.rownames = FALSE,
        file = paste(here::here(),'/MonolixFile/',"/outputMonolix/",ode_list[[1]]$nameproject,"/TableOptimizeParameter.txt",sep=""))
  
  
  return(TableParam)
}