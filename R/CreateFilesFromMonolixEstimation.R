CreateFilesFromMonolixEstimation<-function(path){
  folder<-strsplit(path,"/")
  ProjectName<-folder[[1]][length(folder[[1]])]
  
  dir.create(paste(here::here(),'/MonolixFile/outputMonolix/',sep=""))
  dir.create(paste(here::here(),'/MonolixFile/outputMonolix/',ProjectName,sep=""))
  dir.create(paste(here::here(),'/MonolixFile/outputMonolix/',ProjectName,'/IndividualParameters/',sep=""))

  # Indiv Param
  indiv_param_tot<-read.table(paste(path,'/IndividualParameters/estimatedIndividualParameters.txt',sep=""),sep=",",header=TRUE)
  param_names<-colnames(indiv_param_tot)
  x<-c()
  x[1]<-"id"
  for (i in 2:length(param_names)){
    
    if (substr(param_names[[i]],nchar(param_names[[i]])-3,nchar(param_names[[i]]))=="mode"){
      x<-append(x,param_names[[i]])
    }else{}
    if (substr(param_names[[i]],nchar(param_names[[i]])-1,nchar(param_names[[i]]))=="sd"){
      x<-append(x,param_names[[i]])
    }else{}
  }
  x
  indiv_param<-indiv_param_tot[,x]
  
  write.table(indiv_param,file=paste(here::here(),'/MonolixFile/outputMonolix/',ProjectName,'/IndividualParameters/',"estimatedIndividualParameters.txt",sep=""),sep=",")
  
  pop_tot<-read.table(paste(path,'/populationParameters.txt',sep=""),sep=",",header=TRUE)
  pop<-as.data.frame(pop_tot[,c("se_sa")])
  rownames(pop)<-pop_tot$parameter
  colnames(pop)<-c("stochasticApproximation")
  write.table(pop,file=paste(here::here(),'/MonolixFile/outputMonolix/',ProjectName,'/',"populationParameters.txt",sep=""),sep=",")
  
  logtot<-read.table(paste(path,'/LogLikelihood/logLikelihood.txt',sep=""),sep=",",header=TRUE)
  loglike<-as.data.frame(logtot[,c("importanceSampling")])
  rownames(loglike)<-logtot$criteria
  colnames(loglike)<-c("importanceSampling")
  write.table(loglike,file=paste(here::here(),'/MonolixFile/outputMonolix/',ProjectName,'/',"logLikelihood.txt",sep=""),sep=",")
  
  covariance_estimate<-read.table(paste(path,'/FisherInformation/covarianceEstimatesSA.txt',sep=""),sep=",",header=FALSE)
  write.table(covariance_estimate,file=paste(here::here(),'/MonolixFile/outputMonolix/',ProjectName,'/',"covarianceEstimatesSA.txt",sep=""),sep=",",col.names = FALSE,row.names = FALSE)
  
  
 return(paste(here::here(),'/MonolixFile/outputMonolix/',ProjectName,sep="")) 
}