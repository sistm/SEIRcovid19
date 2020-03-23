#install.packages("./monolix/lixoftConnectors.tar.gz", repos = NULL, type="source", INSTALL_opts ="--no-multiarch")
library(lixoftConnectors)
library(ggplot2)
initializeLixoftConnectors(software="monolix")

nameproject<-"sierah_poisson_proj_2yBIG"
newProject(modelFile = "./monolix/seirah_poisson2YBIG.txt",
           data = list(dataFile = "./monolix/data_region_2y_20200322.txt",
                       headerTypes =c("ignore","ignore","ignore","time","ignore","ignore","ignore","regressor","id","regressor","ignore","observation","obsid","regressor"),
                       observationTypes = list(cas_confirmes_incident="discrete",hospitalisation_incident="discrete"),
                       mapping = list("1" = "cas_confirmes_incident", "2" = "hospitalisation_incident")))

saveProject(projectFile = paste("./monolix/outputMonolix/",nameproject,".mlxtran",sep=""))
scenario <- getScenario()
scenario$tasks = c(populationParameterEstimation = T, 
                   conditionalModeEstimation = T, 
                   conditionalDistributionSampling = T, 
                   standardErrorEstimation=T, 
                   logLikelihoodEstimation=T)
scenario$linearization = FALSE
setScenario(scenario)

# ### Check the stability of estimation
# popparams <- getPopulationParameterInformation()
# tabestimates <- NULL; tabse <- NULL
# for(i in 1:5){
#   # sample new initial estimates
#   popini <- sapply(1:nrow(popparams), function(j){runif(n=1, min=popparams$initialValue[j]/2, max=popparams$initialValue[j]*2)})
# 
#   # set sampled values as new initial estimates
#   newpopparams <- popparams
#   newpopparams$initialValue <- popini
#   setPopulationParameterInformation(newpopparams)
# 
#   # run the estimation
#   runScenario()
# 
#   # store the estimates and s.e. in table
#   tabestimates <- cbind(tabestimates, getEstimatedPopulationParameters())
#   tabse <- cbind(tabse, getEstimatedStandardErrors()$stochasticApproximation)
# }
# ### Results are super stable
# > tabestimates
# [,1]      [,2]      [,3]      [,4]      [,5]
# transmission_pop    1.5254090 1.5225192 1.5248879 1.5266301 1.5215884
# ascertainment_pop   0.3526815 0.3533896 0.3530092 0.3515221 0.3545328
# omega_transmission  0.1792003 0.1813981 0.1757027 0.1802492 0.1817200
# omega_ascertainment 0.6511337 0.6460224 0.6418625 0.6410072 0.6567957
# > tabse
# [,1]       [,2]       [,3]       [,4]       [,5]
# transmission_pop    0.08687370 0.08788548 0.08553685 0.08773834 0.08762680
# ascertainment_pop   0.07082241 0.07049257 0.07008018 0.06969336 0.07171978
# omega_transmission  0.04083728 0.04207584 0.03785260 0.04030371 0.04151532
# omega_ascertainment 0.14333148 0.14072314 0.14039738 0.14115975 0.14917383

#Recupere les estimation individuelle et faire des predictions
#popparams <- getPopulationParameterInformation()
#popparams$initialValue <- c(1.5254090,0.3526815,0.1792003,0.6511337)
#setPopulationParameterInformation(popparams)
#runScenario()
##########
# RUN DIRECT ON MONOLIX
##########

indivParams = read.table(paste("./monolix/outputMonolix/",nameproject,"/IndividualParameters/estimatedIndividualParameters.txt",sep=""),header=TRUE,sep=",")[,c("id","transmission_mode","ascertainment_mode","Di_mode","alpha_mode")]
#as.data.frame(getEstimatedIndividualParameters(method="conditionalMode"))
names(indivParams)<-c("id","transmission","ascertainment")

data<-read.table("./monolix/data_region_20200322.txt",sep="\t",header=TRUE)
chiffres<-read.table("./monolix/Chiffres.txt",sep="\t",header=TRUE)
data$date<-lubridate::as_date(as.character(data$date))

dir.create(paste("./monolix/outputMonolix/",nameproject,"/graphics",sep=""))
result_noaction<-as.data.frame(matrix(NA,ncol=9,nrow=0))
names(result_noaction)<-c("location","Jpic","Dpic","maxlit","Jmaxlit","Dmaxlit","Jdepasselit","Ddepasselit","R0day14")
result30conf<-as.data.frame(matrix(NA,ncol=9,nrow=0))
names(result30conf)<-c("location","Jpic","Dpic","maxlit","Jmaxlit","Dmaxlit","Jdepasselit","Ddepasselit","R0day45")
result90conf<-as.data.frame(matrix(NA,ncol=9,nrow=0))
names(result90conf)<-c("location","Jpic","Dpic","maxlit","Jmaxlit","Dmaxlit","Jdepasselit","Ddepasselit","R0day105")
result180conf<-as.data.frame(matrix(NA,ncol=9,nrow=0))
names(result180conf)<-c("location","Jpic","Dpic","maxlit","Jmaxlit","Dmaxlit","Jdepasselit","Ddepasselit","R0day195")
result270conf<-as.data.frame(matrix(NA,ncol=9,nrow=0))
names(result270conf)<-c("location","Jpic","Dpic","maxlit","Jmaxlit","Dmaxlit","Jdepasselit","Ddepasselit","R0day285")
result340conf<-as.data.frame(matrix(NA,ncol=9,nrow=0))
names(result340conf)<-c("location","Jpic","Dpic","maxlit","Jmaxlit","Dmaxlit","Jdepasselit","Ddepasselit","R0day355")
for (i in 1:length(indivParams$id)){
   print(i)
  temp_monolix_estim<-seirah_estim(binit=as.numeric(indivParams[i,2:3]), 
                                    data=data[which(data$goodID==as.character(indivParams[i,1])),],
                                    alpha=as.numeric(indivParams[i,5]),
                                    De=5.2,
                                    Di=as.numeric(indivParams[i,4]),
                                    Dq=10,
                                    Dh=30,
                                    popSize=chiffres[which(chiffres$goodid==as.character(indivParams[i,1])),"size"], 
                                    dailyMove=0.01*chiffres[which(chiffres$goodid==as.character(indivParams[i,1])),"size"],
                                    verbose = TRUE,
                                    optim_ols=FALSE,obs="2Y")
     
    jpeg(paste("./monolix/outputMonolix/",nameproject,"/graphics/FitI_",as.character(indivParams[i,1]),".jpg",sep=""))
    print(plot(temp_monolix_estim,type=1))
     dev.off()
     jpeg(paste("./monolix/outputMonolix/",nameproject,"/graphics/FitH_",as.character(indivParams[i,1]),".jpg",sep=""))
     print(plot(temp_monolix_estim,type=2))
     dev.off()
     alpha=as.numeric(indivParams[i,5])
     Di=as.numeric(indivParams[i,4])
     Dq=10
     RO<-Di*as.numeric(indivParams[i,2])/(temp_monolix_estim$solution[14,"A"]+temp_monolix_estim$solution[14,"I"])*(alpha*temp_monolix_estim$solution[14,"A"]+Dq*temp_monolix_estim$solution[14,"I"]/(Di+Dq))
     
     jpeg(paste("./monolix/outputMonolix/",nameproject,"/graphics/Epidemics365_noaction_",as.character(indivParams[i,1]),".jpg",sep=""))
      par(mfrow=c(2,3))
       plot(temp_monolix_estim$solution[,"time"],temp_monolix_estim$solution[,"S"],type="l",xlab="Time",ylab="S",col="#56B4E9")
       plot(temp_monolix_estim$solution[,"time"],temp_monolix_estim$solution[,"E"],type="l",xlab="Time",ylab="E",col="#F0E442")
       plot(temp_monolix_estim$solution[,"time"],temp_monolix_estim$solution[,"I"],type="l",xlab="Time",ylab="I",col="#E69F00")
       plot(temp_monolix_estim$solution[,"time"],temp_monolix_estim$solution[,"R"],type="l",xlab="Time",ylab="R",col="#009E73")
       plot(temp_monolix_estim$solution[,"time"],temp_monolix_estim$solution[,"A"],type="l",xlab="Time",ylab="A",col="#CC79A7")
       plot(temp_monolix_estim$solution[,"time"],temp_monolix_estim$solution[,"H"],type="l",xlab="Time",ylab="H",col="#D55E00")
       abline(h=chiffres[which(chiffres$goodid==as.character(indivParams[i,1])),"ICUnb"],col="red")
       par(mfrow=c(1,1))
     dev.off()

      temp_monolix_projection<-predict(temp_monolix_estim, thresholdICU = chiffres[which(chiffres$goodid==as.character(indivParams[i,1])),"ICUnb"],verbose=TRUE)

      result_noaction[i,]<-c(as.character(indivParams[i,1]),temp_monolix_projection$predictions$Jpic,as.character(temp_monolix_projection$predictions$Dpic),round(temp_monolix_projection$predictions$maxlit,0),temp_monolix_projection$predictions$Jmaxlit,as.character(temp_monolix_projection$predictions$Dmaxlit),temp_monolix_projection$predictions$Jdepasselit,as.character(temp_monolix_projection$predictions$Ddepasselit),RO)
   
      result30conf[i,]<-predictepidemie(binit=as.numeric(indivParams[i,2:3]), 
                                    data=data[which(data$goodID==as.character(indivParams[i,1])),],
                                    alpha=as.numeric(indivParams[i,5]),
                                    De=5.2,
                                    Di=as.numeric(indivParams[i,4]),
                                    Dq=10,
                                    Dh=30,
                                    popSize=chiffres[which(chiffres$goodid==as.character(indivParams[i,1])),"size"], 
                                    dailyMove=0.01*chiffres[which(chiffres$goodid==as.character(indivParams[i,1])),"size"],
                                    verbose = TRUE,
                                    optim_ols=FALSE,
                                    timeconf=14,lengthconf=30,
                                    newdailyMove=0,factorreductrans=3,obs="2Y")
      result90conf[i,]<-predictepidemie(binit=as.numeric(indivParams[i,2:3]), 
                                        data=data[which(data$goodID==as.character(indivParams[i,1])),],
                                        alpha=as.numeric(indivParams[i,5]),
                                        De=5.2,
                                        Di=as.numeric(indivParams[i,4]),
                                        Dq=10,
                                        Dh=30,
                                        popSize=chiffres[which(chiffres$goodid==as.character(indivParams[i,1])),"size"], 
                                        dailyMove=0.01*chiffres[which(chiffres$goodid==as.character(indivParams[i,1])),"size"],
                                        verbose = TRUE,
                                        optim_ols=FALSE,
                                        timeconf=14,lengthconf=90,
                                        newdailyMove=0,factorreductrans=3,obs="2Y")  
      result180conf[i,]<-predictepidemie(binit=as.numeric(indivParams[i,2:3]), 
                                        data=data[which(data$goodID==as.character(indivParams[i,1])),],
                                        alpha=as.numeric(indivParams[i,5]),
                                        De=5.2,
                                        Di=as.numeric(indivParams[i,4]),
                                        Dq=10,
                                        Dh=30,
                                        popSize=chiffres[which(chiffres$goodid==as.character(indivParams[i,1])),"size"], 
                                        dailyMove=0.01*chiffres[which(chiffres$goodid==as.character(indivParams[i,1])),"size"],
                                        verbose = TRUE,
                                        optim_ols=FALSE,
                                        timeconf=14,lengthconf=180,
                                        newdailyMove=0,factorreductrans=3,obs="2Y")
      result270conf[i,]<-predictepidemie(binit=as.numeric(indivParams[i,2:3]), 
                                        data=data[which(data$goodID==as.character(indivParams[i,1])),],
                                        alpha=as.numeric(indivParams[i,5]),
                                        De=5.2,
                                        Di=2.3,
                                        Dq=10,
                                        Dh=30,
                                        popSize=chiffres[which(chiffres$goodid==as.character(indivParams[i,1])),"size"], 
                                        dailyMove=0.01*chiffres[which(chiffres$goodid==as.character(indivParams[i,1])),"size"],
                                        verbose = TRUE,
                                        optim_ols=FALSE,
                                        timeconf=14,lengthconf=270,
                                        newdailyMove=0,factorreductrans=3,obs="2Y")
      result340conf[i,]<-predictepidemie(binit=as.numeric(indivParams[i,2:3]), 
                                        data=data[which(data$goodID==as.character(indivParams[i,1])),],
                                        alpha=as.numeric(indivParams[i,5]),
                                        De=5.2,
                                        Di=as.numeric(indivParams[i,4]),
                                        Dq=10,
                                        Dh=30,
                                        popSize=chiffres[which(chiffres$goodid==as.character(indivParams[i,1])),"size"], 
                                        dailyMove=0.01*chiffres[which(chiffres$goodid==as.character(indivParams[i,1])),"size"],
                                        verbose = TRUE,
                                        optim_ols=FALSE,
                                        timeconf=14,lengthconf=340,
                                        newdailyMove=0,factorreductrans=3,obs="2Y")

   
}
result_noaction
result30conf
result90conf
result180conf
result270conf
result340conf
write.table(result_noaction,file=paste("./monolix/outputMonolix/",nameproject,"/graphics/indicateurs_results_noaction.txt",sep=""),row.names = F,quote=F)
write.table(result30conf,file=paste("./monolix/outputMonolix/",nameproject,"/graphics/indicateurs_results_30conf.txt",sep=""),row.names = F,quote=F)
write.table(result90conf,file=paste("./monolix/outputMonolix/",nameproject,"/graphics/indicateurs_results_90conf.txt",sep=""),row.names = F,quote=F)
write.table(result180conf,file=paste("./monolix/outputMonolix/",nameproject,"/graphics/indicateurs_results_180conf.txt",sep=""),row.names = F,quote=F)
write.table(result270conf,file=paste("./monolix/outputMonolix/",nameproject,"/graphics/indicateurs_results_270conf.txt",sep=""),row.names = F,quote=F)
write.table(result340conf,file=paste("./monolix/outputMonolix/",nameproject,"/graphics/indicateurs_results_340conf.txt",sep=""),row.names = F,quote=F)



predictepidemie<-function(binit, 
                          data,
                          alpha,
                          De,
                          Di,
                          Dq,
                          Dh,
                          popSize,
                          dailyMove,
                          verbose,
                          optim_ols,
                          timeconf,
                          lengthconf,
                          newdailyMove,
                          factorreductrans,
                          obs){
   temp_monolix_estim<-seirah_estim(binit=binit, 
                                    data=data,
                                    alpha=alpha,
                                    De=De,
                                    Di=Di,
                                    Dq=Dq,
                                    Dh=Dh,
                                    popSize=popSize, 
                                    dailyMove=dailyMove,
                                    verbose = verbose,
                                    optim_ols=optim_ols,
                                    timeconf=timeconf,
                                    lengthconf=lengthconf,
                                    newdailyMove=newdailyMove,
                                    factorreductrans=factorreductrans,
                                    obs=obs)

   RO<-Di*as.numeric(indivParams[i,2])/(temp_monolix_estim$solution[lengthconf+15,"A"]+temp_monolix_estim$solution[lengthconf+15,"I"])*(alpha*temp_monolix_estim$solution[lengthconf+15,"A"]+Dq*temp_monolix_estim$solution[lengthconf+15,"I"]/(Di+Dq))
   
   jpeg(paste("./monolix/outputMonolix/",nameproject,"/graphics/Epidemics365_",lengthconf,"_",as.character(indivParams[i,1]),".jpg",sep=""))
   par(mfrow=c(2,3))
   plot(temp_monolix_estim$solution[,"time"],temp_monolix_estim$solution[,"S"],type="l",xlab="Time",ylab="S",col="#56B4E9")
   plot(temp_monolix_estim$solution[,"time"],temp_monolix_estim$solution[,"E"],type="l",xlab="Time",ylab="E",col="#F0E442")
   plot(temp_monolix_estim$solution[,"time"],temp_monolix_estim$solution[,"I"],type="l",xlab="Time",ylab="I",col="#E69F00")
   plot(temp_monolix_estim$solution[,"time"],temp_monolix_estim$solution[,"R"],type="l",xlab="Time",ylab="R",col="#009E73")
   plot(temp_monolix_estim$solution[,"time"],temp_monolix_estim$solution[,"A"],type="l",xlab="Time",ylab="A",col="#CC79A7")
   plot(temp_monolix_estim$solution[,"time"],temp_monolix_estim$solution[,"H"],type="l",xlab="Time",ylab="H",col="#D55E00")
   abline(h=chiffres[which(chiffres$goodid==as.character(indivParams[i,1])),"ICUnb"],col="red")
   par(mfrow=c(1,1))
   dev.off()
   
   temp_monolix_projection<-predict(temp_monolix_estim, thresholdICU = chiffres[which(chiffres$goodid==as.character(indivParams[i,1])),"ICUnb"],verbose=TRUE)
   
   predictepidemie<-c(as.character(indivParams[i,1]),temp_monolix_projection$predictions$Jpic,as.character(temp_monolix_projection$predictions$Dpic),round(temp_monolix_projection$predictions$maxlit,0),temp_monolix_projection$predictions$Jmaxlit,as.character(temp_monolix_projection$predictions$Dmaxlit),temp_monolix_projection$predictions$Jdepasselit,as.character(temp_monolix_projection$predictions$Ddepasselit),RO)
   
}
