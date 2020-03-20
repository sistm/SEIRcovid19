#install.packages("./monolix/lixoftConnectors.tar.gz", repos = NULL, type="source", INSTALL_opts ="--no-multiarch")
library(lixoftConnectors)
initializeLixoftConnectors(software="monolix")

newProject(modelFile = "./monolix/seirah_poisson.txt",
           data = list(dataFile = "./monolix/data_region_20200320.txt",
                       headerTypes =c("ignore","ignore","ignore","time","observation","ignore","regressor","id","regressor"),
                       observationTypes = list(cas_confirmes_incident="discrete")))

saveProject(projectFile = "./monolix/outputMonolix/sierah_poisson_proj.mlxtran")
scenario <- getScenario()
scenario$tasks = c(populationParameterEstimation = T, 
                   conditionalModeEstimation = T, 
                   conditionalDistributionSampling = T, 
                   standardErrorEstimation=T, 
                   logLikelihoodEstimation=T)
scenario$linearization = FALSE
setScenario(scenario)

### Check the stability of estimation
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
### Results are super stable
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
popparams <- getPopulationParameterInformation()
popparams$initialValue <- c(1.5254090,0.3526815,0.1792003,0.6511337)
setPopulationParameterInformation(popparams)
runScenario()
indivParams = as.data.frame(getEstimatedIndividualParameters(method="conditionalMode"))
names(indivParams)<-c("id","transmission","ascertainment")

data<-read.table("./monolix/data_region_20200320.txt",sep="\t",header=TRUE)
chiffres<-read.table("./monolix/Chiffres.txt",sep="\t",header=TRUE)

               
for (i in 1:length(indivParams$id)){
   i=6
   temp_monolix_estim<-seirah_estim(binit=indivParams[i,2:3], data=data[which(data$goodID==as.character(indivParams[i,1])),], data=data[which(data$goodID==as.character(indivParams[i,1])),],
                                    alpha=1,De=5.2,Di=2.3,Dq=10,Dh=30,
                                    popSize=chiffres[which(chiffres$goodid==as.character(indivParams[i,1])),"size"], dailyMove=0.10*chiffres[which(chiffres$goodid==as.character(indivParams[i,1])),"size"],
                                    verbose = TRUE)
   predict(temp_monolix_estim, threesholdICU = chiffres[which(chiffres$goodid==as.character(indivParams[i,1])),"nbICU"],verbose=TRUE)
   
   
}

