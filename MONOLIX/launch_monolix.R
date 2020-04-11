#install.packages("./monolix/lixoftConnectors.tar.gz", repos = NULL, type="source", INSTALL_opts ="--no-multiarch")
library(lixoftConnectors)
library(ggplot2)
initializeLixoftConnectors(software="monolix")

##########################
### LAUNCH FROM MONOLIX PARAMETRICS (recommended)
##########################

path<-"./MONOLIX/"
nameproject<-"Final_20200325"
dataname<-"data_monolix_20200325.txt"
codename<- "monolix_constant.txt"
dir.create(paste(path,"outputMonolix/",sep=""))

newProject(modelFile = paste(path,codename,sep=""),
           data = list(dataFile = paste(path,dataname,sep=""),
                       headerTypes =c("ignore","ignore","time","regressor","regressor","obsid","observation","regressor","regressor","regressor","id","regressor","ignore","ignore"),
                       observationTypes = list(cas_confirmes_incident="discrete",hospitalisation_incident="discrete"),
                       mapping = list("1" = "cas_confirmes_incident", "2" = "hospitalisation_incident")))

# Set the distrution to normal for b1
setIndividualParameterDistribution(betat1="normal")
# Check the modification
#indivModel = getIndividualParameterModel()
#indivModel$distribution
setIndividualParameterVariability(betat1 = FALSE)

# Save the Project
saveProject(projectFile = paste(path,"outputMonolix/",nameproject,".mlxtran",sep=""))

# set tasks in scenario
scenario <- getScenario()
scenario$tasks = c(populationParameterEstimation = T, 
                   conditionalModeEstimation = T, 
                   conditionalDistributionSampling = T, 
                   standardErrorEstimation=T, 
                   logLikelihoodEstimation=T)
scenario$linearization = FALSE
setScenario(scenario)
runScenario()

## GET ALL THE RELEVANT INDICATORS
pop_mean<-getEstimatedPopulationParameters()
pop_se<-getEstimatedStandardErrors()$stochasticApproximation
indivparm<-getEstimatedIndividualParameters()
LL<-getEstimatedLogLikelihood()



##########################
### LAUNCH FROM MONOLIX PARAMETRICS (recommended)
##########################

path<-"./MONOLIX/"
nameproject<-"update20200410"
dataname<-"data_monolix_20200410.txt"
codename<- "monolix_constant.txt"
dir.create(paste(path,"outputMonolix/",sep=""))

newProject(modelFile = paste(path,codename,sep=""),
           data = list(dataFile = paste(path,dataname,sep=""),
                       headerTypes =c("ignore","ignore","time","regressor","regressor","obsid","observation","regressor","regressor","regressor","id","regressor","ignore","ignore"),
                       observationTypes = list(cas_confirmes_incident="discrete",hospitalisation_incident="discrete"),
                       mapping = list("1" = "cas_confirmes_incident", "2" = "hospitalisation_incident")))

# Set the distrution to normal for b1
setIndividualParameterDistribution(betat1="normal")
# Check the modification
#indivModel = getIndividualParameterModel()
#indivModel$distribution
setIndividualParameterVariability(betat1 = FALSE)
setPopulationParameterInformation(b1_pop = list(initialValue = 0.808, method = "FIXED"), 
                                  Dq_pop  = list(initialValue = 1.107, method = "FIXED"), 
                                  E0_pop = list(initialValue = 1231, method = "FIXED"),
                                  A0_pop = list(initialValue = 189, method = "FIXED"))# Save the Project
saveProject(projectFile = paste(path,"outputMonolix/",nameproject,".mlxtran",sep=""))

# set tasks in scenario
scenario <- getScenario()
scenario$tasks = c(populationParameterEstimation = T, 
                   conditionalModeEstimation = T, 
                   conditionalDistributionSampling = T, 
                   standardErrorEstimation=T, 
                   logLikelihoodEstimation=T)
scenario$linearization = FALSE
setScenario(scenario)
runScenario()

## GET ALL THE RELEVANT INDICATORS
pop_mean<-getEstimatedPopulationParameters()
pop_se<-getEstimatedStandardErrors()$stochasticApproximation
indivparm<-getEstimatedIndividualParameters()
LL<-getEstimatedLogLikelihood()





