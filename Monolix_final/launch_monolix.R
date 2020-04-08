#install.packages("./monolix/lixoftConnectors.tar.gz", repos = NULL, type="source", INSTALL_opts ="--no-multiarch")
library(lixoftConnectors)
library(ggplot2)
initializeLixoftConnectors(software="monolix")

##########################
### LAUNCH FROM MONOLIX (recommended)
##########################

path<-"./Monolix_final/"
nameproject<-"final_nomove"
dataname<-"data_monolix_20200403.txt"
codename<- "monolix_Estimation_2periode_cov_final.txt"

newProject(modelFile = paste(path,codename,sep=""),
           data = list(dataFile = paste(path,dataname,sep=""),
                       headerTypes =c("ignore","ignore","time","regressor","regressor","obsid","observation","regressor","regressor","regressor","ignore","ignore","ignore","id"),
                       observationTypes = list(cas_confirmes_incident="discrete",hospitalisation_incident="discrete"),
                       mapping = list("1" = "cas_confirmes_incident", "2" = "hospitalisation_incident")))

dir.create(paste(path,"outputMonolix/",sep=""))
saveProject(projectFile = paste(path,"outputMonolix/",nameproject,".mlxtran",sep=""))
