#install.packages("./monolix/lixoftConnectors.tar.gz", repos = NULL, type="source", INSTALL_opts ="--no-multiarch")
library(lixoftConnectors)
library(ggplot2)
initializeLixoftConnectors(software="monolix")

######### INITIAL PROBLEM THAT WORKS -- 1 PERIOD
path<-"./Monolix_incidents/"
nameproject<-"OnePeriod"
dataname<-"data_monolix_20200330.txt"
codename<- "monolix_Estimation_1periode.txt"
    

datanewProject(modelFile = paste(path,codename,sep=""),
            data = list(dataFile = paste(path,dataname,sep=""),
                        headerTypes =c("ignore","ignore","time","regressor","regressor","obsid","observation","ignore","regressor","regressor","ignore","ignore","ignore","id"),
                        observationTypes = list(cas_confirmes_incident="discrete",hospitalisation_incident="discrete"),
                        mapping = list("1" = "cas_confirmes_incident", "2" = "hospitalisation_incident")))
 
dir.create(paste(path,"outputMonolix/",sep=""))
saveProject(projectFile = paste(path,"outputMonolix/",nameproject,".mlxtran",sep=""))



######### INITIAL PROBLEM THAT WORKS -- 2 PERIOD
path<-"./Monolix_incidents/"
nameproject<-"TwoPeriod"
dataname<-"data_monolix_20200330.txt"
codename<- "monolix_Estimation_2periode.txt"


newProject(modelFile = paste(path,codename,sep=""),
           data = list(dataFile = paste(path,dataname,sep=""),
                       headerTypes =c("ignore","ignore","time","regressor","regressor","obsid","observation","regressor","regressor","regressor","ignore","ignore","ignore","id"),
                       observationTypes = list(cas_confirmes_incident="discrete",hospitalisation_incident="discrete"),
                       mapping = list("1" = "cas_confirmes_incident", "2" = "hospitalisation_incident")))

dir.create(paste(path,"outputMonolix/",sep=""))
saveProject(projectFile = paste(path,"outputMonolix/",nameproject,".mlxtran",sep=""))



