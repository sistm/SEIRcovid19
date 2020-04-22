#install.packages("/home/ddutartr/Lixoft/MonolixSuite2019R2/lixoftConnectors.tar.gz", repos = NULL, type="source", INSTALL_opts ="--no-multiarch")
library(lixoftConnectors)
library(ggplot2)
initializeLixoftConnectors(software="monolix")

##########################
### LAUNCH FROM MONOLIX (recommended)
##########################

path<-"/home/ddutartr/Projet/SISTM/SEIRcovid19/Monolix_final/"
nameproject<-"final"
dataname<-"data_monolix_20200403.txt"
codename<- "monolix_Estimation_2periode_cov.txt"
codename<-"mlxmodel.txt"
p<-newProject(modelFile = paste(path,codename,sep=""),
           data = list(dataFile = paste(path,dataname,sep=""),
                       headerTypes =c("ignore","ignore","time","regressor","regressor","obsid","observation","regressor","regressor","regressor","ignore","ignore","ignore","id"),
                       observationTypes = list(cas_confirmes_incident="discrete",hospitalisation_incident="discrete"),
                       mapping = list("1" = "cas_confirmes_incident", "2" = "hospitalisation_incident")))
#Get the parameter of Individual variable
indivModel = getIndividualParameterModel()
# Check the distribution
indivModel$distribution
# Set the distrution to normal for b1
var_name<-indivModel$name[1]

mlxProject.setIndividualParameterDistribution <- function(a) {
  #.hiddenCall(paste0('r <- lixoftConnectors::setIndividualParameterDistribution(',a,'= "normal")'))
  eval.parent(parse(text =paste0('r <- lixoftConnectors::setIndividualParameterDistribution(',a,'= "normal")' )))
}
mlxProject.setIndividualParameterDistribution(var_name)
indivModel = getIndividualParameterModel()
indivModel$distribution



#setIndividualParameterDistribution(b1="normal")
#setIndividualParameterDistribution("b1"="normal")
# Check the modification
indivModel = getIndividualParameterModel()
indivModel$distribution

# Save the Project
saveProject(projectFile = paste(path,"outputMonolix/",'final',".mlxtran",sep=""))

#Load the project
loadProject(projectFile = paste(path,"outputMonolix/",'final',".mlxtran",sep=""))
#Confirm the modification of the distribution
indivModel = getIndividualParameterModel()
indivModel$distribution
