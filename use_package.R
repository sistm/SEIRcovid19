devtools::load_all('.')
Sys.setlocale("LC_NUMERIC","C")
param<-c(b=0.838434,
         ascertainement=0.0431010769979476,
         alpha=1.5,
         De=5.2,
         Di=2.3,
         Dq=2.37556,
         Dh=30,
         popsize=12278210,
         dailyMove=0,
         isolation=0)
init  <- c(initS=12274535.42, 
           initE=1702.34,
           initI=25.00,
           initR=0,
           initA=1942.24,
           initH=5)
model_name<-c("S","E","I","R","A","H")
ode_def<-seirah_ode


myOde<-OdeSystem(func=ode_def,param=param,init=init,modname = model_name)
myOde



# Définition spécifique pour les états initiaux => A et S
SpecificInit<-list(initA=1,initS=1)
myOde<-SetSpecificInit(myOde,SpecificInit)
myOde$IsSpecificInit

# Variabily => 0 = fixed, 1 mobile sans effet aléatoire, 2 mobile avec effet aléatoire (ajout d'un beta)
ParamVar<-list(b=2,Dq=1)
myOde<-SetParamVariability(myOde,ParamVar)
myOde$Variability$param
InitVar<-list(initE=1)
myOde<-SetInitVariability(myOde,InitVar)
myOde$Variability$init

# Distribution : Dans notre cas tout les param/init ont des distrus logNormal, sauf pour le beta
Paramdist<-list(b="logNormal")
myOde<-SetParamDistribution(myOde,Paramdist)
myOde$Distribution$param
Initdist<-list(initS="logNormal")
myOde<-SetInitDistribution(myOde,Initdist)
myOde$Distribution$init
#Regressor
ParamRegressor<-list(ascertainement=1,popsize=1,isolation=1)
myOde<-SetParamIsRegressor(myOde,ParamRegressor)
myOde$IsRegressor$param
InitRegressor<-list(initH=1,initI=1)
myOde<-SetInitIsRegressor(myOde,InitRegressor)
myOde$IsRegressor$init
# Data input info
path<-"./MonolixFile/data_monolix_20200403.txt"
sep<-"\t"
header<-c("ignore","ignore","time","regressor","regressor","obsid","observation","regressor","regressor","regressor","ignore","ignore","ignore","id")
myOde<-SetDataInput(myOde,path,header,sep)
myOde$DataInfo


#Write mlxtran
ModelFile<-paste('./MonolixFile/','mlxmodel',".txt",sep="")
SpecificInitBloc<-c("A_0=I_0*(1-ascertainement)/ascertainement",
                    "S_0=popsize-E_0-I_0-R_0-A_0-H_0")
ModelStatBloc<-c("transmission=b1*exp(beta1*isolation)")
ModelMathBloc<-c("ddt_S = -transmission*S*(I+alpha*A)/popsize+dailyMove-dailyMove*S/(popsize-I-H)",
                 "ddt_E = transmission*S*(I+alpha*A)/popsize-E/De-dailyMove*E/(popsize-I-H)",
                 "ddt_I = ascertainement*E/De-I/Dq-I/Di",
                 "ddt_R = (I+A)/Di+H/Dh-dailyMove*R/(popsize-I-H)",
                 "ddt_A = (1-ascertainement)*E/De-A/Di-dailyMove*A/(popsize-I-H)",
                 "ddt_H = I/Dq-H/Dh")
ModelObservationBloc<-c("Isim=ascertainement*E/De",
                        "Hsim=I/Dq")
myOde<-WriteMonolixModel(myOde,ModelFile,SpecificInitBloc,ModelStatBloc,ModelMathBloc,ModelObservationBloc)

## Launch monolix
obs<-list(cas_confirmes_incident="discrete",hospitalisation_incident="discrete")
map<-list("1" = "cas_confirmes_incident", "2" = "hospitalisation_incident")

myOde<-LaunchMonolix.OdeSystem(myOde, "LaunchTest", obs, map)



# @Melanie ne pas aller plus loin



## Start get_result
path<-"/home/ddutartr/Projet/SISTM/SEIRcovid19/MonolixFile/"
nameproject<-"final/"
dataname<-"./MonolixFile/data_monolix_20200403.txt"
## Get Individual Parameters & data
indivParams <-read.table(paste(path,"/outputMonolix/",nameproject,"/IndividualParameters/estimatedIndividualParameters.txt",sep=""),header=TRUE,sep=",")

data<-read.table(myOde$DataInfo$File,sep=myOde$DataInfo$Sep,header=TRUE)
RegressorName<-GetRegressorName(myOde)
InputNames<-colnames(data)
timename<-InputNames[myOde$DataInfo$HeaderType=="time"]
idname<-InputNames[myOde$DataInfo$HeaderType=="id"]

for (i in 1:length(indivParams$id)){
  ode_id<-myOde
  RegressorValue<-as.list(rep(NA,length(RegressorName)))
  names(RegressorValue)<-(RegressorName)
  # Set the regressor value thanks to data input
  for (j in 1:length(RegressorName)){
    RegressorValue[j]<-data[which(data[idname]==as.character(indivParams$id[i]) & (data[timename]==0)) ,RegressorName[j]][1]
  }
  ode_id$InitState
  ode_id<-SetSomeInit(ode_id,RegressorValue)
  ode_id$InitState
  ode_id$parameter
  ode_id<-SetSomeParameter(ode_id,RegressorValue)
  ode_id$parameter
  # Set the parameter name thanks to monolix
  optimize_param_name<-c(names(ode_id$parameter[ode_id$Variability$param>0]),names(ode_id$ParamRandomEffect))
  OptimizeParam<-as.list(rep(NA,length(optimize_param_name)))
  names(OptimizeParam)<-optimize_param_name
  for (j in 1:length(optimize_param_name)){
    optimize_monolix_name<-paste(optimize_param_name[j],"_mode",sep="")
    OptimizeParam[j]<-indivParams[i,optimize_monolix_name]
  }
  ode_id<-SetSomeParameter(ode_id,OptimizeParam)
  ode_id$parameter
  
}