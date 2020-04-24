devtools::load_all('.')
Sys.setlocale("LC_NUMERIC","C")

# Define all paremeter for Ode system with initial value (0), including regressor parameter
param<-c(b=0,
         ascertainement=0,
         alpha=0.5,
         De=5.1,
         Di=2.3,
         Dq=0,
         Dh=30,
         popsize=0,
         tconf=15,
         lengthconf=1000,
         dailyMove=0,
         isolation=0,
         beta1=0)# Regressor parameter
# Define all init state of the ODE systme with value, it should be initVar
init  <- c(initS=0, 
           initE=0,
           initI=0,
           initR=0,
           initA=0,
           initH=0)
# Model name
model_name<-c("S","E","I","R","A","H")
# Not used for now
ode_def<-seirah_ode

myOde<-OdeSystem(func=ode_def,param=param,init=init,modname = model_name)
myOde
# Définition spécifique pour les états initiaux => A et S
SpecificInit<-list(initA=1,initS=1)
myOde<-SetSpecificInit(myOde,SpecificInit)
myOde$IsSpecificInit
# Variability => 0 = fixed, 1 mobile sans effet aléatoire, 2 mobile avec effet aléatoire (ajout d'un beta)
## Variability has to be defined for Parameter and InitState
#Param
ParamVar<-list(b=2,Dq=2,beta1=1)
myOde<-SetParamVariability(myOde,ParamVar)
# Param
InitVar<-list(initE=2)
myOde<-SetInitVariability(myOde,InitVar)
# Distribution of the parameter, default is logNormal
# Distribution as to be define for Parameter and IniState
#Param
Paramdist<-list(beta1="normal")
myOde<-SetParamDistribution(myOde,Paramdist)
# Init (not used in actual example)
#Initdist<-list(initS="logNormal")
#myOde<-SetInitDistribution(myOde,Initdist)
# Regressor => 1 is regressor , it mean that this value is available in the data input
# Regressor as to be defined for parameter and init
# Param
ParamRegressor<-list(ascertainement=1,popsize=1,isolation=1)
myOde<-SetParamIsRegressor(myOde,ParamRegressor)
myOde$IsRegressor$param
# Init
InitRegressor<-list(initH=1,initI=1)
myOde<-SetInitIsRegressor(myOde,InitRegressor)
myOde$IsRegressor$init
# Data input info
path<-"./MonolixFile/data_monolix_20200403.txt"
sep<-"\t"
header<-c("ignore","ignore","time","regressor","regressor","obsid","observation","regressor","regressor","regressor","ignore","ignore","ignore","id")
myOde<-SetDataInput(myOde,path,header,sep)
myOde$DataInfo

## TODO add a print method
# Print will show
# - Parameter
# - InitState
# - Who is optimisable and their variability (1/2)
# - Who is regressor

#Write mlxtran
# We need several info :
# - The Specific definition of the init state
# - The statistical model
# - The mathematical model
# - The observation model
## TODO keep this info for the estimation (specific,stat,math,obs)
ModelFile<-paste('./MonolixFile/','mlxmodel',".txt",sep="")
SpecificInitBloc<-c("A_0=I_0*(1-ascertainement)/ascertainement",
                    "S_0=popsize-E_0-I_0-R_0-A_0-H_0")
ModelStatBloc<-c("transmission=b*exp(beta1*isolation)")
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
nameproject<-"Final_20200325/"
dataname<-"./MonolixFile/data_monolix_20200325.txt"
myOde$DataInfo$File<-dataname
myOde$DataInfo$HeaderType<-c("ignore","ignore","time","regressor","regressor","obsid","observation","regressor","regressor","regressor","id","regressor","ignore","ignore")
## Get Individual Parameters & data
indivParams <-read.table(paste(path,"/outputMonolix/",nameproject,"/IndividualParameters/estimatedIndividualParameters.txt",sep=""),header=TRUE,sep=",")

data<-read.table(myOde$DataInfo$File,sep=myOde$DataInfo$Sep,header=TRUE)
RegressorName<-GetRegressorName(myOde)
InputNames<-colnames(data)
timename<-InputNames[myOde$DataInfo$HeaderType=="time"]
idname<-InputNames[myOde$DataInfo$HeaderType=="id"]

for (i in 1:1){#length(indivParams$id)){
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
  optimize_param_name<-c(names(ode_id$parameter[ode_id$Variability$param>0]),names(ode_id$InitState[ode_id$Variability$init>0]))
  OptimizeParam<-as.list(rep(NA,length(optimize_param_name)))
  names(OptimizeParam)<-optimize_param_name
  for (j in 1:length(optimize_param_name)){
    optimize_monolix_name<-paste(optimize_param_name[j],"_mode",sep="")
    OptimizeParam[j]<-indivParams[i,optimize_monolix_name]
  }
  ode_id<-SetSomeParameter(ode_id,OptimizeParam)
  ode_id$parameter
  ode_id<-SetSomeInit(ode_id,OptimizeParam)
  ode_id$InitState
  ode_id<-SetSpecificInitState(ode_id,SpecificInitBloc)
  
}

ode_id$parameter
ode_id$InitState

pk.model<-'/home/ddutartr/Projet/SISTM/testminpuls/model_if.txt'
time<-seq(0,365, by=1)
resultat<-mlxtran_solve_simulx(pk.model,time,ode_id$parameter,ode_id$InitState,ode_id$ModelName)



