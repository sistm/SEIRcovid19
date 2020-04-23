devtools::load_all('.')
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
