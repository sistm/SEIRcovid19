devtools::load_all('.')
Sys.setlocale("LC_NUMERIC","C")

# Define all paremeter for Ode system with initial value (0), including regressor parameter
param<-c(b=0,
         ascertainement=0,
         alpha=0.55,
         De=5.1,
         Di=2.3,
         Dq=0,
         Dh=30,
         popsize=0,
         tconf=15,
         lengthconf=1000,
         dailyMove=0,
         isolation=0,
         beta1=0,
         beta3=0)# Regressor parameter
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
#path<-"./MonolixFile/data_monolix_20200403.txt"
path<-"./MonolixFile/data_monolix_20200325.txt"
sep<-"\t"
#header<-c("ignore","ignore","time","regressor","regressor","obsid","observation","regressor","regressor","regressor","ignore","ignore","ignore","id")
header<-c("ignore","ignore","time","regressor","regressor","obsid","observation","regressor","regressor","regressor","id","ignore","ignore","ignore")

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
nameproject<-"LaunchTest"
myOde<-LaunchMonolix.OdeSystem(myOde, nameproject, obs, map,runToBeDone=TRUE)
myOde$nameproject<-nameproject

# @Melanie ne pas aller plus loin
## We want now to update the system after optimisation
# Use the result from melanie
nameproject<-"Final_20200325/"
myOde$nameproject<-nameproject
#Updating ofr id<-1 => IDF
index_id<-1
ode_id<-myOde
ode_id<-UpdateOdeSystem(ode_id,index_id,SpecificInitBloc)
ode_id$parameter
ode_id$InitState
# Write monolix model for estimation
ModeFilename<-"model_estimation.txt"
TimeSpecificEquation<-c("transmission=b",
                        "if (t>=tconf)",
                        "  transmission=b*exp(beta1)",
                        "end")

ode_id<-WriteEstimationModel(ode_id,ModeFilename,TimeSpecificEquation,ModelMathBloc)

time<-seq(0,100, by=1)
is_global<-0

ode_id<-Estimate(ode_id, time,is_global)
resultat<-ode_id$solution

library(ggplot2)
melanie<-read.table("/home/ddutartr/Projet/SISTM/testminpuls/SolutionIDF.txt",header=TRUE)
melanie<-melanie[melanie$time<101,c('time',"S","E","I","R","A","H")]
compare_res<-resultat-melanie 
compare_res$time<-resultat$time

S <- ggplot(compare_res, aes(x=time) ) +
  geom_line(aes(y = S), color = "darkred") 
E <- ggplot(compare_res, aes(x=time) ) +
  geom_line(aes(y = E), color = "darkred") 
I <- ggplot(compare_res, aes(x=time) ) +
  geom_line(aes(y = I), color = "darkred") 
R <- ggplot(compare_res, aes(x=time) ) +
  geom_line(aes(y = R), color = "darkred") 
A <- ggplot(compare_res, aes(x=time) ) +
  geom_line(aes(y = A), color = "darkred") 
H <- ggplot(compare_res, aes(x=time) ) +
  geom_line(aes(y = H), color = "darkred") 

cowplot::plot_grid(S, E,I,R,A,H,labels=c("S","E","I","R","A","H"), ncol = 2, nrow = 3)
