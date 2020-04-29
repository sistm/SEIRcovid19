devtools::load_all('.')
Sys.setlocale("LC_NUMERIC","C")

# Define all paremeter for Ode system with initial value (0), including regressor parameter
param<-c(bbefore=2,
         boneweek=2,
         btwoweek=2,
         bthreeweek=2,
         bfourweek=2,
         r=0.033,
         alpha=0.55,
         De=5.2,
         Di=2.3,
         Dq=2,
         Dh=30,
         popsize=12278210,
         isolation=-100,
         timesinceconf=-100)
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
ParamVar<-list(bbefore=2,
               boneweek=2,
               btwoweek=2,
               bthreeweek=2,
               bfourweek=2,Dq=1)
myOde<-SetParamVariability(myOde,ParamVar)
# Param
InitVar<-list(initE=2)
myOde<-SetInitVariability(myOde,InitVar)
# Distribution of the parameter, default is logNormal
# Distribution as to be define for Parameter and IniState
#Param
Paramdist<-list(bbefore="logNormal")
myOde<-SetParamDistribution(myOde,Paramdist)
# Init (not used in actual example)
#Initdist<-list(initS="logNormal")
#myOde<-SetInitDistribution(myOde,Initdist)
# Regressor => 1 is regressor , it mean that this value is available in the data input
# Regressor as to be defined for parameter and init
# Param
ParamRegressor<-list(popsize=1,isolation=1,timesinceconf=1)
myOde<-SetParamIsRegressor(myOde,ParamRegressor)
myOde$IsRegressor$param
# Init
InitRegressor<-list(initH=1,initI=1)
myOde<-SetInitIsRegressor(myOde,InitRegressor)
myOde$IsRegressor$init
# Data input info
#path<-"./MonolixFile/data_monolix_20200403.txt"
path<-"./TestDAN/data_monolix_20200427.txt"
sep<-"\t"
header<-c("ignore","ignore","time","regressor","regressor","obsid","observation","regressor","regressor","ignore","id","regressor")

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
ModelFile<-paste('./TestDAN/','mlxmodel',".txt",sep="")
SpecificInitBloc<-c("A_0=I_0*(1-r)/r",
                    "S_0=popsize-E_0-I_0-R_0-A_0-H_0")
ModelStatBloc<-c("if (timesinceconf<7)",
   "before = 1",
"else",
   "before = 0",
"end",
"if (timesinceconf>=7) & (timesinceconf<14)",
   "oneweek = 1",
"else",
   "oneweek = 0",
"end",
"if (timesinceconf>=14) & (timesinceconf<21)",
   "twoweek = 1",
"else",
   "twoweek = 0",
"end",
"if (timesinceconf>=21) & (timesinceconf<28)",
   "threeweek = 1",
"else",
   "threeweek = 0",
"end",
"if (timesinceconf>=28) ",
   "fourweek = 1",
"else",
   "fourweek = 0",
"end",
"transmission=bbefore*before+boneweek*oneweek+btwoweek*twoweek+bthreeweek*threeweek+bfourweek*fourweek")
ModelMathBloc<-c("ddt_S = -transmission*S*(I+alpha*A)/popsize",
                 "ddt_E = transmission*S*(I+alpha*A)/popsize-E/De",
                 "ddt_I = r*E/De-I/Dq-I/Di",
                 "ddt_R = (I+A)/Di+H/Dh",
                 "ddt_A = (1-r)*E/De-A/Di",
                 "ddt_H = I/Dq-H/Dh")
ModelObservationBloc<-c("Isim=r*E/De",
                        "Hsim=I/Dq")
myOde<-WriteMonolixModel(myOde,ModelFile,SpecificInitBloc,ModelStatBloc,ModelMathBloc,ModelObservationBloc)

## Launch monolix
obs<-list(cas_confirmes_incident="discrete",hospitalisation_incident="discrete")
map<-list("1" = "cas_confirmes_incident", "2" = "hospitalisation_incident")
nameproject<-"LauchTestTESTDAN"
myOde<-LaunchMonolix.OdeSystem(myOde, nameproject, obs, map)
myOde$nameproject<-nameproject





# @Melanie ne pas aller plus loin



## Start get_result
path<-"/home/ddutartr/Projet/SISTM/SEIRcovid19/MonolixFile/"
nameproject<-"Final_20200325/"
myOde$nameproject<-nameproject

## Get Individual Parameters & data

index_id<-1
ode_id<-myOde
ode_id<-UpdateOdeSystem(ode_id,index_id,SpecificInitBloc)

pk.model<-'/home/ddutartr/Projet/SISTM/testminpuls/model_if.txt'
time<-seq(0,100, by=1)
resultat<-mlxtran_solve_simulx(pk.model,time,ode_id$parameter,ode_id$InitState,ode_id$ModelName)

pk.model<-'../testminpuls/seirah_lastIdea.R'
source(pk.model)
time<-seq(0,10, by=1)
par<-ode_id$parameter
par<-c(par,newdailyMove=0)
init<-ode_id$InitState
names(init)<-c("S","E","I","R","A","H")
model_name<-"SEIRAH"
res<-ode_solve_simulx(pk.model,time,par,init,model_name)


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

compare_res<-resultat-res 
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

