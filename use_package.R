devtools::load_all('.')
Sys.setlocale("LC_NUMERIC","C")

# Define all paremeter for Ode system with initial value (0), including regressor parameter
param<-c(bbefore=2,
         boneweek=2,
         btwoweek=2,
         bthreeweek=2,
         bfourweek=2,
         r=10000,
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
           initE=500,
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
ParamRegressor<-list(r=1,popsize=1,isolation=1,timesinceconf=1)
myOde<-SetParamIsRegressor(myOde,ParamRegressor)
myOde$IsRegressor$param
# Init
InitRegressor<-list(initH=1,initI=1)
myOde<-SetInitIsRegressor(myOde,InitRegressor)
myOde$IsRegressor$init
# Data input info
#path<-"./MonolixFile/data_monolix_20200403.txt"
path<-"./MonolixFile/data_monolix_20200427.txt"
sep<-"\t"
#header<-c("ignore","ignore","time","regressor","regressor","obsid","observation","regressor","regressor","regressor","ignore","ignore","ignore","id")
header<-c("ignore","ignore","time","regressor","regressor","obsid","observation","regressor","regressor","regressor","id","regressor")

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
nameproject<-"Model4WeekPrior"
start_time<-Sys.time()

prior_mean<-list(initE = 1200,bbefore=2.1)
prior_sd<-list(initE = 300,bbefore=0.5)
PopInitValue<-list(boneweek=1.3)
#devtools::load_all('.')
myOde<-LaunchMonolix(myOde, nameproject, obs, map,runToBeDone=FALSE,prior_mean,prior_sd,PopInitValue)
end_time<-Sys.time()
start_time-end_time
myOde$nameproject<-nameproject


# Not used any more
#EstiomationRegressor<-list(isolation=1,timesinceconf=1)
#myOde<-SetParamEstimationRegressor(myOde, EstiomationRegressor)
#myOde$EstimationRegressor$param


# @Melanie : ATTENTION Cela ne fonctionne que pour pour resolution globale
is_global<-0
ModeFilename<-"model_estimation.txt"

# On rajoute ici l'équation du R0
TimeSpecificEquation<-c(ModelStatBloc,"R0=Di*transmission/(A+I)*(alpha*A+Dq*I/(Di+Dq))")
TimeDependantParameter<-c("transmission","R0")
#TimeDependantParameter => Nom des paramètres temps-dépendant à récupérer lors de l'estimation

ode_id<-ComputeEstimationAllId(myOde,ModeFilename,TimeSpecificEquation,SpecificInitBloc,ModelMathBloc,1,TimeDependantParameter)
ode_id[[1]]$solution



nb_mc <- 100
# Global =1 for IC
is_global<-1
TimeDependantParameter<-c("transmission","Dq")

# Attention il est nécessaire que pour l'estimation des intervalles de confiance pour un parametre "temps-dépendant"
# qu'il ait été au préalable estimer via "ComputeEstimationAllId" et son option "TimeDependantParameter"
ode_id<-ComputeConfidenceIntervalAllId(ode_id,nb_mc,is_global,TimeDependantParameter)

# Plot fitted value over observation

## For plot over observation data
# Data used for plot are store in ""ObsSimu" attribute
is_normalize<-1
ode_id<-PlotSolutionOverObservation(ode_id,ModelObservationBloc,is_normalize)
# R0 plot
R0_formula<-"Di*transmission/(A+I)*(alpha*A+Dq*I/(Di+Dq))"
R0min_formula<-"Di*transmission_min/(A_max+I_max)*(alpha*A_min+Dq_min*I_min/(Di+Dq_max))"
R0max_formula<-"Di*transmission_max/(A_min+I_min)*(alpha*A_max+Dq_max*I_max/(Di+Dq_min))"
ode_id<-PlotR0(ode_id,R0_formula,R0min_formula,R0max_formula)

## LONG TERM ANALYSIS

time_date<-seq(as.Date("2020-03-02"), as.Date("2020-06-30"), "days")
time<-seq(0,length(time_date)-1,1)
x<-list()
x[[1]] <- list(name='timesinceconf',
               time=time,
               value=rep(0,length(time)))
x[[1]]$value[17:length(time)]<-seq(1,length(time)-17+1,1)

reg_info<-x

ode_id<-EstimateLongTerm(ode_id,time_date,time,reg_info)

plot_info<-PlotSolutionLongTerm(ode_id)

## TABLE
popsize_name<-"popsize"
systemode<-ode_id[[1]]
indivParams <-read.table(paste(here::here(),'/MonolixFile/',"/outputMonolix/",systemode$nameproject,"/IndividualParameters/estimatedIndividualParameters.txt",sep=""),header=TRUE,sep=",")
popParams<-read.table(paste(here::here(),'/MonolixFile/',"/outputMonolix/",systemode$nameproject,"/populationParameters.txt",sep=""),header=TRUE,sep=",")
optimize_param_name<-c(names(systemode$parameter[systemode$Variability$param>0]),names(systemode$InitState[systemode$Variability$init>0]))
SdOptimizeParam<-as.data.frame(matrix(NA,length(ode_id),length(optimize_param_name)))
colnames(SdOptimizeParam)<-optimize_param_name
rownames(SdOptimizeParam)<-indivParams$id
OptimizeParam<-as.data.frame(matrix(NA,length(ode_id),length(optimize_param_name)))
colnames(OptimizeParam)<-optimize_param_name
rownames(OptimizeParam)<-indivParams$id
TableParam<-OptimizeParam
popsize_per_id<-rep(0,length(ode_id))
for (id in 1:length(ode_id)){
  popsize_per_id[id]<-ode_id[[id]]$parameter[names(ode_id[[id]]$parameter)==popsize_name]
  for (j in 1:length(optimize_param_name)){
    optimize_monolix_name<-paste(optimize_param_name[j],"_sd",sep="")
    SdOptimizeParam[id,j]<-indivParams[id,optimize_monolix_name]
    optimize_monolix_name<-paste(optimize_param_name[j],"_mode",sep="")
    OptimizeParam[id,j]<-indivParams[id,optimize_monolix_name]
    if (SdOptimizeParam[id,j]==0){
      optimize_pop_name<-paste(optimize_param_name[j],"_pop",sep="")
      SdOptimizeParam[id,j]<-popParams[optimize_pop_name,"stochasticApproximation"]
    }
    OptimizeParamMin<-format(round(OptimizeParam[id,j]-1.96*SdOptimizeParam[id,j], 2), nsmall = 2)
    OptimizeParamMax<-format(round(OptimizeParam[id,j]+1.96*SdOptimizeParam[id,j], 2), nsmall = 2)
    TableParam[id,j]<-paste(format(round(OptimizeParam[id,j],2), nsmall = 2)," [",OptimizeParamMin,";",OptimizeParamMax,"]",sep="")
  }
}
for (j in 1:length(optimize_param_name)){
  TableParam[dim(OptimizeParam)[1]+1,j]<-paste(format(round(sum(OptimizeParam[,j]*(popsize_per_id))/sum(popsize_per_id),2),nsmall=0)," [",
                                               format(round(sum((OptimizeParam[,j]-1.96*SdOptimizeParam[,j])*(popsize_per_id))/sum(popsize_per_id),2),nsmall=0),";",
                                               format(round(sum((OptimizeParam[,j]+1.96*SdOptimizeParam[,j])*(popsize_per_id))/sum(popsize_per_id),2),nsmall=0),"]",sep="")
}
rownames(TableParam)<-c(rownames(OptimizeParam),"France")

TableParam$Reg<-row.names(TableParam)

print(xtable::xtable(TableParam[,c("Reg",optimize_param_name)]),include.rownames = FALSE,
      file = paste(here::here(),'/MonolixFile/',"/outputMonolix/",systemode$nameproject,"/TableOptimizeParameter.txt",sep=""))


fileConn<-file("table.txt")
writeLines(print(xtable::xtable(TableParam[,c("Reg",optimize_param_name)]),include.rownames = FALSE), fileConn)
close(fileConn)

