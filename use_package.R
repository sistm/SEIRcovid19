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
myOde<-LaunchMonolix(myOde, nameproject, obs, map,runToBeDone=TRUE,prior_mean,prior_sd,PopInitValue)
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




ode<-ode_id[[6]]

time_date<-seq(as.Date("2020-03-02"), as.Date("2020-06-30"), "days")
time<-seq(0,length(time_date)-1,1)
x<-list()
x[[1]] <- list(name='timesinceconf',
               time=time,
               value=rep(0,length(time)))
x[[1]]$value[17:length(time)]<-seq(1,length(time)-17+1,1)

reg_info<-x


indivParams <-read.table(paste(here::here(),'/MonolixFile/',"/outputMonolix/",ode$nameproject,"/IndividualParameters/estimatedIndividualParameters.txt",sep=""),header=TRUE,sep=",")
data<-read.table(ode$DataInfo$File,sep=ode$DataInfo$Sep,header=TRUE)
# Get the data header interessant name
InputNames<-colnames(data)
timename<-InputNames[ode$DataInfo$HeaderType=="time"]
idname<-InputNames[ode$DataInfo$HeaderType=="id"]
ObsIdName<-InputNames[ode$DataInfo$HeaderType=="obsid"]
ObservationName<-InputNames[ode$DataInfo$HeaderType=="observation"]
RegressorNames<-GetRegressorName(ode)

reg_ode<-reg_info
index<-which(time_date==min(as.Date(ode$ObsData$date)))
for (i in 1:length(reg_ode)){
  
  reg_ode[[i]]$time<-reg_ode[[i]]$time[index:length(time_date)]
  reg_ode[[i]]$value<-reg_ode[[i]]$value[index:length(time_date)]
  
}
reg_ode[[i]]$value
time_ode<-time[index:length(time_date)]

ode<-WriteEstimationModelLongTerm(ode, ModeFilename, TimeSpecificEquation, ModelMathBloc,reg_info)

param_and_init<-c(ode$parameter,ode$InitState)
pk.model<-ode$ModelFileEstimationLongTerm
regressor_info<-reg_ode
# mlxR format
TimeDependantParameter<-c("transmission","timesinceconf")

C <- list(name=c(ode$ModelName,TimeDependantParameter), time=time_ode)
#solution <- mlxR::simulx(model     = pk.model, output    = C,parameter = param_and_init)
solution <- mlxR::simulx(model     = pk.model, output    = C,parameter = param_and_init,regressor=reg_ode)
#Output format of mlxR is : time obs_1,time obs_2 ... time obs_n
result<-as.data.frame(solution)
result<-result[,c(1,seq(2,(length(ode$ModelName)+length(TimeDependantParameter))*2,by=2))]
colnames(result)<-c("time",ode$ModelName,TimeDependantParameter)
result
ode_id[[6]]$TimeDependantParameter
ode_id[[6]]$solution

# Trajectorie Plot
solutions_list <- list()
solutionmin<-list()
solutionmax<-list()
for (id in 1:length(ode_id)){
  solution <- ode_id[[id]]$solution
  solution$date <- seq.Date(from =as.Date(ode_id[[id]]$ObsData$date[1]), by = 1, length.out = nrow(ode_id[[id]]$solution))
  solution$popsize <- ode_id[[id]]$parameter[names(ode_id[[id]]$parameter)=='popsize']

  solutions_list[[id]] <- solution
  solutions_list[[id]]$reg<-as.character(indivParams$id[id])
  
  solutionmin[[id]]<-ode_id[[id]]$ICmin
  solutionmin[[id]]$date <- solution$date
  solutionmin[[id]]$popsize <- solution$popsize
  solutionmin[[id]]$reg <- as.character(indivParams$id[id])
  
  solutionmax[[id]]<-ode_id[[id]]$ICmax
  solutionmax[[id]]$date <- solution$date
  solutionmax[[id]]$popsize <- solution$popsize
  solutionmax[[id]]$reg <- as.character(indivParams$id[id])
  
}


solutions_allsim <- do.call(rbind.data.frame,solutions_list) %>% select(!c("time")) %>% reshape2::melt(id.vars=c("date", "reg", "popsize"))
solutions_allmin <- do.call(rbind.data.frame,solutionmin)  %>% reshape2::melt(id.vars=c("date", "reg","popsize"), value.name  = "value.min")
solutions_allmax <- do.call(rbind.data.frame,solutionmax) %>% reshape2::melt(id.vars=c("date", "reg","popsize"), value.name  = "value.max")
solutions_2plot <- cbind.data.frame(solutions_allsim,
                                           "value.min" = solutions_allmin$value.min,
                                           "value.max" = solutions_allmax$value.max)

p <- ggplot(solutionsREBOUND_2plot, aes(fill=reg, x=date)) +
  geom_line(aes(y=value/popsize, colour = reg)) +
  geom_ribbon(aes(ymin=value.min/popsize, ymax=value.max/popsize), alpha = 0.3) +
  geom_vline(aes(xintercept=as.Date("2020-03-17"), linetype="Lockdown start")) +
  geom_vline(aes(xintercept=as.Date("2020-05-11"),  linetype="Lockdown lift")) +
  scale_linetype_manual("", values=c(2,3), breaks=c("Lockdown start", "Lockdown lift")) +
  xlim(c(as.Date(min(solutionsREBOUND_2plot$date)), as.Date(max(solutionsREBOUND_2plot$date)))) +
  theme_bw() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.01)) +
  facet_wrap(~variable, scales="free_y", ncol=2) +
  ylab("Proportion of region population") +
  xlab("Date") +
  colorspace::scale_color_discrete_qualitative(name = "Region", palette = "Dark3") +
  colorspace::scale_fill_discrete_qualitative(name = "Region", palette = "Dark3") +
  theme(legend.text = element_text(size = 8),
        legend.title = element_text(size = 10)) +
  theme(legend.position = "bottom", legend.box = "vertical")
p