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
myOde<-LaunchMonolix.OdeSystem(myOde, nameproject, obs, map,runToBeDone=FALSE)

myOde$nameproject<-nameproject

# @Melanie ne pas aller plus loin
## We want now to update the system after optimisation
# Use the result from melanie
#nameproject<-"Final_20200325/"
#myOde$nameproject<-nameproject
#Updating ofr id<-1 => IDF
time<-seq(0,100, by=1)
is_global<-0
ModeFilename<-"model_estimation.txt"
TimeSpecificEquation<-c("transmission=b",
                        "if (t>=tconf)",
                        "  transmission=b*exp(beta1)",
                        "end")
ode_id<-ComputeEstimationAllId(myOde,time,ModeFilename,TimeSpecificEquation,SpecificInitBloc,ModelMathBloc,is_global)
# Confidence interval
# Number of monte carlo simulation
nb_mc <- 20
# Global =1 for IC
is_global<-1
ode_id<-ComputeConfidenceIntervalAllId(ode_id,time,nb_mc,is_global)

ode<-ode_id[[1]]
## For plot

a<-"Isim=ascertainement*E/De"
State<-data.frame(matrix(ncol = 1, nrow = dim(ode$solution)[1]))
GetStatWithExp<-function(solution,parameter,exp,name){
  #State<-data.frame(matrix(ncol = 1, nrow = dim(solution)[1]))
  with(as.list(c(solution,parameter)),{
    State<-data.frame((eval(parse(text=exp))))
    names(State)<-name
    return(State)
  })
}
b<-GetStatWithExp(ode$solution,ode$parameter,a,"Isim")
c<-GetStatWithExp(ode$ICmin,ode$parameter,a,"Isim_min")
d<-GetStatWithExp(ode$ICmax,ode$parameter,a,"Isim_max")
e<-data.frame(ode$solution$time)
names(e)<-"day"
data<-read.table(ode$DataInfo$File,sep=ode$DataInfo$Sep,header=TRUE)
InputNames<-colnames(data)
timename<-InputNames[ode$DataInfo$HeaderType=="time"]
idname<-InputNames[ode$DataInfo$HeaderType=="id"]
obs<-data

tot<-cbind(b,c,d,e)

temp<-data[which((data$obs_id==1) & (data$IDname=='IDF')),c("day","obs")]
data2plot <- merge(temp, tot, by = "day")


p<-ggplot(data2plot, aes(x=day)) +
  geom_point(aes(y = obs, color = "Observed")) +
  geom_line(aes(y = Isim, color = "SEIRAH")) +
  geom_ribbon(aes(ymin=Isim_min, ymax=Isim_max,fill="SEIRAH",alpha=0.5)) +
  theme_classic() + ylab("Number of incident cases") +
  scale_color_manual("", values=c("black", "blue"))
p