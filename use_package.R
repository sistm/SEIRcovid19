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
nameproject<-"Model4Week"
myOde<-LaunchMonolix.OdeSystem(myOde, nameproject, obs, map,runToBeDone=FALSE)



myOde$nameproject<-nameproject

#EstiomationRegressor<-list(isolation=1,timesinceconf=1)
#myOde<-SetParamEstimationRegressor(myOde, EstiomationRegressor)
#myOde$EstimationRegressor$param


# @Melanie : ATTENTION Cela ne fonctionne que pour pour resolution globale
is_global<-1
ModeFilename<-"model_estimation.txt"


TimeDependantParameter<-c("transmission")

ode_id<-ComputeEstimationAllId(myOde,ModeFilename,TimeSpecificEquation,SpecificInitBloc,ModelMathBloc,is_global,TimeDependantParameter)
ode_id[[1]]$solution
nb_mc <- 100
# Global =1 for IC
is_global<-1
ode_id<-ComputeConfidenceIntervalAllId(ode_id,nb_mc,is_global)



## For plot over observation data

ModelObservationBloc<-c("Isim=r*E/De",
                        "Hsim=I/Dq")
ObservationResult <- vector(mode = "list", length = length(ModelObservationBloc))

CutObservation<-strsplit(ModelObservationBloc,'=')
data<-read.table(ode_id[[1]]$DataInfo$File,sep=ode_id[[1]]$DataInfo$Sep,header=TRUE)
InputNames<-colnames(data)
timename<-InputNames[ode_id[[1]]$DataInfo$HeaderType=="time"]
idname<-InputNames[ode_id[[1]]$DataInfo$HeaderType=="id"]
ObsIdName<-InputNames[ode_id[[1]]$DataInfo$HeaderType=="obsid"]
ObservationName<-InputNames[ode_id[[1]]$DataInfo$HeaderType=="observation"]
indivParams <-read.table(paste(here::here(),'/MonolixFile/',"/outputMonolix/",ode_id[[1]]$nameproject,"/IndividualParameters/estimatedIndividualParameters.txt",sep=""),header=TRUE,sep=",")

GetStatWithExp<-function(solution,parameter,exp,name){
  #State<-data.frame(matrix(ncol = 1, nrow = dim(solution)[1]))
  with(as.list(c(solution,parameter)),{
    State<-data.frame((eval(parse(text=exp))))
    names(State)<-name
    return(State)
  })
}
iobs<-1
name_variable<-list()
for (iobs in 1:length(ModelObservationBloc)){
  for (id in 1:length(ode_id)){
    name_variable[iobs]<-CutObservation[[iobs]][1]
    Obssim<-GetStatWithExp(ode_id[[id]]$solution,ode_id[[id]]$parameter,ModelObservationBloc[iobs],name_variable[iobs])
    Obssim<-cbind(Obssim,GetStatWithExp(ode_id[[id]]$ICmin,ode_id[[id]]$parameter,ModelObservationBloc[iobs],paste(name_variable[iobs],"_min",sep="")))
    Obssim<-cbind(Obssim,GetStatWithExp(ode_id[[id]]$ICmax,ode_id[[id]]$parameter,ModelObservationBloc[iobs],paste(name_variable[iobs],"_max",sep="")))
    Obssim<-cbind(Obssim,ode_id[[id]]$solution$time)
    colnames(Obssim)[dim(Obssim)[2]]<-timename
    Obssim<-cbind(Obssim,rep(indivParams$id[id],dim(Obssim)[[1]]))
    colnames(Obssim)[dim(Obssim)[2]]<-"id"
    Observation<-ode_id[[id]]$ObsData[which(ode_id[[id]]$ObsData[,ObsIdName]==iobs),]
    ObservationResult[[iobs]][[id]] <-merge(Observation, Obssim, by = timename)
    ode_id[[id]]$ObsSimu[[iobs]]<-ObservationResult[[iobs]][[id]]
  }
  ObservationDataFrame <- do.call(rbind.data.frame, ObservationResult[[iobs]])
  ObservationDataFrame$date<-as.Date(ObservationDataFrame$date)
  p1 <- ggplot(ObservationDataFrame, aes_(x=as.name("date"), y=as.name(ObservationName), group=as.name("id"))) +
    geom_point(aes(color = "Observed"))+
    scale_shape_manual(values=c(NA, 16)) +
    scale_color_manual(values="black") +
    geom_line(aes_(x=as.name("date"), y=as.name(name_variable[[iobs]]),linetype="Estimate"), color="red3") +
    geom_ribbon(aes_(ymin = as.name(paste(name_variable[[iobs]],"_min",sep="")), ymax=as.name(paste(name_variable[[iobs]],"_max",sep="")),alpha="95 %CI"), fill="red3")+
    scale_alpha_manual(values=c(0.3)) +
    facet_grid(vars(id), scales = "free_y") + facet_wrap(~ id, nrow=2)+
    guides(color=guide_legend(title=""), linetype=guide_legend(title=""),
           alpha=guide_legend(title=""))+
    theme(legend.position = "bottom") +
    ylab(map[[iobs]]) +
    xlab("Day") +
    theme(axis.text.x = element_text(angle=45, hjust=1),
          axis.title.y = element_text(hjust=1.4)) +
    theme(strip.background = element_rect(fill="white"),
          strip.text = element_text(size=8))
  p1
  if (dir.exists(paste0(here::here(),'/MonolixFile/outputMonolix/',ode_id[[1]]$nameproject,"/graphics/"))){
    
  }
  else{
    dir.create(paste0(here::here(),'/MonolixFile/outputMonolix/',ode_id[[1]]$nameproject,"/graphics/"))
  }
  ggsave(plot=p1, filename = paste0(here::here(),'/MonolixFile/outputMonolix/',ode_id[[1]]$nameproject,"/graphics/", map[[iobs]], ".jpg"), width=10, height=8)
}

# R0 plot




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