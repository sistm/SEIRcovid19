#Sys.setlocale("LC_NUMERIC", "C")
library(xtable)
library(ggplot2)

nameproject <- "fourstepEAKalman_small"
ModelFile <- paste('./MonolixFile/', 'Estim_', nameproject, ".txt", sep="")
ModeFilename <- paste("Simul_", nameproject, ".txt", sep="")

###RECUPERATION DES INITS DE KALMAN
# DATABIOM <- read.table(file="./MonolixFile/DATABIOM.txt", header=TRUE)
# initKalman <- data.frame(nameid=unique(DATABIOM$nameid))
# OLD initKalman$initH <- c(5,  6,  3,  9,  6,  30,  2,  6,  1,  4,  11,  4)
# OLD initKalman$initI <- c(29,  4,  9,  9,  11,  14,  6 , 9 , 5 , 7,  9,  12)
# initKalman$initH <- c(4,  4,  2,  9,  4,  24 , 2,  5 , 1,  3 , 9,  2)
# initKalman$initI <- c(16,  1,  4,  4,  5 , 6,  4 , 5,  3 , 3,  5 , 5)
# for (i in 1:length(DATABIOM$nameid)){
#   DATABIOM$initH[i] <- initKalman$initH[which(initKalman$nameid==DATABIOM$nameid[i])]
#   DATABIOM$initIref[i] <- initKalman$initI[which(initKalman$nameid==DATABIOM$nameid[i])]
# }
# write.table(DATABIOM, file="./MonolixFile/DATABIOMKALMAN.txt", sep="\t", row.names = F, quote=F)

# Define all paremeter for Ode system with initial value (0),  including regressor parameter
param <- c(b1=1,
         betaw1=1,
         betaw2=1,
         betaw3=1,
         betaw4=1,
         rsent=0.0337,
         alpha=0.55,
         De=5.1,
         Di=2.3,
         Dq=1,
         Dh=30,
         pct=1,
         popsize=-100,
         timeSinceConf=-100,
         initIref=-100)
# Define all init state of the ODE systme with value,  it should be initVar
init   <-  c(initS=0,
           initE=1,
           initI=0,
           initR=0,
           initA=0,
           initH=0)
# Model name
model_name <- c("S", "E", "I", "R", "A", "H")
# Not used for now
ode_def <- seirah_ode

myOde <- OdeSystem(func=ode_def, param=param, init=init, modname = model_name)
myOde
# Définition spécifique pour les états initiaux => A et S
SpecificInit <- list(initA=1, initS=1, initI=1)
myOde <- SetSpecificInit(myOde, SpecificInit)
# Variability => 0 = fixed,  1 mobile sans effet aléatoire,  2 mobile avec effet aléatoire (ajout d'un beta)
## Variability has to be defined for Parameter and InitState
#Param
ParamVar <- list(b1=1,
               betaw1=2,
               betaw2=2,
               betaw3=2,
               betaw4=2,
               Dq=1)
myOde <- SetParamVariability(myOde, ParamVar)
# Param
InitVar <- list(initE=2)
myOde <- SetInitVariability(myOde, InitVar)
# Distribution of the parameter,  default is logNormal
# Distribution as to be define for Parameter and IniState
#Param
Paramdist <- list(betaw1="normal", betaw2="normal", betaw3="normal", betaw4="normal")
myOde <- SetParamDistribution(myOde, Paramdist)

ParamRegressor <- list(popsize=1, timeSinceConf=1, initIref=1)
myOde <- SetParamIsRegressor(myOde, ParamRegressor)
# Init
InitRegressor <- list(initH=1)
myOde <- SetInitIsRegressor(myOde, InitRegressor)
# Data input info
path <- "./MonolixFile/DATABIOMKALMAN.txt"
sep <- "\t"
header <- c("ignore", "id", "ignore", "observation", "regressor", "ignore", "time", "ignore", "regressor", "ignore", "ignore", "regressor", "regressor", "ignore", "obsid")

myOde <- SetDataInput(myOde, path, header, sep)

SpecificInitBloc <- c("I_0=initIref/pct",
                    "A_0=I_0*(1-rsent)/rsent",
                    "S_0=popsize-E_0-I_0-R_0-A_0-H_0")
ModelStatBloc <- c("if (timeSinceConf==0)",
                 "current = 0",
                 "else",
                 "if (timeSinceConf<7)",
                 "current = betaw1",
                 " end",
                 "if (timeSinceConf>=7) & (timeSinceConf<14)",
                 "current = betaw2",
                 "end",
                 "if (timeSinceConf>=14) & (timeSinceConf<21)",
                 "current = betaw3",
                 "end",
                 "if (timeSinceConf>=21) ",
                 "current = betaw4",
                 "end",
                 "end",
                 "transmission=b1*exp(current)",
                 "dailyMove=0")
ModelMathBloc <- c("ddt_S = -transmission*S*(I+alpha*A)/popsize+dailyMove-dailyMove*S/(popsize-I-H)",
                 "ddt_E = transmission*S*(I+alpha*A)/popsize-E/De-dailyMove*E/(popsize-I-H)",
                 "ddt_I = rsent*E/De-I/Dq-I/Di",
                 "ddt_R = (I+A)/Di+H/Dh-dailyMove*R/(popsize-I-H)",
                 "ddt_A = (1-rsent)*E/De-A/Di-dailyMove*A/(popsize-I-H)",
                 "ddt_H = I/Dq-H/Dh")
ModelObservationBloc <- c("Isim=pct*rsent*E/De",
                        "Hsim=I/Dq")
myOde <- WriteMonolixModel(myOde, ModelFile, SpecificInitBloc, ModelStatBloc, ModelMathBloc, ModelObservationBloc)
## Launch monolix

## Launch monolix
obs <- list(cas_confirmes_incident="discrete", hospitalisation_incident="discrete")
map <- list("1" = "cas_confirmes_incident",  "2" = "hospitalisation_incident")
start_time <- Sys.time()

PopInitValue=c(b1=1.95,
               initE=1000,
               betaw1=-0.1,
               betaw2=-0.70,
               betaw3=-1.2, betaw4=-1.2, Dq=0.8)

myOde <- LaunchMonolix(myOde,  nameproject,  obs,  map, runToBeDone=FALSE,  PopInitValue=PopInitValue )
end_time <- Sys.time()
#start_time - end_time

myOde$nameproject <- nameproject

# Uncomment the line below to re-run Monolix estimation
#CreateFilesFromMonolixEstimation(paste("./MonolixFile/", nameproject, sep=""))


TimeSpecificEquation <- c(ModelStatBloc, "R0=Di*transmission/(A+I)*(alpha*A+Dq*I/(Di+Dq))")
TimeDependantParameter <- c("transmission", "R0")

ode_id <- ComputeEstimationAllId(myOde, ModeFilename, TimeSpecificEquation,
                                 SpecificInitBloc, ModelMathBloc, 1,
                                 TimeDependantParameter)


nb_mc  <-  1000
is_global <- 1
TimeDependantParameter <- c("transmission", "R0", "Dq")
ode_id <- ComputeConfidenceIntervalAllId(ode_id, nb_mc, is_global,
                                         TimeDependantParameter)
is_normalize <- 0
ode_id <- PlotSolutionOverObservation(ode_id, ModelObservationBloc,
                                      is_normalize)

R0_formula <- "Di*transmission/(A+I)*(alpha*A+Dq*I/(Di+Dq))"
R0min_formula <- "Di*transmission_min/(A_max+I_max)*(alpha*A_min+Dq_min*I_min/(Di+Dq_max))"
R0max_formula <- "Di*transmission_max/(A_min+I_min)*(alpha*A_max+Dq_max*I_max/(Di+Dq_min))"
#R0min_formula <- "Di*transmission_min/(A+I)*(alpha*A+Dq_min*I/(Di+Dq_max))"
#R0max_formula <- "Di*transmission_max/(A+I)*(alpha*A+Dq_max*I/(Di+Dq_min))"
ode_id <- PlotR0(ode_id, R0_formula, R0min_formula, R0max_formula)


## RO_TABLE
# R0today <- as.data.frame(matrix(NA, ncol=4, nrow=0))
# for (region in 1:12){
#   R0today[region, ] <- c(ode_id[[region]]$R0[1, "id"], round(ode_id[[region]]$R0[which(ode_id[[region]]$R0[, "date"]=="2020-06-04"), "R0"], 2), round(ode_id[[region]]$R0[which(ode_id[[region]]$R0[, "date"]=="2020-06-04"), "R0_min"], 2), round(ode_id[[region]]$R0[which(ode_id[[region]]$R0[, "date"]=="2020-06-04"), "R0_max"], 2))
# }
# R0today
popsize_name <- "popsize"
thisisR0 <- GetR0AtSpecificDate(ode_id, "2020-05-10", "popsize", "France")
xtable(thisisR0)

## TABLE PARAM Optimize
ParameterTable <- GetTableOptimizeParam(ode_id, popsize_name, "France", digits=c(2, 2, 2, 2, 2, 2, 0),
                                      nbinits=1)

## LONG TERM ANALYSIS
time_date <- seq(as.Date("2020-03-02"),  as.Date("2020-06-25"),  "days")
time <- seq(0, length(time_date)-1, 1)
x <- list()
x[[1]]  <-  list(name='timeSinceConf',
               time=time,
               value=rep(0, length(time)))
x[[1]]$value[16:length(time)] <- seq(1, length(time)-16+1, 1)
reg_info <- x


# Not needed ?
# ode_id <- EstimateLongTerm(ode_id, time_date, time, reg_info, TimeDependantParameter)
# plot_info <- PlotSolutionLongTerm(ode_id)
#
# ## ATTACKRATES
# AttackRateInfintyFormula <- "R/popsize*100"
# SpecificDateAttackRateEquation <- "(R+I+A+E)/popsize*100"
#
# thisisAttackRate_inft <- GetAttackRateAtInfinity(ode_id, AttackRateInfintyFormula,
#                                   popsize_name =popsize_name,
#                                   NationalName = "France")
# thisisAttackRate <- GetAttackRateAtSpecificDate(ode_id, SpecificDateAttackRateEquation,
#                                       as.Date("2020-05-11"), popsize_name = popsize_name,
#                                       NationalName = "France")


