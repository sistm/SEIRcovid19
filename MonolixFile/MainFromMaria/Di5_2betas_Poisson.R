library(xtable)
library(ggplot2)
library(SEIRcovid19FR)
library(lixoftConnectors)
nameproject <- "dq59_D_Di5_Poisson2beta"
ModelFile <- paste('./MonolixFile/', 'Estim_', nameproject, ".txt", sep="")
ModeFilename <- paste("Simul_", nameproject, ".txt", sep="")

# Define all parameter for Ode system with initial value (0),  including regressor parameter
param <- c(b1=2.22,
           betaw1=-0.5,
           betaw2=-1,
           rE = 0.844,
           rI = 0.966,
           alpha=0.55,
           De=5.1,
           Di=5,
           Dq=5.9,
           Dh=17.8,
           popsize=-100,
           timeSinceConf=-100)

# Define all init state of the ODE system with value,  it should be initVar
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
SpecificInit <- list(initA=1, initS=1, initI=1, initH=1,initR=1)
myOde <- SetSpecificInit(myOde, SpecificInit)
# Variability => 0 = fixed,  1 mobile sans effet aléatoire,  2 mobile avec effet aléatoire (ajout d'un beta)
## Variability has to be defined for Parameter and InitState
# Distribution as to be define for Parameter and IniState
ParamVar <- list(b1=2,
                 betaw1=1,
                 betaw2=1)
myOde <- SetParamVariability(myOde, ParamVar)
# Param
InitVar <- list(initE=2)
myOde <- SetInitVariability(myOde, InitVar)
# Distribution of the parameter,  default is logNormal
#Param
Paramdist <- list(betaw1="normal", betaw2="normal")
myOde <- SetParamDistribution(myOde, Paramdist)

ParamRegressor <- list(popsize=1, timeSinceConf=1,Dh=1)
myOde <- SetParamIsRegressor(myOde, ParamRegressor)

# Init
# InitRegressor <- list(initH=1)
# myOde <- SetInitIsRegressor(myOde, InitRegressor)

# Data input info
path <- "./MonolixFile/MonolixFirstWave_DREG_20210218.txt"
sep <- "\t"
header <-c("ignore","id","ignore","observation","regressor","ignore","time","ignore","regressor","ignore","ignore","regressor","obsid")
myOde <- SetDataInput(myOde, path, header, sep)

SpecificInitBloc <- c("I_0=rE*E_0/De",
                      "A_0=I_0*(1-rE)/rE",
                      "H_0=(1-rI)*I_0/Dq",
                      "R_0 =1",
                      "S_0=popsize-E_0-I_0-R_0-A_0-H_0")

ModelStatBloc <- c("if (timeSinceConf==0)",
                   "current = 0",
                   "else",
                   "if (timeSinceConf<14)",
                   "current = betaw1",
                   "end",
                   "if (timeSinceConf>=14) ",
                   "current = betaw2",
                   "end",
                   "end",
                   "transmission=b1*exp(current)",
                   "dailyMove=0")

ModelMathBloc <- c("ddt_S = -transmission*S*(I+alpha*A)/popsize",
                   "ddt_E = transmission*S*(I+alpha*A)/popsize-E/De",
                   "ddt_I = rE*E/De-(1-rI)*I/Dq-rI*I/Di",
                   "ddt_R = (rI*I+A)/Di+H/Dh",
                   "ddt_A = (1-rE)*E/De-A/Di",
                   "ddt_H = (1-rI)*I/Dq-H/Dh")

ModelObservationBloc <- c("Hcurrent=H",
                          "Hin=(1-rI)*I/Dq")
myOde <- WriteMonolixModel(myOde, ModelFile, SpecificInitBloc, ModelStatBloc, ModelMathBloc, ModelObservationBloc)
## Launch monolix
obs <- list(hospitalisation_current="discrete", hospitalisation_influx="discrete")
map <- list("1" = "hospitalisation_current",  "2" = "hospitalisation_influx")
start_time <- Sys.time()

PopInitValue=c(b1=0.8,
               initE=1000,
               betaw1=-1,
               betaw2=-2)

myOde <- LaunchMonolix(myOde,  nameproject,  obs,  map, runToBeDone=FALSE,  PopInitValue=PopInitValue)

end_time <- Sys.time()
start_time - end_time

myOde$nameproject <- nameproject

# Uncomment the line below to re-run Monolix estimation
#CreateFilesFromMonolixEstimation(paste("./MonolixFile/", nameproject, sep=""))
#
#
TimeSpecificEquation <- c(ModelStatBloc, "R0=Di*transmission/(A+I) *(alpha*A + 1/rI * (Dq*I/(1-rI))/(Dq/(1-rI) + Di/rI))")
TimeDependantParameter <- c("transmission", "R0")
ode_id <- ComputeEstimationAllId(myOde, ModeFilename, TimeSpecificEquation,
                                 SpecificInitBloc, ModelMathBloc, 1,
                                 TimeDependantParameter)


nb_mc  <-  1000
is_global <- 1
TimeDependantParameter <- c("transmission", "R0")

ode_id <- ComputeConfidenceIntervalAllId(ode_id, nb_mc, is_global,
                                         TimeDependantParameter)
is_normalize <- 0
ode_id <- PlotSolutionOverObservation(ode_id, ModelObservationBloc,
                                      is_normalize)

#R0_formula <- "(Di/rI)*transmission/(A+I)*(alpha*A+Dq/(1-rI)*I/(Di/rI+Dq/(1-rI)))"
R0_formula <- "Di*transmission/(A+I) *(alpha*A + 1/rI * (Dq*I/(1-rI))/(Dq/(1-rI) + Di/rI))"
R0min_formula <- "(Di*transmission_min/(A_max+I_max)*(alpha*A_min+1/rI * (Dq*I_min/(1-rI))/(Dq/(1-rI) + Di/rI)))"
R0max_formula <- "(Di*transmission_max/(A_min+I_min)*(alpha*A_max+1/rI * (Dq*I_max/(1-rI))/(Dq/(1-rI) + Di/rI)))"
#R0min_formula <- "Di*transmission_min/(A+I)*(alpha*A+Dq_min*I/(Di+Dq_max))"
#R0max_formula <- "Di*transmission_max/(A+I)*(alpha*A+Dq_max*I/(Di+Dq_min))"
ode_id <- PlotR0(ode_id, R0_formula, R0min_formula, R0max_formula)

# RO_TABLE
R0today <- as.data.frame(matrix(NA, ncol=4, nrow=0))
for (region in 1:12){
  R0today[region, ] <- c(ode_id[[region]]$R0[1, "id"], round(ode_id[[region]]$R0[which(ode_id[[region]]$R0[, "date"]=="2020-06-04"), "R0"], 2), round(ode_id[[region]]$R0[which(ode_id[[region]]$R0[, "date"]=="2020-06-04"), "R0_min"], 2), round(ode_id[[region]]$R0[which(ode_id[[region]]$R0[, "date"]=="2020-06-04"), "R0_max"], 2))
}
R0today
popsize_name <- "popsize"
thisisR0 <- GetR0AtSpecificDate(ode_id, "2020-05-10", "popsize", "France")
xtable(thisisR0)
thisisR0 <- GetR0AtSpecificDate(ode_id, "2020-03-17", "popsize", "France")
xtable(thisisR0)

# # TABLE PARAM Optimize
# ParameterTable <- GetTableOptimizeParam(ode_id, popsize_name, "France", digits=c(2, 2, 2, 2, 2, 2, 0),
#                                       nbinits=1)
#
# # # ## LONG TERM ANALYSIS
time_date <- seq(as.Date("2020-03-02"),  as.Date("2020-06-25"),  "days")
time <- seq(0, length(time_date)-1, 1)
x <- list()
x[[1]]  <-  list(name='timeSinceConf',
                 time=time,
                 value=rep(0, length(time)))
x[[1]]$value[16:length(time)] <- seq(1, length(time)-16+1, 1)
reg_info <- x

ode_id <- EstimateLongTerm(ode_id, time_date, time, reg_info, TimeDependantParameter)
plot_info <- PlotSolutionLongTerm(ode_id)

## ATTACKRATES
AttackRateInfintyFormula <- "R/popsize*100"
SpecificDateAttackRateEquation <- "(R+I+A+E)/popsize*100"

thisisAttackRate_inft <- GetAttackRateAtInfinity(ode_id, AttackRateInfintyFormula,
                                                 popsize_name =popsize_name,
                                                 NationalName = "France")
thisisAttackRate <- GetAttackRateAtSpecificDate(ode_id, SpecificDateAttackRateEquation,
                                                as.Date("2020-03-17"), popsize_name = popsize_name,
                                                NationalName = "France")
thisisAttackRate <- GetAttackRateAtSpecificDate(ode_id, SpecificDateAttackRateEquation,
                                                as.Date("2020-05-10"), popsize_name = popsize_name,
                                                NationalName = "France")


