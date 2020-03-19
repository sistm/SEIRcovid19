#' OLS Estimation of parameters in SIERAH models
#'
#' @import RJSONIO
#' @import MlxConnectors
#' @import Rsmlx
#'
#' @examples
#' install.packages("./R/monolix/lixoftConnectors.tar.gz", repos = NULL, type="source", INSTALL_opts ="--no-multiarch")
?newProject
library(lixoftConnectors)
initializeLixoftConnectors(software="monolix")
newProject(data = list(dataFile = "./R/monolix/TestCoronavirus.txt",
                       headerTypes =c("ignore","ignore","id","time","observation","ignore","regressor","regressor"),
                       observationTypes = list(y1 = "continuous"),
                       mapping = list("1" = "y1")),
           modelFile = "./R/monolix/seirah.txt")
scenario <- getScenario()
scenario$tasks = c(populationParameterEstimation = T, 
                   conditionalModeEstimation = T, 
                   conditionalDistributionSampling = T, 
                   standardErrorEstimation=T, 
                   logLikelihoodEstimation=T)
scenario$linearization = FALSE
setScenario(scenario)
popparams <- getPopulationParameterInformation()
tabestimates <- NULL; tabse <- NULL
for(i in 1:5){
  # sample new initial estimates
  popini <- sapply(1:nrow(popparams), function(j){runif(n=1, min=popparams$initialValue[j]/2, max=popparams$initialValue[j]*2)})
  
  # set sampled values as new initial estimates
  newpopparams <- popparams
  newpopparams$initialValue <- popini
  setPopulationParameterInformation(newpopparams)
  
  # run the estimation
  runScenario()
  
  # store the estimates and s.e. in table
  tabestimates <- cbind(tabestimates, getEstimatedPopulationParameters())
  tabse <- cbind(tabse, getEstimatedStandardErrors()$stochasticApproximation)
}


#'
#'
#' @export
#'
#'
monolix_sierah <- function(b,data,alpha=1,De=5.2,Di=2.3,Dq=10,Dh=30,popSize=65000000,dailyMove=1000000,log.print=TRUE,plot.comparison=FALSE){
  initializeLixoftConnectors(software = "monolix")

  transmission<-exp(b[1])
  ascertainment<-exp(b[2])
  if(log.print)print(paste("transmission",transmission))
  if(log.print)print(paste("ascertainment",ascertainment))
  E0<- data[1,"cas_confirmes_incident"]*2#Twice the number of cases (346 ref)
  I0<-data[1,"cas_confirmes_incident"] #Numbers of cases (80 ref)
  R0<- 0#(0 ref)
  A0<-I0 # A=I (80 ref)
  H0<-I0*0.50 #all at the begining H=50%I (27 ref)
  S0<- popSize-E0-I0-A0-H0-R0 #N-E-I-A-H-R (9999467 ref)
  init<-c(S0,E0,I0,R0,A0,H0)
  t<-seq(0,365)
  par<-c(transmission,ascertainment,alpha,De,Di,Dq,Dh,popSize,dailyMove)
  sol <- deSolve::lsoda(init,t,seirah_ode,par)
  #if(max(sol[,"time"]!=365)){
  #  objectiveFunction<-100
  #}else{
  #plot_sierah(sol,locator.legend=F)
  selectedsol<-sol[which(sol[,"time"]%in%data$day),4]
  ols_sierah<-sum((data[,"cas_confirmes_incident"]-selectedsol)**2)/length(data[,"cas_confirmes_incident"])
  if(log.print)print(paste("Objective Function",ols_sierah))
  if(plot.comparison){
    ylimmax<-max(rbind(selectedsol,data[,"cas_confirmes_incident"]))+1
    plot(sol[which(sol[,"time"]%in%data$day),1],selectedsol,type="l",col="blue",ylim=c(0,ylimmax),ylab="Proportion")
    points(data[,"day"],data[,"cas_confirmes_incident"],col="black")
  }
  return(ols_sierah)
}

#######################################
############# ODE #####################
#######################################
######################
### Initialisation
#install.packages('/Applications/MonolixSuite2018R2.app/Contents/Resources/mlxsuite/mlxConnectors/R/MlxConnectors.tar.gz', repos = NULL, type="source")
library(MlxConnectors)
initializeMlxConnectors(software = "monolix")
library(Rsmlx) # this package can be used to do model selection

##########################
### Selection de variables TLR7VAC
# methode 1
for.cov.model.1 <-"/Users/melanieprague/BOULOT/RESEARCH/TLR7/CODE/MONOLIX/FINAL_PNAS/VAC/washout6_base.mlxtran"

samba_result <- buildmlx(for.cov.model.1,criterion = "BIC",paramToUse = c("lambda","NPs","ta","m"), covToTest = c("vac","tlr7","study","intTLR7VAC","intTLR7STUD"))

# Iteration 3:
#   No difference between two successive iterations
# ____________________________________________
# Final model:
#
#   Covariate model:
#   intTLR7STUD intTLR7VAC study tlr7 vac
# lambda           1          0     0    0   0
# ta               1          0     0    0   1
# NPs              0          1     1    0   1
# m                0          1     0    0   0
#
# Correlation model:
#   [[1]]
# [1] "ta"  "NPs"
#
#
# Residual error model:
#   vl
# "combined1"
#
# Estimated criteria (importanceSampling):
#   -2LL   s.e.    AIC    BIC
# 816.01   0.49 858.01 900.17


for.cov.model.1 <-"/Users/melanieprague/BOULOT/RESEARCH/TLR7/CODE/MONOLIX/FINAL_PNAS/AB/washout11_base.mlxtran"

samba_result <- buildmlx(for.cov.model.1,criterion = "BIC",paramToUse = c("betas","NPs","ta","m"), covToTest = c("ab","tlr7","inttlr7ab"))
