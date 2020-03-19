install.packages("./monolix/lixoftConnectors.tar.gz", repos = NULL, type="source", INSTALL_opts ="--no-multiarch")
library(lixoftConnectors)
initializeLixoftConnectors(software="monolix")



newProject(data = list(dataFile = "./monolix/data_region_20200319.txt",
                       headerTypes =c("ignore","id","ignore","time","observation","ignore","regressor","regressor"),
                       observationTypes = list(y1 = "continuous"),
                       mapping = list("1" = "y1")),
           modelFile = "./monolix/seirah_normal.txt")

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

