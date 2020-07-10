########### ########### ###########
########### SOME FUNCTIONS
########### ########### ###########
library(xtable)
full_region_names  <-  function(x){
  forcats::fct_recode(x,
                      "Île-de-France"="IDF",
                      "Nouvelle-Aquitaine" = "NAquitaine",
                      "Nouvelle-Aquitaine" = "N-Aquitaine",
                      "Auvergne-Rhône-Alpes" = "AURA",
                      "Centre-Val de Loire" = "Centre",
                      "Bourgogne-Franche-Comté" = "BFC",
                      "Normandie" = "Normandie",
                      "Hauts-de-France" = "HDF",
                      "Grand Est" = "GrandEst",
                      "Pays de la Loire" = "PaysLoire",
                      "Bretagne" = "Bretagne",
                      "Occitanie" = "Occitanie",
                      "Provence-Alpes-Côte d'Azur" = "PACA"
  )
}

source("MonolixFile/fourstepEAKalman_small.R")


########### ########### ###########
# TABLES ----
########### ########### ###########
## TABLE 1 ------
data <- read.table("./MonolixFile/DATABIOM.txt", header=TRUE)
getindicators <- function(data){
  result <- as.data.frame(matrix(ncol=0, nrow=length(unique(data$nameid))))
  result$id <- data[which((data$day==0)&(data$obs_id==1)), "nameid"]
  result$epidemicstart <- data[which((data$day==0)&(data$obs_id==1)), "epidemicsStart"]
  k <- 1
  result$cumulI <- NA
  result$cumulH <- NA
  for (i in result$id){
    result$cumulI[k] <- format(sum(data[which((data$obs_id==1)&(data$nameid==i)), "obs"]), nsmall = 0,  big.mark=", " )
    result$cumulH[k] <- format(sum(data[which((data$obs_id==2)&(data$nameid==i)), "obs"]), nsmall = 0,  big.mark =", ")
    k <- k+1
  }
  result$popsize <- format(as.numeric(data[which((data$day==0)&(data$obs_id==1)), "popsize"]), nsmall = 0 ,  big.mark =", ")
  result$rsent <- format(round(data[which((data$day==0)&(data$obs_id==1)), "rsent"], 2), nsmall = 2)
  result$id <- as.character(full_region_names(result$id))
  result <- result[order(result$id), ]
  q25=quantile(data[which((data$day==0)&(data$obs_id==1)), "rsent"], 0.025)
  mean <- sum(data[which((data$day==0)&(data$obs_id==1)), "popsize"]*data[which((data$day==0)&(data$obs_id==1)), "rsent"])/sum(data[which((data$day==0)&(data$obs_id==1)), "popsize"])
  q975=quantile(data[which((data$day==0)&(data$obs_id==1)), "rsent"], 0.975)

  result[length(unique(data$nameid))+1, ] <- c("France", "NA",   format(sum(data[which((data$obs_id==1)), "obs"]), nsmall = 0,  big.mark=", " )
                                               ,   format(sum(data[which((data$obs_id==2)), "obs"]), nsmall = 0,  big.mark=", " ),  format(sum(as.numeric(data[which((data$day==0)&(data$obs_id==1)), "popsize"])), nsmall = 0 ,  big.mark =", "),
                                               paste(format(round(mean, 3), nsmall=2), " [", format(round(q25, 2), nsmall=2), "; ", format(round(q975, 2), nsmall=2), "]", sep=""))

  print(xtable(result[, c("id", "epidemicstart", "cumulI", "cumulH", "popsize", "rsent")]),
        include.rownames=FALSE)
}
getindicators(data)



#### TABLE 2 ----
# From litterature

### TABLE 3 ----
ParameterTable <- GetTableOptimizeParam(ode_id, popsize_name, "France", digits=c(2, 2, 2, 2, 2, 2, 0),
                                        nbinits=1)
ParameterTable$id <- rownames(ParameterTable)
table3 <- ParameterTable[, c("id", "initE", "betaw1", "betaw2", "betaw3", "betaw4")]
table3$id <- as.character(full_region_names(table3$id))
table3 <- table3[order(table3$id), ]
print(xtable(table3),
      include.rownames=FALSE)

###  FREE INFORMATION IN THE TEXT
## ATTACKRATES FINAL NO INTERVENTION
time_date <- seq(as.Date("2020-03-02"),  as.Date("2021-06-25"),  "days")
time <- seq(0, length(time_date)-1, 1)
x <- list()
x[[1]]  <-  list(name='timeSinceConf',
                 time=time,
                 value=rep(0, length(time)))
reg_info <- x
ode_id <- EstimateLongTerm(ode_id, time_date, time, reg_info, TimeDependantParameter)
plot_info <- PlotSolutionLongTerm(ode_id)
AttackRateInfintyFormula <- "R/popsize*100"
attackrates <- GetAttackRateAtInfinity(ode_id, AttackRateInfintyFormula, popsize_name =popsize_name, NationalName = "France")

## R0 -- Table 4 ----
R0before <- GetR0AtSpecificDate(ode_id, "2020-03-15", "popsize", "France")
R0w1 <- GetR0AtSpecificDate(ode_id, "2020-03-22", "popsize", "France")
R0w2 <- GetR0AtSpecificDate(ode_id, "2020-03-29", "popsize", "France")
R0w3 <- GetR0AtSpecificDate(ode_id, "2020-04-05", "popsize", "France")
R0w4 <- GetR0AtSpecificDate(ode_id, "2020-05-11", "popsize", "France")
R0 <- merge(R0before, R0w1, by="Reg")
names(R0) <- c("Reg", "before", "w1")
R0 <- merge(R0, R0w2, by="Reg")
names(R0) <- c("Reg", "before", "w1", "w2")
R0 <- merge(R0, R0w3, by="Reg")
names(R0) <- c("Reg", "before", "w1", "w2", "w3")
R0 <- merge(R0, R0w4, by="Reg")
names(R0) <- c("id", "before", "w1", "w2", "w3", "w4")
R0$id <- as.character(full_region_names(R0$id))
R0 <- R0[order(R0$id), ]
print(xtable(R0),
      include.rownames=FALSE)


### TABLE 6 Attack rate with intervention ----
time_date <- seq(as.Date("2020-03-02"),  as.Date("2021-05-15"),  "days")
time <- seq(0, length(time_date)-1, 1)
x <- list()
x[[1]]  <-  list(name='timeSinceConf',
                 time=time,
                 value=rep(0, length(time)))
x[[1]]$value[16:length(time)] <- seq(1, length(time)-16+1, 1)
reg_info <- x
ode_id <- EstimateLongTerm(ode_id, time_date, time, reg_info, TimeDependantParameter)
beforeattackrate <- GetAttackRateAtSpecificDate(ode_id, SpecificDateAttackRateEquation, as.Date("2020-03-17"), popsize_name =popsize_name, NationalName = "France")
afterattackrate <- GetAttackRateAtSpecificDate(ode_id, SpecificDateAttackRateEquation, as.Date("2020-05-11"), popsize_name =popsize_name, NationalName = "France")

result <- cbind(beforeattackrate[, c("Reg", "AR")], afterattackrate[, "AR"])
result$Reg <- full_region_names(result$Reg)
result <- result[order(result$Reg), ]
print(xtable(result),
      include.rownames=FALSE)

#############################
#### FIGURES -----
#############################
library(ggplot2)
## FIGURE 1 ----
# Mechanistic model

## FIGURE 2 - Fits ----
plot_article  <-  PlotArticleFit(ode_id, ModelObservationBloc, is_normalize=0)

old.loc <- Sys.getlocale("LC_TIME")
Sys.setlocale("LC_TIME", "en_GB.UTF-8")
plot_article
Sys.setlocale("LC_TIME", old.loc)

### FIGURE3 ----
datagouv <- read.table("./data/geodesSPF.csv", header=TRUE, sep=";")
datagouv$time <- as.Date(datagouv$time)
datagouv$Iobs <- as.numeric(datagouv$Iobs)
datagouv$Dobs <- as.numeric(datagouv$Dobs)
datagouv$Hobs <- as.numeric(datagouv$Hobs)
datagouv$ICUobs <- as.numeric(datagouv$ICUobs)
datagouv$Hallobs <- datagouv$ICUobs+datagouv$Hobs
time_date <- seq(as.Date("2020-03-02"),  as.Date("2021-07-15"),  "days")
time <- seq(0, length(time_date)-1, 1)
x <- list()
x[[1]]  <-  list(name='timeSinceConf',
                 time=time,
                 value=rep(0, length(time)))
x[[1]]$value[16:length(time)] <- seq(1, length(time)-16+1, 1)
reg_info <- x
ode_id <- EstimateLongTerm(ode_id, time_date, time, reg_info, TimeDependantParameter)


time_date <- seq(as.Date("2020-03-02"),  as.Date("2021-07-15"),  "days")
time <- seq(0, length(time_date)-1, 1)
x <- list()
x[[1]]  <-  list(name='timeSinceConf',
                 time=time,
                 value=rep(0, length(time)))
reg_info <- x
ode_idNOEFFECT <- EstimateLongTerm(ode_id, time_date, time, reg_info, TimeDependantParameter)


time_date <- seq(as.Date("2020-03-02"),  as.Date("2021-07-15"),  "days")
time <- seq(0, length(time_date)-1, 1)
x <- list()
x[[1]]  <-  list(name='timeSinceConf',
                 time=time,
                 value=rep(0, length(time)))
x[[1]]$value[16:(16+55)] <- seq(1, 55+1, 1)
reg_info <- x
ode_idSTOPLOCK <- EstimateLongTerm(ode_id, time_date, time, reg_info, TimeDependantParameter)



datapred <- data.frame(time=seq(as.Date("2020-03-10"),  as.Date("2021-07-15"),  "day"))
cumulI <- 0
cumulH <- 0
cumulImin <- 0
cumulImax <- 0
cumulHmin <- 0
cumulHmax <- 0
cumulICU <- 0
cumulICUmin <- 0
cumulICUmax <- 0
cumulD <- 0
cumulDmin <- 0
cumulDmax <- 0

cumulINOEFFECT <- 0
cumulHNOEFFECT <- 0
cumulENOEFFECT <- 0
cumulANOEFFECT <- 0
incINOEFFECT <- 0
cumulIminNOEFFECT <- 0
cumulImaxNOEFFECT <- 0
cumulHminNOEFFECT <- 0
cumulHmaxNOEFFECT <- 0
cumulICUNOEFFECT <- 0
cumulICUminNOEFFECT <- 0
cumulICUmaxNOEFFECT <- 0
cumulDNOEFFECT <- 0
cumulDminNOEFFECT <- 0
cumulDmaxNOEFFECT <- 0


cumulISTOPLOCK <- 0
cumulHSTOPLOCK <- 0
cumulIminSTOPLOCK <- 0
cumulImaxSTOPLOCK <- 0
cumulHminSTOPLOCK <- 0
cumulHmaxSTOPLOCK <- 0
cumulICUSTOPLOCK <- 0
cumulICUminSTOPLOCK <- 0
cumulICUmaxSTOPLOCK <- 0
cumulDSTOPLOCK <- 0
cumulDminSTOPLOCK <- 0
cumulDmaxSTOPLOCK <- 0

incISTOPLOCK <- 0
cumulASTOPLOCK <- 0
cumulESTOPLOCK <- 0

cumulENOEFFECT <- 0

incImaxSTOPLOCK <- 0
cumulAmaxSTOPLOCK <- 0
cumulEmaxSTOPLOCK  <- 0
incIminSTOPLOCK <- 0
cumulAminSTOPLOCK <- 0
cumulEminSTOPLOCK <- 0

for(reg in 1:12){
  cumulI <- cumulI+ode_id[[reg]]$LongTerm$I[which(ode_id[[reg]]$LongTerm$time>=8)]+ode_id[[reg]]$LongTerm$H[which(ode_id[[reg]]$LongTerm$time>=8)]+0.0337*ode_id[[reg]]$LongTerm$R[which(ode_id[[reg]]$LongTerm$time>=8)]
  cumulImin <- cumulImin+ode_id[[reg]]$LongTermMin$I[which(ode_id[[reg]]$LongTerm$time>=8)]+ode_id[[reg]]$LongTermMin$H[which(ode_id[[reg]]$LongTerm$time>=8)]+0.0337*ode_id[[reg]]$LongTermMin$R[which(ode_id[[reg]]$LongTerm$time>=8)]
  cumulImax <- cumulImax+ode_id[[reg]]$LongTermMax$I[which(ode_id[[reg]]$LongTerm$time>=8)]++ode_id[[reg]]$LongTermMax$H[which(ode_id[[reg]]$LongTerm$time>=8)]+0.0337*+ode_id[[reg]]$LongTermMax$R[which(ode_id[[reg]]$LongTerm$time>=8)]
  cumulH <- cumulH+ode_id[[reg]]$LongTerm$H[which(ode_id[[reg]]$LongTerm$time>=8)]
  cumulHmin <- cumulHmin+ode_id[[reg]]$LongTermMin$H[which(ode_id[[reg]]$LongTerm$time>=8)]
  cumulHmax <- cumulHmax+ode_id[[reg]]$LongTermMax$H[which(ode_id[[reg]]$LongTerm$time>=8)]
  cumulICU <- cumulICU+0.25*ode_id[[reg]]$LongTerm$H[which(ode_id[[reg]]$LongTerm$time>=8)]
  cumulICUmin <- cumulICUmin+0.25*ode_id[[reg]]$LongTermMin$H[which(ode_id[[reg]]$LongTerm$time>=8)]
  cumulICUmax <- cumulICUmax+0.25*ode_id[[reg]]$LongTermMax$H[which(ode_id[[reg]]$LongTerm$time>=8)]
  cumulD <- cumulD+0.005*ode_id[[reg]]$LongTerm$R[which(ode_id[[reg]]$LongTerm$time>=8)]
  cumulDmin <- cumulDmin+0.005*ode_id[[reg]]$LongTermMin$R[which(ode_id[[reg]]$LongTerm$time>=8)]
  cumulDmax <- cumulDmax+0.005*ode_id[[reg]]$LongTermMax$R[which(ode_id[[reg]]$LongTerm$time>=8)]
  cumulENOEFFECT <- cumulENOEFFECT+ode_idNOEFFECT[[reg]]$LongTerm$E[which(ode_idNOEFFECT[[reg]]$LongTerm$time>=8)]
  cumulANOEFFECT <- cumulANOEFFECT+ode_idNOEFFECT[[reg]]$LongTerm$A[which(ode_idNOEFFECT[[reg]]$LongTerm$time>=8)]
  incINOEFFECT <- incINOEFFECT+ode_idNOEFFECT[[reg]]$LongTerm$I[which(ode_idNOEFFECT[[reg]]$LongTerm$time>=8)]


  cumulINOEFFECT <- cumulINOEFFECT+ode_idNOEFFECT[[reg]]$LongTerm$I[which(ode_idNOEFFECT[[reg]]$LongTerm$time>=8)]+ode_idNOEFFECT[[reg]]$LongTerm$H[which(ode_idNOEFFECT[[reg]]$LongTerm$time>=8)]+0.0337*ode_idNOEFFECT[[reg]]$LongTerm$R[which(ode_idNOEFFECT[[reg]]$LongTerm$time>=8)]
  cumulIminNOEFFECT <- cumulIminNOEFFECT+ode_idNOEFFECT[[reg]]$LongTermMin$I[which(ode_idNOEFFECT[[reg]]$LongTerm$time>=8)]+ode_idNOEFFECT[[reg]]$LongTermMin$H[which(ode_idNOEFFECT[[reg]]$LongTerm$time>=8)]+0.0337*ode_idNOEFFECT[[reg]]$LongTermMin$R[which(ode_idNOEFFECT[[reg]]$LongTerm$time>=8)]
  cumulImaxNOEFFECT <- cumulImaxNOEFFECT+ode_idNOEFFECT[[reg]]$LongTermMax$I[which(ode_idNOEFFECT[[reg]]$LongTerm$time>=8)]++ode_idNOEFFECT[[reg]]$LongTermMax$H[which(ode_idNOEFFECT[[reg]]$LongTerm$time>=8)]+0.0337*+ode_idNOEFFECT[[reg]]$LongTermMax$R[which(ode_idNOEFFECT[[reg]]$LongTerm$time>=8)]
  cumulHNOEFFECT <- cumulHNOEFFECT+ode_idNOEFFECT[[reg]]$LongTerm$H[which(ode_idNOEFFECT[[reg]]$LongTerm$time>=8)]
  cumulHminNOEFFECT <- cumulHminNOEFFECT+ode_idNOEFFECT[[reg]]$LongTermMin$H[which(ode_idNOEFFECT[[reg]]$LongTerm$time>=8)]
  cumulHmaxNOEFFECT <- cumulHmaxNOEFFECT+ode_idNOEFFECT[[reg]]$LongTermMax$H[which(ode_idNOEFFECT[[reg]]$LongTerm$time>=8)]
  cumulICUNOEFFECT <- cumulICUNOEFFECT+0.25*ode_idNOEFFECT[[reg]]$LongTerm$H[which(ode_idNOEFFECT[[reg]]$LongTerm$time>=8)]
  cumulICUminNOEFFECT <- cumulICUminNOEFFECT+0.25*ode_idNOEFFECT[[reg]]$LongTermMin$H[which(ode_idNOEFFECT[[reg]]$LongTerm$time>=8)]
  cumulICUmaxNOEFFECT <- cumulICUmaxNOEFFECT+0.25*ode_idNOEFFECT[[reg]]$LongTermMax$H[which(ode_idNOEFFECT[[reg]]$LongTerm$time>=8)]
  cumulDNOEFFECT <- cumulDNOEFFECT+0.005*ode_idNOEFFECT[[reg]]$LongTerm$R[which(ode_idNOEFFECT[[reg]]$LongTerm$time>=8)]
  cumulDminNOEFFECT <- cumulDminNOEFFECT+0.005*ode_idNOEFFECT[[reg]]$LongTermMin$R[which(ode_idNOEFFECT[[reg]]$LongTerm$time>=8)]
  cumulDmaxNOEFFECT <- cumulDmaxNOEFFECT+0.005*ode_idNOEFFECT[[reg]]$LongTermMax$R[which(ode_idNOEFFECT[[reg]]$LongTerm$time>=8)]

  cumulISTOPLOCK <- cumulISTOPLOCK+ode_idSTOPLOCK[[reg]]$LongTerm$I[which(ode_idSTOPLOCK[[reg]]$LongTerm$time>=8)]+ode_idSTOPLOCK[[reg]]$LongTerm$H[which(ode_idSTOPLOCK[[reg]]$LongTerm$time>=8)]+0.0337*ode_idSTOPLOCK[[reg]]$LongTerm$R[which(ode_idSTOPLOCK[[reg]]$LongTerm$time>=8)]
  cumulIminSTOPLOCK <- cumulIminSTOPLOCK+ode_idSTOPLOCK[[reg]]$LongTermMin$I[which(ode_idSTOPLOCK[[reg]]$LongTerm$time>=8)]+ode_idSTOPLOCK[[reg]]$LongTermMin$H[which(ode_idSTOPLOCK[[reg]]$LongTerm$time>=8)]+0.0337*ode_idSTOPLOCK[[reg]]$LongTermMin$R[which(ode_idSTOPLOCK[[reg]]$LongTerm$time>=8)]
  cumulImaxSTOPLOCK <- cumulImaxSTOPLOCK+ode_idSTOPLOCK[[reg]]$LongTermMax$I[which(ode_idSTOPLOCK[[reg]]$LongTerm$time>=8)]++ode_idSTOPLOCK[[reg]]$LongTermMax$H[which(ode_idSTOPLOCK[[reg]]$LongTerm$time>=8)]+0.0337*+ode_idSTOPLOCK[[reg]]$LongTermMax$R[which(ode_idSTOPLOCK[[reg]]$LongTerm$time>=8)]
  cumulHSTOPLOCK <- cumulHSTOPLOCK+ode_idSTOPLOCK[[reg]]$LongTerm$H[which(ode_idSTOPLOCK[[reg]]$LongTerm$time>=8)]
  cumulHminSTOPLOCK <- cumulHminSTOPLOCK+ode_idSTOPLOCK[[reg]]$LongTermMin$H[which(ode_idSTOPLOCK[[reg]]$LongTerm$time>=8)]
  cumulHmaxSTOPLOCK <- cumulHmaxSTOPLOCK+ode_idSTOPLOCK[[reg]]$LongTermMax$H[which(ode_idSTOPLOCK[[reg]]$LongTerm$time>=8)]
  cumulICUSTOPLOCK <- cumulICUSTOPLOCK+0.25*ode_idSTOPLOCK[[reg]]$LongTerm$H[which(ode_idSTOPLOCK[[reg]]$LongTerm$time>=8)]
  cumulICUminSTOPLOCK <- cumulICUminSTOPLOCK+0.25*ode_idSTOPLOCK[[reg]]$LongTermMin$H[which(ode_idSTOPLOCK[[reg]]$LongTerm$time>=8)]
  cumulICUmaxSTOPLOCK <- cumulICUmaxSTOPLOCK+0.25*ode_idSTOPLOCK[[reg]]$LongTermMax$H[which(ode_idSTOPLOCK[[reg]]$LongTerm$time>=8)]
  cumulDSTOPLOCK <- cumulDSTOPLOCK+0.005*ode_idSTOPLOCK[[reg]]$LongTerm$R[which(ode_idSTOPLOCK[[reg]]$LongTerm$time>=8)]
  cumulDminSTOPLOCK <- cumulDminSTOPLOCK+0.005*ode_idSTOPLOCK[[reg]]$LongTermMin$R[which(ode_idSTOPLOCK[[reg]]$LongTerm$time>=8)]
  cumulDmaxSTOPLOCK <- cumulDmaxSTOPLOCK+0.005*ode_idSTOPLOCK[[reg]]$LongTermMax$R[which(ode_idSTOPLOCK[[reg]]$LongTerm$time>=8)]
  cumulESTOPLOCK <- cumulESTOPLOCK+ode_idSTOPLOCK[[reg]]$LongTerm$E[which(ode_idSTOPLOCK[[reg]]$LongTerm$time>=8)]
  cumulASTOPLOCK <- cumulASTOPLOCK+ode_idSTOPLOCK[[reg]]$LongTerm$A[which(ode_idSTOPLOCK[[reg]]$LongTerm$time>=8)]
  incISTOPLOCK <- incISTOPLOCK+ode_idSTOPLOCK[[reg]]$LongTerm$I[which(ode_idSTOPLOCK[[reg]]$LongTerm$time>=8)]

  cumulEminSTOPLOCK <- cumulEminSTOPLOCK+ode_idSTOPLOCK[[reg]]$LongTermMin$E[which(ode_idSTOPLOCK[[reg]]$LongTerm$time>=8)]
  cumulAminSTOPLOCK <- cumulAminSTOPLOCK+ode_idSTOPLOCK[[reg]]$LongTermMin$A[which(ode_idSTOPLOCK[[reg]]$LongTerm$time>=8)]
  incIminSTOPLOCK <- incIminSTOPLOCK+ode_idSTOPLOCK[[reg]]$LongTermMin$I[which(ode_idSTOPLOCK[[reg]]$LongTerm$time>=8)]

  cumulEmaxSTOPLOCK <- cumulEmaxSTOPLOCK+ode_idSTOPLOCK[[reg]]$LongTermMax$E[which(ode_idSTOPLOCK[[reg]]$LongTerm$time>=8)]
  cumulAmaxSTOPLOCK <- cumulAmaxSTOPLOCK+ode_idSTOPLOCK[[reg]]$LongTermMax$A[which(ode_idSTOPLOCK[[reg]]$LongTerm$time>=8)]
  incImaxSTOPLOCK <- incImaxSTOPLOCK+ode_idSTOPLOCK[[reg]]$LongTermMax$I[which(ode_idSTOPLOCK[[reg]]$LongTerm$time>=8)]



}

datapred$incImaxSTOPLOCK <- incImaxSTOPLOCK
datapred$cumulAmaxSTOPLOCK <- cumulAmaxSTOPLOCK
datapred$cumulEmaxSTOPLOCK <- cumulEmaxSTOPLOCK
datapred$incIminSTOPLOCK <- incIminSTOPLOCK
datapred$cumulAminSTOPLOCK <- cumulAminSTOPLOCK
datapred$cumulEminSTOPLOCK <- cumulEminSTOPLOCK

datapred$incISTOPLOCK <- incISTOPLOCK
datapred$cumulASTOPLOCK <- cumulASTOPLOCK
datapred$cumulESTOPLOCK <- cumulESTOPLOCK
datapred$incINOEFFECT <- incINOEFFECT
datapred$cumulANOEFFECT <- cumulANOEFFECT
datapred$cumulI <- cumulI
datapred$cumulH <- cumulH
datapred$cumulImin <- cumulImin
datapred$cumulImax <- cumulImax
datapred$cumulHmin <- cumulHmin
datapred$cumulHmax <- cumulHmax
datapred$cumulICU <- cumulICU
datapred$cumulICUmin <- cumulICUmin
datapred$cumulICUmax <- cumulICUmax
datapred$cumulD <- cumulD
datapred$cumulDmin <- cumulDmin
datapred$cumulDmax <- cumulDmax

datapred$cumulINOEFFECT <- cumulINOEFFECT
datapred$cumulHNOEFFECT <- cumulHNOEFFECT
datapred$cumulIminNOEFFECT <- cumulIminNOEFFECT
datapred$cumulImaxNOEFFECT <- cumulImaxNOEFFECT
datapred$cumulHminNOEFFECT <- cumulHminNOEFFECT
datapred$cumulHmaxNOEFFECT <- cumulHmaxNOEFFECT
datapred$cumulICUNOEFFECT <- cumulICUNOEFFECT
datapred$cumulICUminNOEFFECT <- cumulICUminNOEFFECT
datapred$cumulICUmaxNOEFFECT <- cumulICUmaxNOEFFECT
datapred$cumulDNOEFFECT <- cumulDNOEFFECT
datapred$cumulDminNOEFFECT <- cumulDminNOEFFECT
datapred$cumulDmaxNOEFFECT <- cumulDmaxNOEFFECT
datapred$cumulENOEFFECT <- cumulENOEFFECT

datapred$cumulISTOPLOCK <- cumulISTOPLOCK
datapred$cumulHSTOPLOCK <- cumulHSTOPLOCK
datapred$cumulIminSTOPLOCK <- cumulIminSTOPLOCK
datapred$cumulImaxSTOPLOCK <- cumulImaxSTOPLOCK
datapred$cumulHminSTOPLOCK <- cumulHminSTOPLOCK
datapred$cumulHmaxSTOPLOCK <- cumulHmaxSTOPLOCK
datapred$cumulICUSTOPLOCK <- cumulICUSTOPLOCK
datapred$cumulICUminSTOPLOCK <- cumulICUminSTOPLOCK
datapred$cumulICUmaxSTOPLOCK <- cumulICUmaxSTOPLOCK
datapred$cumulDSTOPLOCK <- cumulDSTOPLOCK
datapred$cumulDminSTOPLOCK <- cumulDminSTOPLOCK
datapred$cumulDmaxSTOPLOCK <- cumulDmaxSTOPLOCK

## FREE INFORMATION IN THE TEXT
#### IF NO NPI
## DATE PIC I
datapred$time[which(datapred$incINOEFFECT+datapred$cumulENOEFFECT+datapred$cumulANOEFFECT+datapred$cumulHNOEFFECT==max(datapred$incINOEFFECT+datapred$cumulENOEFFECT+datapred$cumulANOEFFECT+datapred$cumulHNOEFFECT))]
##DATA EXTINCT
min(datapred$time[which(datapred$incINOEFFECT+datapred$cumulENOEFFECT+datapred$cumulANOEFFECT+datapred$cumulHNOEFFECT<1)])
## pic Hospit
max(datapred$cumulHNOEFFECT)
datapred$cumulHminNOEFFECT[which(datapred$cumulHNOEFFECT == max(datapred$cumulHNOEFFECT))]
datapred$cumulHmaxNOEFFECT[which(datapred$cumulHNOEFFECT == max(datapred$cumulHNOEFFECT))]
##prevented death
deathwithoutlockdown=datapred$cumulDNOEFFECT[which(datapred$time=="2020-05-11")] - datapred$cumulDNOEFFECT[which(datapred$time=="2020-03-17")]
deathwithlockdown=datapred$cumulD[which(datapred$time=="2020-05-11")] - datapred$cumulD[which(datapred$time=="2020-03-17")]
deathwithoutlockdown-deathwithlockdown

deathwithoutlockdownMIN=datapred$cumulDminNOEFFECT[which(datapred$time=="2020-05-11")] - datapred$cumulDminNOEFFECT[which(datapred$time=="2020-03-17")]
deathwithlockdownMAX=datapred$cumulDmax[which(datapred$time=="2020-05-11")] - datapred$cumulDmax[which(datapred$time=="2020-03-17")]
deathwithoutlockdownMIN-deathwithlockdownMAX

deathwithoutlockdownMAX=datapred$cumulDmaxNOEFFECT[which(datapred$time=="2020-05-11")] - datapred$cumulDmaxNOEFFECT[which(datapred$time=="2020-03-17")]
deathwithlockdownMIN=datapred$cumulDmin[which(datapred$time=="2020-05-11")] - datapred$cumulDmin[which(datapred$time=="2020-03-17")]
deathwithoutlockdownMAX-deathwithlockdownMIN

#In percentages
1-deathwithlockdown/deathwithoutlockdown
1-deathwithlockdownMIN/deathwithoutlockdownMAX
1-deathwithlockdownMAX/deathwithoutlockdownMIN
###


### IF ETERNAL LOCKDOWN OPTIMAL TIME##
timeend <- min(datapred$time[which(datapred$incISTOPLOCK+datapred$cumulESTOPLOCK+datapred$cumulASTOPLOCK<1)])
timeend-as.Date("2020-05-11")
timeend <- min(datapred$time[which(datapred$incIminSTOPLOCK+datapred$cumulEminSTOPLOCK+datapred$cumulAminSTOPLOCK<1)])
timeend-as.Date("2020-05-11")
timeend <- min(datapred$time[which(datapred$incImaxSTOPLOCK+datapred$cumulEmaxSTOPLOCK+datapred$cumulAmaxSTOPLOCK<1)])
timeend-as.Date("2020-05-11")
###



datapred <- datapred %>% filter(time < as.Date("2020-07-15"))
logscale=TRUE
log_title  <-  ""
if(logscale){
  log_title  <-  " (log-scale)"
}

library(ggplot2)
p1  <-  ggplot(datapred,  aes(x=time,  y=cumulI)) +
  xlab("Date") +
  geom_vline(aes(xintercept = as.Date("2020-03-25"), linetype="Incident cases"),  color="red3") +
  geom_vline(aes(xintercept = as.Date("2020-05-11"), linetype="Hospitalizations"),  color="black") +
  geom_line(aes(col="Infinite Lockdown")) +
  geom_line(aes(y=cumulINOEFFECT, color="no intervention")) +
  geom_line(aes(y=cumulISTOPLOCK, color="Back to pre-lockdown\n on 2020-05-11"))+
  geom_ribbon(aes(ymin = cumulIminNOEFFECT,  ymax = cumulImaxNOEFFECT,
                  fill = "no intervention",
                  alpha="no intervention"))+
  geom_ribbon(aes(ymin = cumulIminSTOPLOCK,  ymax = cumulImaxSTOPLOCK,
                  fill = "Back to pre-lockdown\n on 2020-05-11",
                  alpha="Back to pre-lockdown\n on 2020-05-11"))+
  geom_ribbon(aes(ymin = cumulImin,  ymax = cumulImax,
                  fill = "Infinite Lockdown",
                  alpha="Infinite Lockdown")) +
  geom_point(data=datagouv,  aes(x=time, y=Iobs,  shape="Source: Santé\nPublique France")) +
  scale_shape("Observations") +
  scale_alpha_manual("Estimate (95% CI)",  values=c(0.25,  0.25,  0.25),
                     breaks = c("no intervention",  "Infinite Lockdown",
                                "Back to pre-lockdown\n on 2020-05-11")) +
  scale_color_manual("Estimate (95% CI)",  values=c("purple4",  "red3",  "skyblue"),
                     breaks = c("no intervention",  "Infinite Lockdown",
                                "Back to pre-lockdown\n on 2020-05-11")) +
  scale_fill_manual("Estimate (95% CI)",  values=c("purple4",  "red3",  "skyblue"),
                    breaks = c("no intervention",  "Infinite Lockdown",
                               "Back to pre-lockdown\n on 2020-05-11")) +
  scale_linetype_manual("End of data collection",  values=c(2, 2),
                        breaks = c("Incident cases", "Hospitalizations")) +
  guides(linetype = guide_legend(override.aes = list(color=c("red3", "black")))) +
  theme_classic() +
  ylab(paste0("National cumulative incidence\nof ascertained cases",  log_title)) +
  ggtitle("France")

p2  <-  ggplot(datapred,  aes(x=time,  y=cumulH)) +
  xlab("Date") +
  geom_vline(aes(xintercept = as.Date("2020-03-25"), linetype="Incident cases"),  color="red3") +
  geom_vline(aes(xintercept = as.Date("2020-05-11"), linetype="Hospitalizations"),  color="black") +
  geom_line(aes(col="Infinite Lockdown")) +
  geom_line(aes(y=cumulHNOEFFECT, color="no intervention")) +
  geom_line(aes(y=cumulHSTOPLOCK, color="Back to pre-lockdown\n on 2020-05-11"))+
  geom_ribbon(aes(ymin = cumulHminNOEFFECT,  ymax = cumulHmaxNOEFFECT,
                  fill = "no intervention",
                  alpha="no intervention"))+
  geom_ribbon(aes(ymin = cumulHminSTOPLOCK,  ymax = cumulHmaxSTOPLOCK,
                  fill = "Back to pre-lockdown\n on 2020-05-11",
                  alpha="Back to pre-lockdown\n on 2020-05-11"))+
  geom_ribbon(aes(ymin = cumulHmin,  ymax = cumulHmax,
                  fill = "Infinite Lockdown",
                  alpha="Infinite Lockdown")) +
  geom_point(data=datagouv,  aes(x=time, y=Hallobs,  shape="Source: Santé\nPublique France")) +
  #geom_point(data=datagouv,  aes(x=time, y=Hallobs)) +
  #geom_line(data=datagouv,  aes(x=time, y=Hobs)) +
  scale_shape("Observations") +
  scale_alpha_manual("Estimate (95% CI)",  values=c(0.25,  0.25,  0.25),
                     breaks = c("no intervention",  "Infinite Lockdown",
                                "Back to pre-lockdown\n on 2020-05-11")) +
  scale_color_manual("Estimate (95% CI)",  values=c("purple4",  "red3",  "skyblue"),
                     breaks = c("no intervention",  "Infinite Lockdown",
                                "Back to pre-lockdown\n on 2020-05-11")) +
  scale_fill_manual("Estimate (95% CI)",  values=c("purple4",  "red3",  "skyblue"),
                    breaks = c("no intervention",  "Infinite Lockdown",
                               "Back to pre-lockdown\n on 2020-05-11")) +
  scale_linetype_manual("End of data collection",  values=c(2, 2),
                        breaks = c("Incident cases", "Hospitalizations")) +
  guides(linetype = guide_legend(override.aes = list(color=c("red3", "black")))) +
  theme_classic() +
  ylab(paste0("National prevalence\nof hospitalized cases",  log_title)) +
  ggtitle("France") +
  scale_y_log10()

p3 <- ggplot(datapred,  aes(x=time,  y=cumulICU)) +
  xlab("Date") +
  geom_vline(aes(xintercept = as.Date("2020-03-25"), linetype="Incident cases"),  color="red3") +
  geom_vline(aes(xintercept = as.Date("2020-05-11"), linetype="Hospitalizations"),  color="black") +
  geom_line(aes(col="Infinite Lockdown")) +
  geom_line(aes(y=cumulICUNOEFFECT, color="no intervention")) +
  geom_line(aes(y=cumulICUSTOPLOCK, color="Back to pre-lockdown\n on 2020-05-11"))+
  geom_ribbon(aes(ymin = cumulICUminNOEFFECT,  ymax = cumulICUmaxNOEFFECT,
                  fill = "no intervention",
                  alpha="no intervention"))+
  geom_ribbon(aes(ymin = cumulICUminSTOPLOCK,  ymax = cumulICUmaxSTOPLOCK,
                  fill = "Back to pre-lockdown\n on 2020-05-11",
                  alpha="Back to pre-lockdown\n on 2020-05-11"))+
  geom_ribbon(aes(ymin = cumulICUmin,  ymax = cumulICUmax,
                  fill = "Infinite Lockdown",
                  alpha="Infinite Lockdown")) +
  geom_point(data=datagouv,  aes(x=time, y=ICUobs,  shape="Source: Santé\nPublique France")) +
  scale_shape("Observations") +
  scale_alpha_manual("Estimate (95% CI)",  values=c(0.25,  0.25,  0.25),
                     breaks = c("no intervention",  "Infinite Lockdown",
                                "Back to pre-lockdown\n on 2020-05-11")) +
  scale_color_manual("Estimate (95% CI)",  values=c("purple4",  "red3",  "skyblue"),
                     breaks = c("no intervention",  "Infinite Lockdown",
                                "Back to pre-lockdown\n on 2020-05-11")) +
  scale_fill_manual("Estimate (95% CI)",  values=c("purple4",  "red3",  "skyblue"),
                    breaks = c("no intervention",  "Infinite Lockdown",
                               "Back to pre-lockdown\n on 2020-05-11")) +
  scale_linetype_manual("End of data collection",  values=c(2, 2),
                        breaks = c("Incident cases", "Hospitalizations")) +
  guides(linetype = guide_legend(override.aes = list(color=c("red3", "black")))) +
  theme_classic() +
  ylab(paste0("National prevalence\nof ICU cases",  log_title)) +
  ggtitle("France")

p4 <- ggplot(datapred,  aes(x=time,  y=cumulD)) +
  xlab("Date") +
  geom_vline(aes(xintercept = as.Date("2020-03-25"), linetype="Incident cases"),  color="red3") +
  geom_vline(aes(xintercept = as.Date("2020-05-11"), linetype="Hospitalizations"),  color="black") +
  geom_line(aes(col="Infinite Lockdown")) +
  geom_line(aes(y=cumulDNOEFFECT, color="no intervention")) +
  geom_line(aes(y=cumulDSTOPLOCK, color="Back to pre-lockdown\n on 2020-05-11"))+
  geom_ribbon(aes(ymin = cumulDminNOEFFECT,  ymax = cumulDmaxNOEFFECT,
                  fill = "no intervention",
                  alpha="no intervention"))+
  geom_ribbon(aes(ymin = cumulDminSTOPLOCK,  ymax = cumulDmaxSTOPLOCK,
                  fill = "Back to pre-lockdown\n on 2020-05-11",
                  alpha="Back to pre-lockdown\n on 2020-05-11"))+
  geom_ribbon(aes(ymin = cumulDmin,  ymax = cumulDmax,
                  fill = "Infinite Lockdown",
                  alpha="Infinite Lockdown")) +
  geom_point(data=datagouv,  aes(x=time, y=Dobs,  shape="Source: Santé\nPublique France")) +
  scale_shape("Observations") +
  scale_alpha_manual("Estimate (95% CI)",  values=c(0.25,  0.25,  0.25),
                     breaks = c("no intervention",  "Infinite Lockdown",
                                "Back to pre-lockdown\n on 2020-05-11")) +
  scale_color_manual("Estimate (95% CI)",  values=c("purple4",  "red3",  "skyblue"),
                     breaks = c("no intervention",  "Infinite Lockdown",
                                "Back to pre-lockdown\n on 2020-05-11")) +
  scale_fill_manual("Estimate (95% CI)",  values=c("purple4",  "red3",  "skyblue"),
                    breaks = c("no intervention",  "Infinite Lockdown",
                               "Back to pre-lockdown\n on 2020-05-11")) +
  scale_linetype_manual("End of data collection",  values=c(2, 2),
                        breaks = c("Incident cases", "Hospitalizations")) +
  guides(linetype = guide_legend(override.aes = list(color=c("red3", "black")))) +
  theme_classic() +
  ylab(paste0("National cumulative number\nof death",  log_title)) +
  ggtitle("France")

if(logscale){
  p1  <-  p1 + scale_y_log10() + annotation_logticks(sides = "l")
  p2  <-  p2 + scale_y_log10() + annotation_logticks(sides = "l")
  p3  <-  p3 + scale_y_log10() + annotation_logticks(sides = "l")
  p4  <-  p4 + scale_y_log10() + annotation_logticks(sides = "l")
}

if(logscale){
  logindicator  <-  "_logscale"
}else{
  logindicator  <-  ""
}

library(patchwork)
device="pdf"
old.loc  <-  Sys.getlocale("LC_TIME")
Sys.setlocale("LC_TIME",  "en_GB.UTF-8")

# p1 + (p2 +ggtitle("")) + plot_layout(ncol=1, nrow=2, guides = "collect")
#
# if(device == "pdf"){
#   ggsave(file = paste("./MonolixFile/outputMonolix/", nameproject, "/graphics/vsdata",  logindicator,  ".pdf", sep=""),
#          device = "pdf",  width=6,  height=8)
# }else if(device == "jpg"){
#   ggsave(file = paste("./MonolixFile/outputMonolix/", nameproject, "/graphics/vsdata",  logindicator, ".jpg", sep=""),
#          device = "jpeg",  dpi =300,  width=6,  height=8)
# }
#
# (p1 + (p2+ggtitle("")))/((p3+ggtitle("")) + (p4+ggtitle(""))) + plot_layout(guides = "collect")
# if(device == "pdf"){
#   ggsave(file = paste("./MonolixFile/outputMonolix/", nameproject, "/graphics/vsdataall",  logindicator,  ".pdf", sep=""),
#          device = "pdf",  width=11,  height=8)
# }else if(device == "jpg"){
#   ggsave(file = paste("./MonolixFile/outputMonolix/", nameproject, "/graphics/vsdataall",  logindicator,  ".jpg", sep=""),
#          device = "jpeg",  dpi = 300,  width=11,  height=8)
# }

(p1 + (p2+ggtitle("")))+ ((p4+ggtitle(""))) + plot_layout(guides='collect') &
  theme(legend.position='bottom', legend.direction = "vertical")
if(device == "pdf"){
  ggsave(file = paste("./MonolixFile/outputMonolix/", nameproject, "/graphics/vsdata3",  logindicator,  ".pdf", sep=""),
         device = "pdf",  width=10,  height=6)
}else if(device == "jpg"){
  ggsave(file = paste("./MonolixFile/outputMonolix/", nameproject, "/graphics/vsdata3",  logindicator,  ".jpg", sep=""),
         device = "jpeg",  dpi = 300,  width=10,  height=6)
}
Sys.setlocale("LC_TIME",  old.loc)



### FIGURE 4 - R0 ----

old.loc  <-  Sys.getlocale("LC_TIME")
Sys.setlocale("LC_TIME",  "en_GB.UTF-8")
p <- PlotArticleR0(ode_id)
Sys.setlocale("LC_TIME",  old.loc)

### FIGURE 5 - MAP ----
time_date <- seq(as.Date("2020-03-02"),  as.Date("2020-05-11"),  "days")
time <- seq(0, length(time_date)-1, 1)
x <- list()
x[[1]]  <-  list(name='timeSinceConf',
                 time=time,
                 value=rep(0, length(time)))
x[[1]]$value[16:length(time)] <- seq(1, length(time)-16+1, 1)
reg_info <- x
ode_idATAACK <- EstimateLongTerm(ode_id, time_date, time, reg_info, TimeDependantParameter)
plot_info <- PlotSolutionLongTerm(ode_idATAACK)

## ATTACKRATES MAP
SpecificDateAttackRateEquation <- "(R+I+A+E)/popsize*100"
attackrates <- GetAttackRateAtSpecificDate(ode_idATAACK, SpecificDateAttackRateEquation, as.Date("2020-05-11"), popsize_name =popsize_name, NationalName = "France")


fill_info  <-  cbind.data.frame("name" = as.character(full_region_names(attackrates$Reg[1:12])),
                                "fill_value" = as.numeric(attackrates$ARmean[1:12]),
                                stringsAsFactors = FALSE)

library(mapproj)
pmap <- french_regions_map(fill_info,  mytitle = "",
                           one_out_of = 1,  show_labels = TRUE)

ggsave(plot = pmap,  file=paste("./MonolixFile/outputMonolix/", nameproject, "/graphics/map.pdf", sep=""),
       width=8,  height=6.4)
ggsave(plot = pmap,  file=paste("./MonolixFile/outputMonolix/", nameproject, "/graphics/map600.jpg", sep=""),
       width=8,  height=6.4,  dpi = 600, device = "jpeg")
#ggsave(plot = pmap,  file=paste("./MonolixFile/outputMonolix/", nameproject, "/graphics/map300.jpg", sep=""),
#       width=8,  height=6.4,  dpi = 300, device = "jpeg")

######  APPENDIX KALMAN ----

KALMAN1 <- read.table("./MonolixFile/outputMonolix/fourstepEAKalman_small/graphics/estim_for_R_zero.txt", header=TRUE)
KALMAN2 <- read.table("./MonolixFile/outputMonolix/fourstepEAKalman_small/graphics/estim_for_R_simple.txt", header=TRUE)
KALMAN3 <- read.table("./MonolixFile/outputMonolix/fourstepEAKalman_small/graphics/", header=TRUE)
KALMAN1$ID <- full_region_names(KALMAN1$ID)
KALMAN2$ID <- full_region_names(KALMAN2$ID)
KALMAN1$date <- as.Date(KALMAN1$date)
KALMAN2$date <- as.Date(KALMAN2$date)


old.loc  <-  Sys.getlocale("LC_TIME")
Sys.setlocale("LC_TIME",  "en_GB.UTF-8")

p1 <- ggplot(KALMAN1,  aes_(x=as.name("date"), y=as.name("B"), group=as.name("ID"))) +
  geom_line(color="black") +
  scale_linetype_manual("",  values = c(2,  1)) +
  geom_ribbon(aes_(ymin = as.name("Bmin"),  ymax=as.name("Bmax"), alpha="Region-wise value\n(95% CI)"),  fill="red3")+
  facet_grid(vars(ID),  scales = "free_y") + facet_wrap(~ ID,  ncol=4)+
  geom_vline(xintercept = as.Date("2020-03-17"))+
  geom_vline(xintercept = as.Date("2020-03-17")+7, linetype=2)+
  geom_vline(xintercept = as.Date("2020-03-17")+14, linetype=2)+
  geom_vline(xintercept = as.Date("2020-03-17")+21, linetype=2)+
  scale_alpha_manual(values=c(0.3)) +
  theme_bw() +
  theme(strip.background = element_rect(fill="white")) +
  ylab(expression(paste("Transmission rate"))) +
  xlab(expression(paste("Date"))) +
  ylim(c(0, 5)) +
  guides(linetype=guide_legend(title=""), alpha=guide_legend(title=""))+
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(angle=45,  hjust=1)) +
  theme(axis.text.y = element_text(size=8)) +
  theme(strip.background = element_rect(fill="white"),
        strip.text = element_text(size=12))
p1
Sys.setlocale("LC_TIME",  old.loc)
