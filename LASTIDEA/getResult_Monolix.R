rm(list = ls())
library(xtable)
library(ggplot2)
library(lubridate)
library(gridExtra)
library(forcats)
library(dplyr)
library(glue)
source("./LASTIDEA/routineResults.R")


path<-"./LASTIDEA/"
nameproject<-"Final_20200325/"
dataname<-"data_monolix_20200325.txt"
nameprojectupdate<-"Final_Update_20200410/"

typecov<-"constant"
alphafixed<-0.55
Defixed<-5.1
Difixed<-2.3
Dhfixed<-30
timings<-15

## Get Individual Parameters & data
indivParams = read.table(paste(path,"/outputMonolix/",nameproject,"/IndividualParameters/estimatedIndividualParameters.txt",sep=""),header=TRUE,sep=",")
indivParamsUP = read.table(paste(path,"/outputMonolix/",nameprojectupdate,"/IndividualParameters/estimatedIndividualParameters.txt",sep=""),header=TRUE,sep=",")
popParams= read.table(paste(path,"/outputMonolix/",nameproject,"/populationParameters.txt",sep=""),header=TRUE,sep=",")
popParamsUP = read.table(paste(path,"/outputMonolix/",nameprojectupdate,"/populationParameters.txt",sep=""),header=TRUE,sep=",")

dir.create(paste(path,"outputMonolix/",nameproject,"/graphics",sep=""))
data<-read.table(paste(path,dataname,sep=""),sep="\t",header=TRUE)
## Get rfrom sentinelle
for (i in 1:length(indivParams$id)){
    indivParams$r_sent[i]<-data$ascertainement[which((data$IDname==indivParams$id[i])&(data$day==0)&(data$obs_id==1))]
}
data$date<-lubridate::as_date(as.character(data$date))
timesconfinement<-data[which((data$date=="2020-03-17")&(data$obs_id==1)),c("day","IDname")]
## Get population size
load("./data/popreg.RData")
popreg$idnames<-c("AURA","BFC","Bretagne","Centre","Corse","GrandEst","HDF","IDF","Normandie","NAquitaine","Occitanie","PaysLoire","PACA","G1","G2","G3","G4","G5","France")
for(i in 1:length(indivParams$id)){
    indivParams$popsize[i]<-popreg$population[which(popreg$idnames==indivParams$id[i])]
}
#data[which(data$day==0),c("reg_id","IDname")]
## Get ICU
load("./data/ICUcapacity_FR.RData")
ICUcapacity_FR$idnames<-c("G1","G2","G3","G4","G5","IDF","Centre","BFC","Normandie","HDF","GrandEst","PaysLoire","Bretagne","NAquitaine","Occitanie","AURA","PACA","Corse","France")
for(i in 1:length(indivParams$id)){
  indivParams$ICUcapacity[i]<-ICUcapacity_FR$nbICU_adult[which(ICUcapacity_FR$idnames==indivParams$id[i])]
}

R0s_list <- list()
solutions_list <- list()
predictions_list <- list()
solutionsNOEFFECT_list <- list()
solutionsUPDATED_list <- list()
solutionsCOMBINED_list<-list()
predictionsNOEFFECT_list<- list()
predictionsUPDATED_list<- list()
predictionsCOMBINED_list<- list()

for (i in 1:length(indivParams$id)){
    message(as.character(indivParams$id[i]), " (",i,"/",length(indivParams$id),") ...")
    b<-as.numeric(indivParams[i,c("b1_mode")])
    r<-as.numeric(indivParams[i,c("r_sent")])
    dataregion<-data[which(data$IDname==as.character(indivParams[i,1])),]
    alpha<-alphafixed
    De<-Defixed
    Di<-Difixed
    Dq<-as.numeric(indivParams[i,c("Dq_mode")])
    Dh<-Dhfixed
    b3<-0
    popSize<-dataregion$popsize[1]
    E0given<-as.numeric(indivParams[i,c("E0_mode")])
    A0given<-10000
    if(typecov=="constant"){
    b2<-as.numeric(indivParams[i,"betat1_mode"])
    }
    if(typecov=="parametric"){
      b2<-c(as.numeric(indivParams[i,"betat1_mode"]),timings)
    }
    if(typecov=="splines"){
      b2<-c(as.numeric(indivParams[i,"beta1_mode"]),as.numeric(indivParams[i,"beta2_mode"]),as.numeric(indivParams[i,"beta3_mode"]))
    }
    tconf<-timesconfinement[which(timesconfinement$IDname==as.character(indivParams[i,1])),1]

    bsd<-sqrt(as.numeric(indivParams[i,"b1_sd"])**2)#+as.numeric(popParams[which(popParams$parameter=="b1_pop"),"se_sa"])**2)
    Dqsd<-sqrt(as.numeric(indivParams[i,"Dq_sd"])**2)#+as.numeric(popParams[which(popParams$parameter=="Dq_pop"),"se_sa"])**2)
    E0sd<-sqrt(as.numeric(indivParams[i,"E0_sd"])**2)#+as.numeric(popParams[which(popParams$parameter=="E0_pop"),"se_sa"])**2)
    A0sd<-0
    betasd<-as.numeric(popParams[which(popParams$parameter=="betat1_pop"),"se_sa"])
    beta2sd<-0
    
    solution <- getSolution( b,
                             r,
                             dataregion,
                             alpha,
                             De,
                             Di,
                             Dq,
                             Dh,
                             popSize,
                             E0given,
                             A0given,b2,b3,tconf,typecov,1000,0,FALSE,bsd,Dqsd,E0sd,A0sd,betasd,beta2sd, CI=TRUE,ncores=parallel::detectCores()-1)

    
    solutionNOEFFECT <- getSolution( b,
                                     r,
                                     dataregion,
                                     alpha,
                                     De,
                                     Di,
                                     Dq,
                                     Dh,
                                     popSize,
                                     E0given,
                                     A0given,0.0,0.0,tconf,typecov,1000,0,FALSE,bsd,Dqsd,E0sd,A0sd,betasd,beta2sd, CI=TRUE,ncores=parallel::detectCores()-1)
    
    bUP<-as.numeric(indivParamsUP[i,c("b1_mode")])
    DqUP<-as.numeric(indivParamsUP[i,c("Dq_mode")])
    E0givenUP<-as.numeric(indivParamsUP[i,c("E0_mode")])
    A0givenUP<-as.numeric(indivParamsUP[i,c("pctA0_mode")])
    if(typecov=="constant"){
      b2UP<-as.numeric(indivParamsUP[i,"betat1_mode"])
      b3UP<-as.numeric(indivParamsUP[i,"betat2_mode"])
    }
    beta2sdUP<-as.numeric(popParamsUP[which(popParamsUP$parameter=="betat2_pop"),"se_sa"])
    

    solutionUPDATED <- getSolution( bUP,
                             r,
                             dataregion,
                             alpha,
                             De,
                             Di,
                             DqUP,
                             Dh,
                             popSize,
                             E0givenUP,
                             A0givenUP,b2UP,b3UP,tconf,typecov,1000,0,FALSE,bsd,Dqsd,E0sd,A0sd,betasd,beta2sdUP, CI=TRUE,ncores=parallel::detectCores()-1)

    solutionCOMBINED <- getSolution( b,
                             r,
                             dataregion,
                             alpha,
                             De,
                             Di,
                             Dq,
                             Dh,
                             popSize,
                             E0given,
                             A0given,b2UP,b3UP,tconf,typecov,1000,0,FALSE,bsd,Dqsd,E0sd,A0sd,betasd,beta2sdUP, CI=TRUE,ncores=parallel::detectCores()-1)

    solution$solution$date <- seq.Date(from = dataregion$date[1], by=1, length.out = nrow(solution$solution))
    solutionCOMBINED$solution$date <- seq.Date(from = dataregion$date[1], by=1, length.out = nrow(solutionCOMBINED$solution))
    solutionUPDATED$solution$date <- seq.Date(from = dataregion$date[1], by=1, length.out = nrow(solutionUPDATED$solution))
    solutionNOEFFECT$solution$date <- seq.Date(from = dataregion$date[1], by=1, length.out = nrow(solutionNOEFFECT$solution))

    solutionNOEFFECT$solution$reg <- as.character(indivParams$id[i])
    solutionUPDATED$solution$reg <- as.character(indivParams$id[i])
    solution$solution$reg <- as.character(indivParams$id[i])
    solutionCOMBINED$solution$reg <- as.character(indivParams$id[i])
    solutions_list[[i]] <- solution
    solutionsNOEFFECT_list[[i]] <- solutionNOEFFECT
    solutionsUPDATED_list[[i]] <- solutionUPDATED
    solutionsCOMBINED_list[[i]] <- solutionCOMBINED

    getPlot(solutionCOMBINED,nameproject,indivParams[i,],path)

    res<-getR0(solution,indivParams[i,],typecov,timings,indivParamsUP[i,],solutionCOMBINED)
    R0s_list[[i]] <-  res

    getPlotR0(res,nameproject,indivParams[i,])

    indivParams[i,c("R0","R0min","R0max")]<-res[which(res$time==(tconf-1)),c("R0","R0ICmin","R0ICmax")]
    indivParams[i,c("R0conf","R0minconf","R0maxconf")]<-res[which(res$time==tconf+2),c("R0","R0ICmin","R0ICmax")]
    indivParams[i,c("R0conf2","R0minconf2","R0maxconf2")]<-res[which(res$time==tconf+20),c("R0","R0ICmin","R0ICmax")]
    indivParams[i,"timestart"]<-as.character(dataregion$date[which((dataregion$day==0)&(dataregion$obs_id==1))])
    indivParams[i,"Icumul"]<-sum(dataregion$obs[which((dataregion$obs_id==1))])
    indivParams[i,"Hcumul"]<-sum(dataregion$obs[which((dataregion$obs_id==2))])
    res <- getIHD(solution,indivParams[i,])
    resNOEFFECT <- getIHD(solutionNOEFFECT,indivParams[i,])
    resUPDATED <- getIHD(solutionUPDATED,indivParams[i,])
    resCOMBINED <- getIHD(solutionCOMBINED,indivParams[i,])

    predictionsCOMBINED_list[[i]] <- resCOMBINED
    predictionsNOEFFECT_list[[i]] <- resNOEFFECT
    predictionsUPDATED_list[[i]] <- resUPDATED
    predictions_list[[i]] <- res
    message("Done\n")
}


saveRDS(R0s_list, file = "./LASTIDEA/data/all_R0s_df_final20200416.rds")
saveRDS(solutions_list, file = "./LASTIDEA/data/solutions_list20200416.rds")
saveRDS(solutionsUPDATED_list, file = "./LASTIDEA/data/solutionsUPDATED_list20200416.rds")
saveRDS(solutionsNOEFFECT_list, file = "./LASTIDEA/data/solutionsNOEFFECT_list20200416.rds")
saveRDS(solutionsCOMBINED_list, file = "./LASTIDEA/data/solutionsCOMBINED_list20200416.rds")
saveRDS(predictions_list, file = "./LASTIDEA/data/predictions20200416.rds")
saveRDS(predictionsUPDATED_list, file = "./LASTIDEA/data/predictionsUPDATED20200416.rds")
saveRDS(predictionsNOEFFECT_list, file = "./LASTIDEA/data/predictionsNOEFFECT20200416.rds")
saveRDS(predictionsCOMBINED_list, file = "./LASTIDEA/data/predictionsCOMBINED20200416.rds")

# # 
  R0s_list<-readRDS(file = "./data/all_R0s_df_final20200416.rds")
  solutions_list<-readRDS(file = "./data/solutions_list20200416.rds")#
  solutionsUPDATED_list<-readRDS(file = "./data/solutionsUPDATED_list20200416.rds")
  solutionsNOEFFECT_list<-readRDS(file = "./data/solutionsNOEFFECT_list20200416.rds")
  solutionsCOMBINED_list<-readRDS(file = "./data/solutionsCOMBINED_list20200416.rds")
  predictions_list<-readRDS(file = "./data/predictions20200416.rds")
  predictionsUPDATED_list<-readRDS(file = "./data/predictionsUPDATED20200416.rds")#
  predictionsNOEFFECT_list<-readRDS(file = "./data/predictionsNOEFFECT20200416.rds")
  predictionsCOMBINED_list<-readRDS(file = "./data/predictionsCOMBINED20200416.rds")

getPlotSolutionAll(solutions_list, nameproject = nameproject)


#### REFF ----
all_R0s_df <- do.call(rbind.data.frame, R0s_list)
getPlotR0all(all_R0s_df, nameproject = nameproject,path,timings,typecov,
          Di=Difixed, alpha=alphafixed, facet_scales = "free_y")


### Get Table Article ----
getindicators(indivParams)




### PREDICTION COURT TERME -----
#predictions_list <- readRDS("data/predictions20200411.rds")
#predictionsUPDATED_list <- readRDS("data/predictionsUPDATED20200411.rds")
#predictionsNOEFFECT_list <- readRDS("data/predictionsNOEFFECT20200411.rds")
#predictionsCOMBINED_list <- readRDS("data/predictionsCOMBINED20200411.rds")

# predictions 10 jours
predictions <- do.call(rbind.data.frame, predictions_list)
predictionsUPDATED <- do.call(rbind.data.frame, predictionsUPDATED_list)
predictionsNOEFFECT <- do.call(rbind.data.frame, predictionsNOEFFECT_list)
predictionsCOMBINED <- do.call(rbind.data.frame, predictionsCOMBINED_list)
getPlotPredictionShortterm(predictions,predictionsCOMBINED,predictionsNOEFFECT,nameproject, logscale=TRUE)
getPlotPredictionShortterm(predictions,predictionsCOMBINED,predictionsNOEFFECT,nameproject, logscale=FALSE)


### INDICATEURS -----

# 6=17/03
# 33=13/04
# 61 11/05
# 89 8/06
# 103 22/06

predictionsUSED <- predictionsCOMBINED
result<-as.data.frame(indivParams$id)
names(result)<-"reg"
k<-1
for (region in unique(result$reg)){
  #  result$immunised0[k]<-as.numeric(predictionsUSED$immunised[which((predictionsUSED$reg==region)&(predictionsUSED$i==7))])/popreg$population[which(popreg$idnames==region)]
    result$infected0[k]<-as.numeric(predictionsUSED$infected[which((predictionsUSED$reg==region)&(predictionsUSED$i==6))])/popreg$population[which(popreg$idnames==region)]
    result$infected0min[k]<-as.numeric(predictionsUSED$infectedmin[which((predictionsUSED$reg==region)&(predictionsUSED$i==6))])/popreg$population[which(popreg$idnames==region)]
    result$infected0max[k]<-as.numeric(predictionsUSED$infectedmax[which((predictionsUSED$reg==region)&(predictionsUSED$i==6))])/popreg$population[which(popreg$idnames==region)]
    #  result$immunised37[k]<-as.numeric(predictionsUSED$immunised[which((predictionsUSED$reg==region)&(predictionsUSED$i==37))])/popreg$population[which(popreg$idnames==region)]
    result$infected37[k]<-as.numeric(predictionsUSED$infected[which((predictionsUSED$reg==region)&(predictionsUSED$i==33))])/popreg$population[which(popreg$idnames==region)]
    result$infected37min[k]<-as.numeric(predictionsUSED$infectedmin[which((predictionsUSED$reg==region)&(predictionsUSED$i==33))])/popreg$population[which(popreg$idnames==region)]
    result$infected37max[k]<-as.numeric(predictionsUSED$infectedmax[which((predictionsUSED$reg==region)&(predictionsUSED$i==33))])/popreg$population[which(popreg$idnames==region)]
    # result$immunised21[k]<-as.numeric(predictionsUSED$immunised[which((predictionsUSED$reg==region)&(predictionsUSED$i==28))])/popreg$population[which(popreg$idnames==region)]
    result$infected52[k]<-as.numeric(predictionsUSED$infected[which((predictionsUSED$reg==region)&(predictionsUSED$i==61))])/popreg$population[which(popreg$idnames==region)]
    result$infected52min[k]<-as.numeric(predictionsUSED$infectedmin[which((predictionsUSED$reg==region)&(predictionsUSED$i==61))])/popreg$population[which(popreg$idnames==region)]
    result$infected52max[k]<-as.numeric(predictionsUSED$infectedmax[which((predictionsUSED$reg==region)&(predictionsUSED$i==61))])/popreg$population[which(popreg$idnames==region)]
    # result$immunised90[k]<-as.numeric(predictionsUSED$immunised[which((predictionsUSED$reg==region)&(predictionsUSED$i==97))])/popreg$population[which(popreg$idnames==region)]
    #result$infected90[k]<-as.numeric(predictionsUSED$infected[which((predictionsUSED$reg==region)&(predictionsUSED$i==97))])/popreg$population[which(popreg$idnames==region)]
   # result$immunised1000[k]<-as.numeric(predictionsUSED$immunised[which((predictionsUSED$reg==region)&(predictionsUSED$i==700))])/popreg$population[which(popreg$idnames==region)]
    #result$infected1000[k]<-as.numeric(predictionsUSED$infected[which((predictionsUSED$reg==region)&(predictionsUSED$i==700))])/popreg$population[which(popreg$idnames==region)]
    result$infected67[k]<-as.numeric(predictionsUSED$infected[which((predictionsUSED$reg==region)&(predictionsUSED$i==89))])/popreg$population[which(popreg$idnames==region)]
    result$infected67min[k]<-as.numeric(predictionsUSED$infectedmin[which((predictionsUSED$reg==region)&(predictionsUSED$i==89))])/popreg$population[which(popreg$idnames==region)]
    result$infected67max[k]<-as.numeric(predictionsUSED$infectedmax[which((predictionsUSED$reg==region)&(predictionsUSED$i==89))])/popreg$population[which(popreg$idnames==region)]

    result$infected97[k]<-as.numeric(predictionsUSED$infected[which((predictionsUSED$reg==region)&(predictionsUSED$i==103))])/popreg$population[which(popreg$idnames==region)]
    result$infected97min[k]<-as.numeric(predictionsUSED$infectedmin[which((predictionsUSED$reg==region)&(predictionsUSED$i==103))])/popreg$population[which(popreg$idnames==region)]
    result$infected97max[k]<-as.numeric(predictionsUSED$infectedmax[which((predictionsUSED$reg==region)&(predictionsUSED$i==103))])/popreg$population[which(popreg$idnames==region)]
    k<-k+1
}
sizeFR<-popreg$population[which(popreg$maille_code=="FRA")]


# Dates are 17/03 14/04 1/05 15/05
names(result)
result$reg<-as.character(result$reg)
result<-rbind(result,c("France",sum(as.numeric(predictionsUSED$infected[which(predictionsUSED$i==6)]))/sizeFR,sum(as.numeric(predictionsUSED$infectedmin[which(predictionsUSED$i==6)]))/sizeFR,sum(as.numeric(predictionsUSED$infectedmax[which(predictionsUSED$i==6)]))/sizeFR,sum(as.numeric(predictionsUSED$infected[which(predictionsUSED$i==33)]))/sizeFR,sum(as.numeric(predictionsUSED$infectedmin[which(predictionsUSED$i==33)]))/sizeFR,sum(as.numeric(predictionsUSED$infectedmax[which(predictionsUSED$i==33)]))/sizeFR,sum(as.numeric(predictionsUSED$infected[which(predictionsUSED$i==61)]))/sizeFR,sum(as.numeric(predictionsUSED$infectedmin[which(predictionsUSED$i==61)]))/sizeFR,sum(as.numeric(predictionsUSED$infectedmax[which(predictionsUSED$i==61)]))/sizeFR,sum(as.numeric(predictionsUSED$infected[which(predictionsUSED$i==89)]))/sizeFR,sum(as.numeric(predictionsUSED$infectedmin[which(predictionsUSED$i==89)]))/sizeFR,sum(as.numeric(predictionsUSED$infectedmax[which(predictionsUSED$i==89)]))/sizeFR,sum(as.numeric(predictionsUSED$infected[which(predictionsUSED$i==103)]))/sizeFR,sum(as.numeric(predictionsUSED$infectedmin[which(predictionsUSED$i==103)]))/sizeFR,sum(as.numeric(predictionsUSED$infectedmax[which(predictionsUSED$i==103)]))/sizeFR))

result$infected0<-round(as.numeric(result$infected0)*100,1)
result$infected0min<-pmax(0,round(as.numeric(result$infected0min)*100,1))
result$infected0max<-pmin(100,round(as.numeric(result$infected0max)*100,1))
result$infected37<-round(as.numeric(result$infected37)*100,1)
result$infected37min<-pmax(0,round(as.numeric(result$infected37min)*100,1))
result$infected37max<-pmin(100,round(as.numeric(result$infected37max)*100,1))
result$infected52<-round(as.numeric(result$infected52)*100,1)
result$infected52min<-pmax(0,round(as.numeric(result$infected52min)*100,1))
result$infected52max<-pmin(100,round(as.numeric(result$infected52max)*100,1))
result$infected67<-round(as.numeric(result$infected67)*100,1)
result$infected67min<-pmax(0,round(as.numeric(result$infected67min)*100,1))
result$infected67max<-pmin(100,round(as.numeric(result$infected67max)*100,1))

result$infected97<-round(as.numeric(result$infected97)*100,1)
result$infected97min<-pmax(0,round(as.numeric(result$infected97min)*100,1))
result$infected97max<-pmin(100,round(as.numeric(result$infected97max)*100,1))

result$summary0<- paste(result$infected0," [",result$infected0min,"; ",result$infected0max,"]",sep="")
result$summary37<- paste(result$infected37," [",result$infected37min,"; ",result$infected37max,"]",sep="")
result$summary52<- paste(result$infected52," [",result$infected52min,"; ",result$infected52max,"]",sep="")
result$summary67<- paste(result$infected67," [",result$infected67min,"; ",result$infected67max,"]",sep="")
result$summary97<- paste(result$infected97," [",result$infected97min,"; ",result$infected97max,"]",sep="")

result$reg<-full_region_names(result$reg)
result<-result[order(result$reg),]
result<-result[,c("reg","summary0","summary37","summary52","summary67","summary97")]
xtable(result)







### PREDICTION LONG TERME ----

indivParams$fullname<-full_region_names(indivParams$id)
indivParams<-indivParams[order(indivParams$fullname),]

tauxICU=0.25
tauxD=0.05
nbICUplus<-1
load("./data/ICUcapacity_FR.RData")
result<-as.data.frame(matrix(NA,ncol=9,nrow=1))
names(result)<-c("K","location","I","A","E","topt","timeMAXICU","ICU","Death")
typecov="constant"
k<-1

for (K in c(3,5,10)){ #c(1,exp(-as.numeric(indivParams[1,"beta_mode"])),3,5,10,100)
    print(paste("K",K,sep=" "))
    for (i in 1:length(indivParams$id)){
        print(as.character(indivParams$id[i]))
        
        dureeconf=2000
        b<-as.numeric(indivParams[i,c("b1_mode")])
        r<-as.numeric(indivParams[i,c("r_sent")])
        dataregion<-data[which(data$IDname==as.character(indivParams[i,1])),]
        alpha<-alphafixed
        De<-Defixed
        Di<-Difixed
        Dq<-as.numeric(indivParams[i,c("Dq_mode")])
        Dh<-Dhfixed
        popSize<-dataregion$popsize[1]
        E0given<-as.numeric(indivParams[i,c("E0_mode")])
        A0given<-as.numeric(indivParams[i,c("pctA0_mode")])*E0given
        b2<-as.numeric(indivParams[i,c("betat1_mode")])
        b3<--log(K)-b2
        tconf<-timesconfinement[which(timesconfinement$IDname==as.character(indivParams[i,1])),1]
        lengthconf=dureeconf
        newdailyMove=0
        pred=TRUE
        bsd<-as.numeric(indivParams[i,"b1_sd"])
        Dqsd<-as.numeric(indivParams[i,"Dq_sd"])
        E0sd<-as.numeric(indivParams[i,"E0_sd"])
        A0sd<-as.numeric(indivParams[i,"A0_sd"])
        betasd<-as.numeric(popParams[which(popParams$parameter=="betat1_pop"),"se_sa"])
        beta2sd<-as.numeric(popParams[which(popParamsUP$parameter=="betat2_pop"),"se_sa"])
        
        solution<-getSolution( b,
                               r,
                               dataregion,
                               alpha,
                               De,
                               Di,
                               Dq,
                               Dh,
                               popSize,
                               E0given,
                               A0given,
                               b2,b3,
                               tconf,typecov,
                               lengthconf=dureeconf,
                               newdailyMove=newdailyMove,
                               pred=FALSE,bsd,Dqsd,E0sd,A0sd,betasd,beta2sd,TRUE,ncores=parallel::detectCores()-1)
        
        mai11<-tconf+55
        temp<-solution$solution$time[which((solution$solution$I+solution$solution$E+solution$solution$A)<3)]
        tempmin<-solution$solution$time[which((solution$solution$Imin+solution$solution$Emin+solution$solution$Amin)<3)]
        tempmax<-solution$solution$time[which((solution$solution$Imax+solution$solution$Emax+solution$solution$Amax)<3)]
        tempmax<-ifelse(is.finite(tempmax),tempmax,1000)
        topt<-paste(min(temp)," [",min(tempmin),"; ",min(tempmax),"]",sep="")
        
        E<-round(solution$solution$E[mai11],0)
        Emin<-round(solution$solution$Emin[mai11],0)
        Emax<-round(solution$solution$Emax[mai11],0)
        E11mai<-paste(E," [",Emin,"; ",Emax,"]",sep="")

        A<-round(solution$solution$A[mai11],0)
        Amin<-round(solution$solution$Amin[mai11],0)
        Amax<-round(solution$solution$Amax[mai11],0)
        A11mai<-paste(A," [",Amin,"; ",Amax,"]",sep="")
        
        I<-round(solution$solution$I[mai11],0)
        Imin<-round(solution$solution$Imin[mai11],0)
        Imax<-round(solution$solution$Imax[mai11],0)
        I11mai<-paste(I," [",Imin,"; ",Imax,"]",sep="")
        
        nbdeath<-paste(round(tauxD*solution$solution$R[1000],0)," [",round(tauxD*solution$solution$Rmin[1000],0),"; ",round(tauxD*solution$solution$Rmax[1000],0),"]",sep="")
        
        nblits<-nbICUplus*ICUcapacity_FR$nbICU_adult[which(ICUcapacity_FR$maille_code==as.character(solution$data$reg_id[1]))]
        
        
        overload<-solution$data$date[1]+as.numeric(min(solution$solution$time[which(tauxICU*solution$solution$H>=nblits)]))
        
        
        timemax<-which(solution$solution$H==max(solution$solution$H))
        maxICU<-paste(round((tauxICU*solution$solution$H[timemax])/nblits*100,0),"% [",round(max(tauxICU*solution$solution$Hmin[timemax])/nblits*100,0),"%; ",round(max(tauxICU*solution$solution$Hmax[timemax])/nblits*100,0),"%]",sep="")
        
          
        result[k,]<-c(K,as.character(indivParams$id[i]),I11mai,A11mai,E11mai,topt,as.character(overload),maxICU,nbdeath)
        
        print(result[k,])
        k<-k+1
    }
}
#result$nbdeath<-resultdeath[,"t720"]
#resultfois_ICU$location2<-full_region_names(resultfois_ICU$location)
xtable(result)

 resultfois_death2<-resultdeath
   resultfois_end2<-resultfinepidemics
   resultfois_hosto2<-resulthospmax
  resultfois_ICU2<-result

     # resultfois_death<-resultdeath
     #   resultfois_end<-resultfinepidemics
     #   resultfois_hosto<-resulthospmax
     #   resultfois_ICU<-result



### PERCENTAGE OF ASYMTOMATIC ----
mean(indivParams$r_sent)
quantile(indivParams$r_sent,0.025)
quantile(indivParams$r_sent,0.975)
(1-(2*mean(indivParams$r_sent))/(1-mean(indivParams$r_sent)))/5.1
(1-(2*quantile(indivParams$r_sent,0.05))/(1-quantile(indivParams$r_sent,0.05)))/5.1
(1-(2*quantile(indivParams$r_sent,0.95))/(1-quantile(indivParams$r_sent,0.95)))/5.1


### GLOBAL DESC. EPIDDEMICS WITHOUT INTERVENTION ----

pop<-read.table(file="MONOLIX/outputMonolix/Final_20200325/populationParameters.txt",header=TRUE,sep=",")
b<-pop[1,"value"]
r<-mean(indivParams[,c("r_sent")])
dataregion<-data[which(data$IDname==as.character(indivParams[i,1])),]
alpha<-alphafixed
De<-Defixed
Di<-Difixed
Dq<-pop[2,"value"]
Dh<-Dhfixed
popSize<-66990000
E0given<-pop[3,"value"]
A0given<-pop[4,"value"]
if(typecov=="constant"){
  b2<-pop[5,"value"]
}
tconf<-15

bsd<-pop[1,"se_sa"]
Dqsd<-pop[2,"se_sa"]
E0sd<-pop[3,"se_sa"]
A0sd<-pop[4,"se_sa"]
betasd<-pop[4,"se_sa"]

solution <- getSolution( b,
                         r,
                         dataregion,
                         alpha,
                         De,
                         Di,
                         Dq,
                         Dh,
                         popSize,
                         E0given,
                         A0given,b2,tconf,typecov,0,0,FALSE,bsd,Dqsd,E0sd,A0sd,betasd, CI=TRUE,ncores=parallel::detectCores()-1)

solution$data$date[1]+64
maxIA<-max(solution$solution$I+solution$solution$A+solution$solution$H)
timepeak<-solution$solution$time[which(solution$solution$I+solution$solution$A+solution$solution$H==maxIA)]
endepi<-solution$solution$time[which(solution$solution$I+solution$solution$A+solution$solution$H<3)]
solution$data$date[1]+457
mashospi<-max(solution$solution$H)
solution$solution$time[which(solution$solution$H==mashospi)]
solution$solution$Hmax[solution$solution$time==74]
solution$solution$Hmin[solution$solution$time==74]
solution$solution$R[800]/popSize
solution$solution$Rmin[800]/popSize
solution$solution$Rmax[800]/popSize

