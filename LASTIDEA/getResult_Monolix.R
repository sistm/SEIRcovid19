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

# 
# saveRDS(R0s_list, file = "./LASTIDEA/data/all_R0s_df_final20200416.rds")
# saveRDS(solutions_list, file = "./LASTIDEA/data/solutions_list20200416.rds")
# saveRDS(solutionsUPDATED_list, file = "./LASTIDEA/data/solutionsUPDATED_list20200416.rds")
# saveRDS(solutionsNOEFFECT_list, file = "./LASTIDEA/data/solutionsNOEFFECT_list20200416.rds")
# saveRDS(solutionsCOMBINED_list, file = "./LASTIDEA/data/solutionsCOMBINED_list20200416.rds")
# saveRDS(predictions_list, file = "./LASTIDEA/data/predictions20200416.rds")
# saveRDS(predictionsUPDATED_list, file = "./LASTIDEA/data/predictionsUPDATED20200416.rds")
# saveRDS(predictionsNOEFFECT_list, file = "./LASTIDEA/data/predictionsNOEFFECT20200416.rds")
# saveRDS(predictionsCOMBINED_list, file = "./LASTIDEA/data/predictionsCOMBINED20200416.rds")

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
          Di=Difixed, alpha=alphafixed, facet_scales = "free_y",nameprojectupdate)

### %INFECTED // ATTACK RATES
attack<-getAttackrates(predictionsCOMBINED,indivParams,inf=FALSE)
attackinfnothingdone<-getAttackrates(predictionsNOEFFECT,indivParams,inf=TRUE)
for (i in 1:length(indivParams$id)){
indivParams$attackinfnothingdone[i]<-attackinfnothingdone$summaryinf[which(attackinfnothingdone$reg==indivParams$id[i])]
}
attackinfnothingdoneFRANCE<-attackinfnothingdone[which(attackinfnothingdone$reg=="France"),"summaryinf"]


### Get Table Article ----
R0france<-getR0France(all_R0s_df,nameproject,nameprojectupdate,alpha,Di)
getindicators(indivParams,R0france,path,nameproject,nameprojectupdate,attackinfnothingdoneFRANCE)

R0france$I[which(R0france$time=="2020-04-15")]+0.033*R0france$R[which(R0france$time=="2020-04-15")]+R0france$H[which(R0france$time=="2020-04-15")]




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

### No intervention what if 
solutionNOEFFECT$solution
fo

maxepidemics<-max(predictionnoint$Iincident)
predictionnoint$time[which(predictionnoint$Iincident==maxepidemics)]
predictionnoint$time[which(predictionnoint$Iincident<5)]

### INDICATEURS -----

# 6=17/03
# 33=13/04
# 61 11/05
# 89 8/06
# 103 22/06



predictionsUSED <- predictionsNOEFFECT







### PREDICTION LONG TERME ----

indivParams$fullname<-full_region_names(indivParams$id)
indivParams<-indivParams[order(indivParams$fullname),]

tauxICU=0.25
tauxD=0.05
nbICUplus<-1
load("./data/ICUcapacity_FR.RData")
result<-as.data.frame(matrix(NA,ncol=27,nrow=1))
names(result)<-  c("K","location","I","A","E","topt","timeMAXICU","ICU","Death","toptval","toptvalmin","toptvalmax","Eval","Evalmin","Evalmax","Aval","Avalmin","Avalmax","Ival","Ivalmin","Ivalmax","dval","dvalmin","dvalmax","icuval","icuvalmin","icuvalmax")
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
        A0given<-10000
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
        temp<-min(solution$solution$time[which((solution$solution$I+solution$solution$E+solution$solution$A)<3)])
        tempmin<-min(solution$solution$time[which((solution$solution$Imin+solution$solution$Emin+solution$solution$Amin)<3)])
        tempmax<-solution$solution$time[which((solution$solution$Imax+solution$solution$Emax+solution$solution$Amax)<3)]
        tempmax<-min(ifelse(is.finite(tempmax),tempmax,1000))
        topt<-paste(temp," [",tempmin,"; ",tempmax,"]",sep="")
        
        
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
   

        death<-round(tauxD*solution$solution$R[1000],0)
        deathmin<-round(tauxD*solution$solution$Rmin[1000],0)
        deathmax<-round(tauxD*solution$solution$Rmax[1000],0)
        nbdeath<-paste(round(tauxD*solution$solution$R[1000],0)," [",round(tauxD*solution$solution$Rmin[1000],0),"; ",round(tauxD*solution$solution$Rmax[1000],0),"]",sep="")
        
        nblits<-nbICUplus*ICUcapacity_FR$nbICU_adult[which(ICUcapacity_FR$maille_code==as.character(solution$data$reg_id[1]))]
        
        
        overload<-solution$data$date[1]+as.numeric(min(solution$solution$time[which(tauxICU*solution$solution$H>=nblits)]))
        
        
        timemax<-which(solution$solution$H==max(solution$solution$H))
        
        ICUpct<-round((tauxICU*solution$solution$H[timemax])/nblits*100,0)
        ICUpctmin<-round(max(tauxICU*solution$solution$Hmin[timemax])/nblits*100,0)
        ICUpctmax<-round(max(tauxICU*solution$solution$Hmax[timemax])/nblits*100,0)
        maxICU<-paste(round((tauxICU*solution$solution$H[timemax])/nblits*100,0),"% [",round(max(tauxICU*solution$solution$Hmin[timemax])/nblits*100,0),"%; ",round(max(tauxICU*solution$solution$Hmax[timemax])/nblits*100,0),"%]",sep="")
        
        
        result[k,]<-c(K,as.character(indivParams$id[i]),I11mai,A11mai,E11mai,topt,as.character(overload),maxICU,nbdeath,temp,tempmin,tempmax,E,Emin,Emax,A,Amin,Amax,I,Imin,Imax,death,deathmin,deathmax,ICUpct,ICUpctmin,ICUpctmax)
        
        print(result[k,])
        k<-k+1
    }
}
result_save<-result
#saveRDS(result_save, file = "./LASTIDEA/data/result_lockdown20200416.rds")
result_save$location<-full_region_names(result_save$location)
xtable(result_save[,c("location","I","A","E")],include.rownames=FALSE)

result_save$R<-as.numeric(result_save$dval)/0.05
result_save$Rmin<-as.numeric(result_save$dvalmin)/0.05
result_save$Rmax<-as.numeric(result_save$dvalmax)/0.05
for (i in 1:length(result_save$location)){
  result_save$popsize[i]<-indivParams$popsize[which(full_region_names(indivParams$id)==result_save$location[i])]
}
result_save$attack<-format(round(result_save$R/result_save$popsize*100,1),nsmall = 1)
result_save$attackmin<-format(round(result_save$Rmin/result_save$popsize*100,1),nsmall = 1)
result_save$attackmax<-format(round(result_save$Rmax/result_save$popsize*100,1),nsmall = 1)
result_save$attack<-paste(result_save$attack,"% [",result_save$attackmin,"%; ",result_save$attackmax,"%]",sep="")

xtable(result_save[,c("location","topt","timeMAXICU","ICU","Death","attack")],include.rownames=FALSE)
strsplit(result_save$Death, split=c(" ",";"))

?xtable

result_save<-result
result$location<-full_region_names(result$location)
result<-result[order(result$K, result$location),]
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
pop<-read.table(file="LASTIDEA/outputMonolix/Final_20200325/populationParameters.txt",header=TRUE,sep=",")
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
A0given<-1000
if(typecov=="constant"){
  b2<-pop[4,"value"]
}
tconf<-1000

bsd<-sqrt(pop[1,"se_sa"]**2+pop[5,"value"]**2)
Dqsd<-sqrt(pop[2,"se_sa"]**2+pop[6,"value"]**2)
E0sd<-sqrt(pop[3,"se_sa"]**2++pop[7,"value"]**2)
A0sd<-0
betasd<-sqrt(pop[4,"se_sa"]**2)

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
                         A0given,0,0,tconf,typecov,0,0,FALSE,bsd,Dqsd,E0sd,A0sd,betasd,0, CI=TRUE,ncores=parallel::detectCores()-1)


## Timing peak
maxIA<-max(solution$solution$E+solution$solution$I+solution$solution$A+solution$solution$H)
timepeak<-solution$solution$time[which(solution$solution$E+solution$solution$I+solution$solution$A+solution$solution$H==maxIA)]
solution$data$date[1]+timepeak

endepi<-min(solution$solution$time[which(solution$solution$E+solution$solution$I+solution$solution$A+solution$solution$H<4)])
solution$data$date[1]+endepi

mashospi<-max(solution$solution$H)
timemaxhospi<-solution$solution$time[which(solution$solution$H==mashospi)]
solution$solution$Hmax[solution$solution$time==timemaxhospi]
solution$solution$Hmin[solution$solution$time==timemaxhospi]

exp(--0.2702769)
exp(-(-0.2702769-1.96*0.01533011))
exp(-(-0.2702769+1.96*0.01533011))

exp(--0.2702769--1.0195847)
exp(-(-0.2702769-1.0195847-1.96*sqrt(0.01533011**2+0.0164773**2)))
exp(-(-0.2702769-1.0195847+1.96*sqrt(0.01533011**2+0.0164773**2)))


###################
solutionsREBOUND_list <- list()
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
  if(typecov=="constant"){
    b3UP<-as.numeric(indivParamsUP[i,"betat2_mode"])
  }
  beta2sdUP<-as.numeric(popParamsUP[which(popParamsUP$parameter=="betat2_pop"),"se_sa"])
  
  
  solutionREBOUND <- getSolution( b,
                                   r,
                                   dataregion,
                                   alpha,
                                   De,
                                   Di,
                                   Dq,
                                   Dh,
                                   popSize,
                                   E0given,
                                   A0given,b2UP,b3UP,tconf,typecov,tconf+55,0,FALSE,bsd,Dqsd,E0sd,A0sd,betasd,beta2sdUP, CI=TRUE,ncores=parallel::detectCores()-1)

  par(mfrow=c(2,3))
  plot(solutionREBOUND$solution$time,(solutionREBOUND$solution$S),xlim=c(0,400))
  plot(solutionREBOUND$solution$time,(solutionREBOUND$solution$E),xlim=c(0,400))
  plot(solutionREBOUND$solution$time,(solutionREBOUND$solution$I),xlim=c(0,400))
  plot(solutionREBOUND$solution$time,(solutionREBOUND$solution$R),xlim=c(0,400))
  plot(solutionREBOUND$solution$time,(solutionREBOUND$solution$A),xlim=c(0,400))
  plot(solutionREBOUND$solution$time,(solutionREBOUND$solution$H),xlim=c(0,400))
  solutionsREBOUND_list[[i]] <- solutionREBOUND$solution
  solutionsREBOUND_list[[i]]$reg<-as.character(indivParams$id[i])
  
}
solutionsREBOUND_all <- do.call(rbind.data.frame, solutionsREBOUND_list)
p1 <- ggplot(solutionsREBOUND_all) + geom_line(aes(y=S, x=time, colour = reg))+
  geom_ribbon(aes(ymin=Smin, ymax=Smax, x=time, fill = reg), alpha = 0.3)+xlim(c(0,300))
p2 <- ggplot(solutionsREBOUND_all) + geom_line(aes(y=E, x=time, colour = reg))+
  geom_ribbon(aes(ymin=Emin, ymax=Emax, x=time, fill = reg), alpha = 0.3)+xlim(c(0,300))
p3 <- ggplot(solutionsREBOUND_all) + geom_line(aes(y=I, x=time, colour = reg))+
  geom_ribbon(aes(ymin=Imin, ymax=Imax, x=time, fill = reg), alpha = 0.3)+xlim(c(0,300))
p4 <- ggplot(solutionsREBOUND_all) + geom_line(aes(y=R, x=time, colour = reg))+
  geom_ribbon(aes(ymin=Rmin, ymax=Rmax, x=time, fill = reg), alpha = 0.3)+xlim(c(0,300))
p5 <- ggplot(solutionsREBOUND_all) + geom_line(aes(y=A, x=time, colour = reg))+
  geom_ribbon(aes(ymin=Amin, ymax=Amax, x=time, fill = reg), alpha = 0.3)+xlim(c(0,300))



p6 <- ggplot(solutionsREBOUND_all) + geom_line(aes(y=H, x=time, colour = reg))+
  geom_ribbon(aes(ymin=Hmin, ymax=Hmax, x=time, fill = reg), alpha = 0.3)+xlim(c(0,300))
grid.arrange(p1,p2,p3,p4,p5,p6,ncol=3,nrow=2)

for (i in 1:length(solutionsREBOUND_all$popsize)){
solutionsREBOUND_all$popsize[i]<-indivParams$popsize[which(indivParams$id==solutionsREBOUND_all$reg[i])]
}


p7 <- ggplot(solutionsREBOUND_all) + geom_line(aes(y=(H+I+E+A)/popsize, x=time, colour = reg))+
  geom_ribbon(aes(ymin=(Hmin+Imin+Emin+Amin)/popsize, ymax=(Hmax+Imax+Emax+Amax)/popsize, x=time, fill = reg), alpha = 0.3)+xlim(c(0,300))
p7

p8 <- ggplot(solutionsREBOUND_all) + geom_line(aes(y=(R)/popsize, x=time, colour = reg))+
  geom_ribbon(aes(ymin=(Rmin)/popsize, ymax=(Rmax)/popsize, x=time, fill = reg), alpha = 0.3)+xlim(c(0,300))
p8
