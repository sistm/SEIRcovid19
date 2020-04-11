rm(list = ls())
library(xtable)
library(ggplot2)
library(lubridate)
library(gridExtra)
library(forcats)
library(dplyr)
source("./MONOLIX/routineResults.R")


path<-"./MONOLIX/"
nameproject<-"Final_20200325/"
dataname<-"data_monolix_20200325.txt"
nameprojectupdate<-"Final_Update20200406/"

typecov<-"constant"
alphafixed<-1.5
Defixed<-5.1
Difixed<-2.3
Dhfixed<-30
timings<-15

## Get Individual Parameters & data
indivParams = read.table(paste(path,"/outputMonolix/",nameproject,"/IndividualParameters/estimatedIndividualParameters.txt",sep=""),header=TRUE,sep=",")
indivParamsUP = read.table(paste(path,"/outputMonolix/",nameprojectupdate,"/IndividualParameters/estimatedIndividualParameters.txt",sep=""),header=TRUE,sep=",")
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
data[which(data$day==0),c("reg_id","IDname")]
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
predictionsNOEFFECT_list<- list()
predictionsUPDATED_list<- list()
for (i in 1:length(indivParams$id)){
    message(as.character(indivParams$id[i]), "...")
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
    A0given<-as.numeric(indivParams[i,c("A0_mode")])
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

    bsd<-as.numeric(indivParams[i,"b1_sd"])
    Dqsd<-as.numeric(indivParams[i,"Dq_sd"])
    E0sd<-as.numeric(indivParams[i,"E0_sd"])
    A0sd<-as.numeric(indivParams[i,"A0_sd"])
    betasd<-as.numeric(indivParams[i,"betat1_sd"])
    
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
                             A0given,b2,tconf,typecov,1000,0,FALSE,bsd,Dqsd,E0sd,A0sd,betasd)
    
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
                                     A0given,b,tconf,typecov,0,0,FALSE,bsd,Dqsd,E0sd,A0sd,betasd)
    
    bUP<-as.numeric(indivParamsUP[i,c("b1_mode")])
    DqUP<-as.numeric(indivParamsUP[i,c("Dq_mode")])
    E0givenUP<-as.numeric(indivParamsUP[i,c("E0_mode")])
    A0givenUP<-as.numeric(indivParamsUP[i,c("A0_mode")])
    if(typecov=="constant"){
      b2UP<-as.numeric(indivParamsUP[i,"betat1_mode"])
    }
    bsd<-as.numeric(indivParamsUP[i,"b1_sd"])
    Dqsd<-as.numeric(indivParamsUP[i,"Dq_sd"])
    E0sd<-as.numeric(indivParamsUP[i,"E0_sd"])
    A0sd<-as.numeric(indivParamsUP[i,"A0_sd"])
    betasd<-as.numeric(indivParamsUP[i,"betat1_sd"])
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
                             A0givenUP,b2UP,tconf,typecov,1000,0,FALSE,bsd,Dqsd,E0sd,A0sd,betasd)
 
    
    
    solution$solution$date <- seq.Date(from = dataregion$date[1], by=1, length.out = nrow(solution$solution))
    solutionUPDATED$solution$date <- seq.Date(from = dataregion$date[1], by=1, length.out = nrow(solution$solution))
    solutionNOEFFECT$solution$date <- seq.Date(from = dataregion$date[1], by=1, length.out = nrow(solution$solution))
    solutionUPDATED$solution$reg <- as.character(indivParams$id[i])
    solution$solution$reg <- as.character(indivParams$id[i])
    solutionNOEFFECT$solution$reg <- as.character(indivParams$id[i])
    solutions_list[[i]] <- solution
    solutionsNOEFFECT_list[[i]] <- solutionNOEFFECT
    solutionsUPDATED_list[[i]] <- solutionUPDATED
    
    getPlot(solution,nameproject,indivParams[i,])
    
    res<-getR0(solution,indivParams[i,],typecov,timings,indivParamsUP[i,],solutionUPDATED)
    R0s_list[[i]] <-  res

    getPlotR0(res,nameproject,indivParams[i,])

    indivParams[i,c("R0","R0min","R0max")]<-res[which(res$time==(tconf-1)),c("R0","R0ICmin","R0ICmax")]
    indivParams[i,c("R0conf","R0minconf","R0maxconf")]<-res[which(res$time==tconf+1),c("R0","R0ICmin","R0ICmax")]
    indivParams[i,c("R0conf2","R0minconf2","R0maxconf2")]<-res[which(res$time==tconf+30),c("R0","R0ICmin","R0ICmax")]
    indivParams[i,"timestart"]<-as.character(dataregion$date[which((dataregion$day==0)&(dataregion$obs_id==1))])
    indivParams[i,"Icumul"]<-sum(dataregion$obs[which((dataregion$obs_id==1))])
    indivParams[i,"Hcumul"]<-sum(dataregion$obs[which((dataregion$obs_id==2))])
    res <- getIHD(solution,indivParams[i,])
    resNOEFFECT <- getIHD(solutionNOEFFECT,indivParams[i,])
    resUPDATED <- getIHD(solutionUPDATED,indivParams[i,])
    
    predictionsNOEFFECT_list[[i]] <- resNOEFFECT
    predictionsUPDATED_list[[i]] <- resUPDATED
    predictions_list[[i]] <- res
    message("Done\n")
}

### Get Figure Article
getPlotSolutionAll(solutions_list, nameproject = nameproject)

all_R0s_df <- do.call(rbind.data.frame, R0s_list)
getPlotR0all(all_R0s_df, nameproject = nameproject,path,timings,typecov)#,facet_scales = "fixed")


### Get Table Article
getindicators(indivParams)

##################
### PREDICTION COURT TERME
#################
###### Get predictions 10 jours
predictions <- do.call(rbind.data.frame, predictions_list)
predictionsUPDATED <- do.call(rbind.data.frame, predictionsUPDATED_list)
predictionsNOEFFECT <- do.call(rbind.data.frame, predictionsNOEFFECT_list)
getpredictionShortterm(predictions,predictionsUPDATED,predictionsNOEFFECT,nameproject)

##################
### INDICATEURS
#################
result<-as.data.frame(indivParams$id)
names(result)<-"reg"
k<-1
for (region in unique(result$reg)){
    result$immunised0[k]<-as.numeric(predictions$immunised[which((predictions$reg==region)&(predictions$i==7))])/popreg$population[which(popreg$idnames==region)]
    result$infected0[k]<-as.numeric(predictions$infected[which((predictions$reg==region)&(predictions$i==7))])/popreg$population[which(popreg$idnames==region)]
    result$immunised8[k]<-as.numeric(predictions$immunised[which((predictions$reg==region)&(predictions$i==14))])/popreg$population[which(popreg$idnames==region)]
    result$infected8[k]<-as.numeric(predictions$infected[which((predictions$reg==region)&(predictions$i==14))])/popreg$population[which(popreg$idnames==region)]
    result$immunised21[k]<-as.numeric(predictions$immunised[which((predictions$reg==region)&(predictions$i==28))])/popreg$population[which(popreg$idnames==region)]
    result$infected21[k]<-as.numeric(predictions$infected[which((predictions$reg==region)&(predictions$i==28))])/popreg$population[which(popreg$idnames==region)]
    result$immunised90[k]<-as.numeric(predictions$immunised[which((predictions$reg==region)&(predictions$i==97))])/popreg$population[which(popreg$idnames==region)]
    result$infected90[k]<-as.numeric(predictions$infected[which((predictions$reg==region)&(predictions$i==97))])/popreg$population[which(popreg$idnames==region)]
    result$immunised1000[k]<-as.numeric(predictions$immunised[which((predictions$reg==region)&(predictions$i==700))])/popreg$population[which(popreg$idnames==region)]
    result$infected1000[k]<-as.numeric(predictions$infected[which((predictions$reg==region)&(predictions$i==700))])/popreg$population[which(popreg$idnames==region)]
    k<-k+1
}
sizeFR<-popreg$population[which(popreg$maille_code=="FRA")]



names(result)
result$reg<-as.character(result$reg)
result<-rbind(result,c("France",sum(as.numeric(predictions$immunised[which(predictions$i==7)]))/sizeFR,sum(as.numeric(predictions$infected[which(predictions$i==7)]))/sizeFR,sum(as.numeric(predictions$immunised[which(predictions$i==14)]))/sizeFR,sum(as.numeric(predictions$infected[which(predictions$i==14)]))/sizeFR,sum(as.numeric(predictions$immunised[which(predictions$i==28)]))/sizeFR,sum(as.numeric(predictions$infected[which(predictions$i==28)]))/sizeFR,sum(as.numeric(predictions$immunised[which(predictions$i==97)]))/sizeFR,sum(as.numeric(predictions$infected[which(predictions$i==97)]))/sizeFR,sum(as.numeric(predictions$immunised[which(predictions$i==700)]))/sizeFR,sum(as.numeric(predictions$infected[which(predictions$i==700)]))/sizeFR))
result$immunised0<-round(as.numeric(result$immunised0)*100,1)
result$infected0<-round(as.numeric(result$infected0)*100,1)
result$immunised8<-round(as.numeric(result$immunised8)*100,1)
result$infected8<-round(as.numeric(result$infected8)*100,1)
result$immunised21<-round(as.numeric(result$immunised21)*100,1)
result$infected21<-round(as.numeric(result$infected21)*100,1)
result$immunised90<-round(as.numeric(result$immunised90)*100,1)
result$infected90<-round(as.numeric(result$infected90)*100,1)
result$immunised1000<-round(as.numeric(result$immunised1000)*100,1)
result$infected1000<-round(as.numeric(result$infected1000)*100,1)
result<-result[,c("reg","infected0","infected8","infected21","infected90","immunised0","immunised8","immunised21","immunised90")]
xtable(result)
############


##################
### PREDICTION LONG TERME
#################
tauxICU=0.25
tauxD=0.01
nbICUplus<-2
load("./data/ICUcapacity_FR.RData")
result<-as.data.frame(matrix(NA,ncol=10,nrow=1))
names(result)<-c("K","location","t30","t45","t60","t90","t180","t340","t720","topt")
resultdeath<-as.data.frame(matrix(NA,ncol=9,nrow=1))
names(resultdeath)<-c("K","location","t30","t45","t60","t90","t180","t340","t720")
resulthospmax<-as.data.frame(matrix(NA,ncol=9,nrow=1))
names(resulthospmax)<-c("K","location","t30","t45","t60","t90","t180","t340","t720")
resultfinepidemics<-as.data.frame(matrix(NA,ncol=9,nrow=1))
names(resultfinepidemics)<-c("K","location","t30","t45","t60","t90","t180","t340","t720")
typecov="constant"
k<-1
for (K in c(1.5,2,3,5,7,10)){ #c(1,exp(-as.numeric(indivParams[1,"beta_mode"])),3,5,10,100)
    print(paste("K",K,sep=" "))
    for (i in 1:length(indivParams$id)){
        print(as.character(indivParams$id[i]))
        toprint<-c()
        nbhospmax<-c()
        datefin<-c()
        FIRST<-TRUE
        topt<-NA
        nbdeath<-c()
        timings<-c(30,45,60,90,180,365,730)
        for (dureeconf in timings){
            # print(paste("dureeconf",dureeconf,sep=" "))
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
            A0given<-as.numeric(indivParams[i,c("A0_mode")])
            b2<--log(K)
            tconf<-timesconfinement[which(timesconfinement$IDname==as.character(indivParams[i,1])),1]
            lengthconf=dureeconf
            newdailyMove=0
            pred=TRUE
            
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
                                b2,
                                tconf,typecov,
                                lengthconf=dureeconf,
                                newdailyMove=newdailyMove,
                                pred=pred)
                                
         nblits<-nbICUplus*ICUcapacity_FR$nbICU_adult[which(ICUcapacity_FR$maille_code==as.character(solution$data$reg_id[1]))]
         
         nbdeath<-c(nbdeath,tauxD*solution$solution$R[1000])
         nbhospmax<-c(nbhospmax,max(solution$solution$H))
         datefin<-c(datefin,as.character(dataregion$date[1]+min(solution$solution$time[which(solution$solution$I==0)])))
         
         timeout<-min(solution$solution$time[which(solution$solution$H*tauxICU>nblits)])   
         dateout<-dataregion$date[1]+timeout
         # print(dateout)
             toprint<-c(toprint,as.character(dateout))
        
        topt<-min(solution$solution$time[which((solution$solution$I+solution$solution$A)==0)])
    }
          
        # if(length(which(is.na(toprint)))!=0){
        # for (dureeconf in seq(min(0,timings[which(is.na(toprint))[1]-1],na.rm = TRUE),max(timings[which(is.na(toprint))[1]],365,na.rm = TRUE),by=1)){
        #     # print(paste("dureeconf",dureeconf,sep=" "))
        #     b<-as.numeric(indivParams[i,c("b1_mode")])
        #     r<-as.numeric(indivParams[i,c("r_sent")])
        #     dataregion<-data[which(data$IDname==as.character(indivParams[i,1])),]
        #     alpha<-alphafixed
        #     De<-Defixed
        #     Di<-Difixed
        #     Dq<-as.numeric(indivParams[i,c("Dq_mode")])
        #     Dh<-Dhfixed
        #     popSize<-dataregion$popsize[1]
        #     E0given<-as.numeric(indivParams[i,c("E0_mode")])
        #     A0given<-as.numeric(indivParams[i,c("A0_mode")])
        #     b2<--log(K)
        #     tconf<-timesconfinement[which(timesconfinement$IDname==as.character(indivParams[i,1])),1]
        #     lengthconf=dureeconf
        #     newdailyMove=0
        #     pred=TRUE
        # 
        #         solution<-getSolution( b,
        #                                r,
        #                                dataregion,
        #                                alpha,
        #                                De,
        #                                Di,
        #                                Dq,
        #                                Dh,
        #                                popSize,
        #                                E0given,
        #                                A0given,tconf,typecov,lengthconf,newdailyMove,pred)
        # 
        #         nblits<-ICUcapacity_FR$nbICU_adult[which(ICUcapacity_FR$maille_code==as.character(solution$data$reg_id[1]))]
        #         # par(mfrow=c(2,2))
        #         # plot(solution$solution$time,solution$solution$E)
        #         # plot(solution$solution$time,solution$solution$A)
        #         # plot(solution$solution$time,solution$solution$I,ylim=c(0,100))
        #         # plot(solution$solution$time,solution$solution$H)
        # 
        # 
        #         timeout<-min(solution$solution$time[which(solution$solution$H*tauxICU>nblits)])
        #         dateout<-dataregion$date[1]+timeout
        #         # print(dateout)
        #         if((FIRST)&!is.finite(timeout)){
        #             topt<-dureeconf-1
        #             FIRST<-FALSE
        #             break
        #         }
        #     }
        # }
        result[k,]<-c(K,as.character(indivParams$id[i]),toprint,topt)
        resultdeath[k,]<-c(K,as.character(indivParams$id[i]),round(nbdeath,0))
        resulthospmax[k,]<-c(K,as.character(indivParams$id[i]),round(nbhospmax,0))
        resultfinepidemics[k,]<-c(K,as.character(indivParams$id[i]),datefin)
        
        print(result[k,])
        print(resultdeath[k,])
        print(resulthospmax[k,])
        print(resultfinepidemics[k,])
        k<-k+1
    }
}
result$location<-full_region_names(result$location)
xtable(result[,c("K","location","t45","t60","t90","t180","t340","topt" )])

     
     # result2fois_death<-resultdeath 
     #   result2fois_end<-resultdeath 
     #   result2fois_hosto<-resulthospmax 
     #   result2fois_ICU<-result 
  
     result3fois_death<-resultdeath
       result3fois_end<-resultdeath
       result3fois_hosto<-resulthospmax
       result3fois_ICU<-result

#############################
### PERCENTAGE OF ASYMTOMATIC
#############################
mean(indivParams$r_sent)
quantile(indivParams$r_sent,0.025)
quantile(indivParams$r_sent,0.975)
(1-(2*mean(indivParams$r_sent))/(1-mean(indivParams$r_sent)))/5.1
(1-(2*quantile(indivParams$r_sent,0.05))/(1-quantile(indivParams$r_sent,0.05)))/5.1
(1-(2*quantile(indivParams$r_sent,0.95))/(1-quantile(indivParams$r_sent,0.95)))/5.1

