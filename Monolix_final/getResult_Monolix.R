library(xtable)
library(ggplot2)
library(lubridate)
library(gridExtra)
library(forcats)
library(dplyr)
source("./Monolix_final/routineResults.R")


path<-"./Monolix_final/"
nameproject<-"final_nomove"
dataname<-"data_monolix_20200403.txt"
codename<- "monolix_Estimation_2periode_cov_final.txt"

alphafixed<-1.5
Defixed<-5.1
Difixed<-2.3
Dhfixed<-30

## Get Individual Parameters & data
indivParams = read.table(paste(path,"/outputMonolix/",nameproject,"/IndividualParameters/estimatedIndividualParameters.txt",sep=""),header=TRUE,sep=",")
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
    b2<-as.numeric(indivParams[i,"b1_mode"])*exp(as.numeric(indivParams[i,"beta_mode"]))
    tconf<-timesconfinement[which(timesconfinement$IDname==as.character(indivParams[i,1])),1]

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
                             A0given,b2,tconf)
    solution$solution$date <- seq.Date(from = dataregion$date[1], by=1, length.out = nrow(solution$solution))
    solution$solution$reg <- as.character(indivParams$id[i])
    solutions_list[[i]] <- solution
    getPlot(solution,nameproject,indivParams[i,])

    res<-getR0(solution,indivParams[i,])
    R0s_list[[i]] <-  res

    getPlotR0(res,nameproject,indivParams[i,])

    indivParams[i,c("R0","R0min","R0max")]<-res[which(res$time==(tconf-1)),c("R0","R0ICmin","R0ICmax")]
    indivParams[i,c("R0conf","R0minconf","R0maxconf")]<-res[which(res$time==100),c("R0","R0ICmin","R0ICmax")]
    indivParams[i,"timestart"]<-as.character(dataregion$date[which((dataregion$day==0)&(dataregion$obs_id==1))])
    indivParams[i,"Icumul"]<-sum(dataregion$obs[which((dataregion$obs_id==1))])
    indivParams[i,"Hcumul"]<-sum(dataregion$obs[which((dataregion$obs_id==2))])

    res <- getIHD(solution,indivParams[i,])
    predictions_list[[i]] <- res
    message("Done\n")
}

### Get Figure Article

getPlotSolutionAll(solutions_list, nameproject = nameproject)

all_R0s_df <- do.call(rbind.data.frame, R0s_list)
getPlotR0all(all_R0s_df, nameproject = nameproject)


### Get Table Article
getindicators(indivParams)

##################
### PREDICTION COURT TERME
#################
###### Get predictions 10 jours
getpredictionShortterm(predictions,nameproject)

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
    k<-k+1
}
sizeFR<-popreg$population[which(popreg$maille_code=="FRA")]

names(result)
result$reg<-as.character(result$reg)
result<-rbind(result,c("France",sum(as.numeric(predictions$immunised[which(predictions$i==7)]))/sizeFR,sum(as.numeric(predictions$infected[which(predictions$i==7)]))/sizeFR,sum(as.numeric(predictions$immunised[which(predictions$i==14)]))/sizeFR,sum(as.numeric(predictions$infected[which(predictions$i==14)]))/sizeFR,sum(as.numeric(predictions$immunised[which(predictions$i==28)]))/sizeFR,sum(as.numeric(predictions$infected[which(predictions$i==28)]))/sizeFR,sum(as.numeric(predictions$immunised[which(predictions$i==97)]))/sizeFR,sum(as.numeric(predictions$infected[which(predictions$i==97)]))/sizeFR))
result$immunised0<-round(as.numeric(result$immunised0)*100,1)
result$infected0<-round(as.numeric(result$infected0)*100,1)
result$immunised8<-round(as.numeric(result$immunised8)*100,1)
result$infected8<-round(as.numeric(result$infected8)*100,1)
result$immunised21<-round(as.numeric(result$immunised21)*100,1)
result$infected21<-round(as.numeric(result$infected21)*100,1)
result$immunised90<-round(as.numeric(result$immunised90)*100,1)
result$infected90<-round(as.numeric(result$infected90)*100,1)

result<-result[,c("reg","infected0","infected8","infected21","infected90","immunised0","immunised8","immunised21","immunised90")]
xtable(result)
############


##################
### PREDICTION LONG TERME
#################
tauxICU=0.056
tauxD=0.05
nbICUplus<-3
load("./data/ICUcapacity_FR.RData")
result<-as.data.frame(matrix(NA,ncol=9,nrow=36))
names(result)<-c("K","location","t30","t45","t60","t90","t180","t340","topt")
resultdeath<-as.data.frame(matrix(NA,ncol=8,nrow=36))
names(resultdeath)<-c("K","location","t30","t45","t60","t90","t180","t340")
resulthospmax<-as.data.frame(matrix(NA,ncol=8,nrow=36))
names(resulthospmax)<-c("K","location","t30","t45","t60","t90","t180","t340")
resultfinepidemics<-as.data.frame(matrix(NA,ncol=8,nrow=36))
names(resultfinepidemics)<-c("K","location","t30","t45","t60","t90","t180","t340")
k<-1
for (K in c(3,10,100)){ #c(1,exp(-as.numeric(indivParams[1,"beta_mode"])),3,5,10,100)
    print(paste("K",K,sep=" "))
    for (i in 1:length(indivParams$id)){
        print(as.character(indivParams$id[i]))
        toprint<-c()
        nbhospmax<-c()
        datefin<-c()
        FIRST<-TRUE
        topt<-NA
        nbdeath<-c()
        timings<-c(30,45,60,90,180,365)
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
            b2<-as.numeric(indivParams[i,"b1_mode"])/K
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
                                tconf,
                                lengthconf=dureeconf,
                                newdailyMove=newdailyMove,
                                pred=pred)
                                
         
         nblits<-nbICUplus*ICUcapacity_FR$nbICU_adult[which(ICUcapacity_FR$maille_code==as.character(solution$data$reg_id[1]))]
          # par(mfrow=c(2,3))
          # xlim<-c(0,200)
          # plot(solution$solution$time,solution$solution$S,xlim=xlim)
          # abline(v=tconf+lengthconf)
          # plot(solution$solution$time,solution$solution$E,xlim=xlim)
          # abline(v=tconf+lengthconf)
          # plot(solution$solution$time,solution$solution$A,xlim=xlim)
          # abline(v=tconf+lengthconf)
          # plot(solution$solution$time,solution$solution$I,xlim=xlim)
          # abline(v=tconf+lengthconf)
          # plot(solution$solution$time,solution$solution$H,xlim=xlim)
          # abline(v=tconf+lengthconf)
          # plot(solution$solution$time,solution$solution$R,xlim=xlim)
          # abline(v=tconf+lengthconf)
         nbdeath<-c(nbdeath,tauxD*solution$solution$R[1000])
         nbhospmax<-c(nbhospmax,max(solution$solution$H))
         datefin<-c(datefin,as.character(dataregion$date[1]+min(solution$solution$time[which(solution$solution$I==0)])))
         
         timeout<-min(solution$solution$time[which(solution$solution$H*tauxICU>nblits)])   
         dateout<-dataregion$date[1]+timeout
         # print(dateout)
         if(dureeconf%in%timings){
             toprint<-c(toprint,as.character(dateout))
         }
        }
        
        if(length(which(is.na(toprint)))!=0){
        for (dureeconf in seq(min(0,timings[which(is.na(toprint))[1]-1],na.rm = TRUE),max(timings[which(is.na(toprint))[1]],365,na.rm = TRUE),by=1)){
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
            b2<-as.numeric(indivParams[i,"b1_mode"])/K
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
                                       A0given,tconf,lengthconf,newdailyMove,pred)

                nblits<-ICUcapacity_FR$nbICU_adult[which(ICUcapacity_FR$maille_code==as.character(solution$data$reg_id[1]))]
                # par(mfrow=c(2,2))
                # plot(solution$solution$time,solution$solution$E)
                # plot(solution$solution$time,solution$solution$A)
                # plot(solution$solution$time,solution$solution$I,ylim=c(0,100))
                # plot(solution$solution$time,solution$solution$H)


                timeout<-min(solution$solution$time[which(solution$solution$H*tauxICU>nblits)])
                dateout<-dataregion$date[1]+timeout
                # print(dateout)
                if((FIRST)&!is.finite(timeout)){
                    topt<-dureeconf-1
                    FIRST<-FALSE
                    break
                }
            }
        }
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

xtable(result)

     
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
quantile(indivParams$r_sent,0.05)
quantile(indivParams$r_sent,0.95)
(1-(2*mean(indivParams$r_sent))/(1-mean(indivParams$r_sent)))/5.1
(1-(2*quantile(indivParams$r_sent,0.05))/(1-quantile(indivParams$r_sent,0.05)))/5.1
(1-(2*quantile(indivParams$r_sent,0.95))/(1-quantile(indivParams$r_sent,0.95)))/5.1

