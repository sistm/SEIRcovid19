library(xtable)
library(ggplot2)
library(lubridate)
library(gridExtra)
source("./Monolix_final/routineResults.R")


path<-"./Monolix_final/"
nameproject<-"final"
dataname<-"data_monolix_20200403.txt"
codename<- "monolix_Estimation_2periode_cov.txt"

alphafixed<-1.5
Defixed<-5.2
Difixed<-2.3
Dhfixed<-30


indivParams = read.table(paste(path,"/outputMonolix/",nameproject,"/IndividualParameters/estimatedIndividualParameters.txt",sep=""),header=TRUE,sep=",")
dir.create(paste(path,"outputMonolix/",nameproject,"/graphics",sep=""))
data<-read.table(paste(path,dataname,sep=""),sep="\t",header=TRUE)
for (i in 1:length(indivParams$id)){
indivParams$r_sent[i]<-data$ascertainement[which((data$IDname==indivParams$id[i])&(data$day==0)&(data$obs_id==1))]
}
data$date<-lubridate::as_date(as.character(data$date))
timesconfinement<-data[which((data$date=="2020-03-17")&(data$obs_id==1)),c("day","IDname")]

load("./data/popreg.RData")
popreg$idnames<-c("AURA","BFC","Bretagne","Centre","Corse","GrandEst","HDF","IDF","Normandie","NAquitaine","Occitanie","PaysLoire","PACA","G1","G2","G3","G4","G5","France")
for(i in 1:length(indivParams$id)){
indivParams$popsize[i]<-popreg$population[which(popreg$idnames==indivParams$id[i])]
}

 for (i in 1:length(indivParams$id)){
    print(as.character(indivParams$id[i]))
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
          A0given,b2,tconf)
    getPlot(solution,nameproject,indivParams[i,])
    res<-getR0(solution,indivParams[i,])
    if(i==1){
        R0table<-res
    }else{
        R0table<-rbind(R0table,res)
    }
    
    getPlotR0(res,nameproject,indivParams[i,])
    
    indivParams[i,c("R0","R0min","R0max")]<-res[which(res$time==(tconf-1)),c("R0","R0ICmin","R0ICmax")]
    indivParams[i,c("R0conf","R0minconf","R0maxconf")]<-res[which(res$time==100),c("R0","R0ICmin","R0ICmax")]
    indivParams[i,"timestart"]<-as.character(dataregion$date[which((dataregion$day==0)&(dataregion$obs_id==1))])
    indivParams[i,"Icumul"]<-sum(dataregion$obs[which((dataregion$obs_id==1))])
    indivParams[i,"Hcumul"]<-sum(dataregion$obs[which((dataregion$obs_id==2))])  
    
    res<-getIHD(solution,indivParams[i,])
    if(i==1){
        predictions<-res
    }else{
        predictions<-rbind(predictions,res)
    }
}
### Get Table Article
getindicators(indivParams)

### Plot R0
getPlotR0all(R0table,nameproject)

################## 
### PREDICTION COURT TERME
#################
###### Get predictions 10 jours
datagouv<-read.table("./data/datagouv.txt",header=TRUE)
datagouv$time<-as.Date(datagouv$time)

datapred<-data.frame(time=c("2020-03-11","2020-03-12","2020-03-13","2020-03-14","2020-03-15","2020-03-16","2020-03-17","2020-03-18","2020-03-19","2020-03-20","2020-03-21","2020-03-22","2020-03-23","2020-03-24","2020-03-25","2020-03-26","2020-03-27","2020-03-28","2020-03-29","2020-03-30","2020-03-31","2020-04-01","2020-04-02","2020-04-03","2020-04-04","2020-04-05","2020-04-06","2020-04-07","2020-04-08","2020-04-09","2020-04-10"))
datapred$i<-seq(0,30,by=1)
for (i in datapred$i){
    datapred$infected[i+1]<-sum(as.numeric(predictions$infected[which(predictions$i==i)]))
    datapred$immunised[i+1]<-sum(as.numeric(predictions$immunised[which(predictions$i==i)]))
    datapred$Iincident[i+1]<-sum(as.numeric(predictions$Iincident[which(predictions$i==i)]))
    datapred$Iincidentmin[i+1]<-sum(as.numeric(predictions$Iincidentmin[which(predictions$i==i)]))
    datapred$Iincidentmax[i+1]<-sum(as.numeric(predictions$Iincidentmax[which(predictions$i==i)]))
    datapred$Hincident[i+1]<-sum(as.numeric(predictions$Hincident[which(predictions$i==i)]))
    datapred$Hincidentmin[i+1]<-sum(as.numeric(predictions$Hincidentmin[which(predictions$i==i)]))
    datapred$Hincidentmax[i+1]<-sum(as.numeric(predictions$Hincidentmax[which(predictions$i==i)]))
    datapred$Dincident[i+1]<-sum(as.numeric(predictions$Dincident[which(predictions$i==i)]))
    datapred$Dincidentmin[i+1]<-sum(as.numeric(predictions$Dincidentmin[which(predictions$i==i)]))
    datapred$Dincidentmax[i+1]<-sum(as.numeric(predictions$Dincidentmax[which(predictions$i==i)]))
    datapred$ICUincident[i+1]<-sum(as.numeric(predictions$ICUincident[which(predictions$i==i)]))
    datapred$ICUincidentmin[i+1]<-sum(as.numeric(predictions$ICUincidentmin[which(predictions$i==i)]))
    datapred$ICUincidentmax[i+1]<-sum(as.numeric(predictions$ICUincidentmax[which(predictions$i==i)]))
}
datapred$time<-as.Date(datapred$time)

p1 <- ggplot(datapred, aes(x=time,y=log10(Iincident)))+ geom_line() + geom_point(data=datagouv, aes(x=time,y=log10(Iobs)))+theme_classic()+ylab("Log10 Cumulative number of ascertained cases") +xlab("Time") + geom_vline(xintercept = as.Date("2020-03-25"))#+geom_ribbon(aes(ymin = log10(Iincidentmin), ymax = log10(Iincidentmax)), fill = "red",alpha=0.2)
p2 <- ggplot(datapred, aes(x=time,y=log10(Hincident)))+ geom_line() + geom_point(data=datagouv, aes(x=time,y=log10(Hobs)))+theme_classic()+ylab("Log10 Prevalent number of hospitalized cases") +xlab("Time") + geom_vline(xintercept = as.Date("2020-03-25"))#+geom_ribbon(aes(ymin = log10(Hincidentmin), ymax = log10(Hincidentmax)), fill = "red",alpha=0.2)
p3 <- ggplot(datapred, aes(x=time,y=log10(ICUincident)))+ geom_line() + geom_point(data=datagouv, aes(x=time,y=log10(ICUobs)))+theme_classic()+ylab("Log10 Prevalent number of ICU cases") +xlab("Time") + geom_vline(xintercept = as.Date("2020-03-25"))#+geom_ribbon(aes(ymin = log10(ICUincidentmin), ymax = log10(ICUincidentmax)), fill = "red",alpha=0.2)
p4 <- ggplot(datapred, aes(x=time,y=log10(Dincident)))+ geom_line() + geom_point(data=datagouv, aes(x=time,y=log10(Dobs)))+theme_classic()+ylab("Log10 Cumulative number of death") +xlab("Time") + geom_vline(xintercept = as.Date("2020-03-25"))#+geom_ribbon(aes(ymin = log10(Dincidentmin), ymax = log10(Dincidentmax)), fill = "red",alpha=0.2)

grid.arrange(p1,p2,p3,p4, ncol=2, nrow = 2)



################## 
### INDICATEURS 
#################
############
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
result<-rbind(result,c("France",datapred$immunised[which(datapred$i==7)]/sizeFR,,datapred$infected[which(datapred$i==7)]/sizeFR,,datapred$immunised[which(datapred$i==14)]/sizeFR,,datapred$infected[which(datapred$i==14)]/sizeFR,,datapred$immunised[which(datapred$i==28)]/sizeFR,,datapred$infected[which(datapred$i==28)]/sizeFR,,datapred$immunised[which(datapred$i==97)]/sizeFR,,datapred$infected[which(datapred$i==97)]/sizeFR))
result$immunised0<-round(result$immunised0*100,1)
result$infected0<-round(result$infected0*100,1)
result$immunised8<-round(result$immunised8*100,1)
result$infected8<-round(result$infected8*100,1)
result$immunised21<-round(result$immunised21*100,1)
result$infected21<-round(result$infected21*100,1)
result$immunised90<-round(result$immunised90*100,1)
result$infected90<-round(result$infected90*100,1)

result<-result[,c("reg","infected0","infected8","infected21","immunised0","immunised8","immunised21")]
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
                                   A0given,b2,tconf,lengthconf,newdailyMove,pred)
            
            nblits<-nbICUplus*ICUcapacity_FR$nbICU_adult[which(ICUcapacity_FR$maille_code==as.character(solution$data$reg_id[1]))]
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
     
     