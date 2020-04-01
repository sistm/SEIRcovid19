rm(list = ls())
date<-"20200330"
  

#Get the data by region
load("./data/SPF_I_SURSAUD_H_covid19fr.RData")
load("./data/popreg.RData")
head(popreg)
head(SPF_I_SURSAUD_H_covid19fr)


### SUPRESSION REGIONS
#Supress DOM TOM
supression<-c("REG-01","REG-02","REG-03","REG-04","REG-05","REG-06")
SPF_I_SURSAUD_H_covid19fr<-SPF_I_SURSAUD_H_covid19fr[which(!SPF_I_SURSAUD_H_covid19fr$reg_id%in%supression),]
length(unique(SPF_I_SURSAUD_H_covid19fr$reg_id))


###GET POPSIZE
for (i in 1:length(SPF_I_SURSAUD_H_covid19fr$reg_id)){
  SPF_I_SURSAUD_H_covid19fr$popsize[i]<-as.numeric(popreg$population[which(popreg$maille_code==SPF_I_SURSAUD_H_covid19fr$reg_id[i])])
}

### GET R FROM SENTINELLE NETWORK
chiffres<-read.table("./TRASH/monolix/Chiffres.txt",sep="\t",header=TRUE)
chiffres$I12<-c(34071,10165,10207,7561,390,37253,23489,90448,15332,13995,15881,18150,18422,0,0,0,0,0)
chiffres$I12min<-c(28398,7635,7881,6015,184,31737,18982,78677,10175,10549,12409,12914,13522,0,0,0,0,0)
chiffres$I12max<-c(38744,12695,12533,9107,596,42769,27996,102219,20489,17441,19353,23386,23322,0,0,0,0,0)
I12<-c()
I12min<-c()
I12max<-c()
I12obs<-c()
for (i in 1:length(SPF_I_SURSAUD_H_covid19fr$reg_id)){
  I12[i]<-as.numeric(chiffres[which(as.character(chiffres$names)==as.character(SPF_I_SURSAUD_H_covid19fr$reg_id[i])),"I12"])
  I12min[i]<-as.numeric(chiffres[which(as.character(chiffres$names)==as.character(SPF_I_SURSAUD_H_covid19fr$reg_id[i])),"I12min"])
  I12max[i]<-as.numeric(chiffres[which(as.character(chiffres$names)==as.character(SPF_I_SURSAUD_H_covid19fr$reg_id[i])),"I12max"])
  I12obs[i]<-sum(SPF_I_SURSAUD_H_covid19fr[which((as.character(SPF_I_SURSAUD_H_covid19fr$date)%in%c("2020-03-16","2020-03-17","2020-03-18","2020-03-19","2020-03-20","2020-03-22","2020-03-23"))&(SPF_I_SURSAUD_H_covid19fr$reg_id==SPF_I_SURSAUD_H_covid19fr$reg_id[i])&(SPF_I_SURSAUD_H_covid19fr$obs_id=="I")),"obs"])
}
SPF_I_SURSAUD_H_covid19fr$ascertainement<-I12obs/I12
SPF_I_SURSAUD_H_covid19fr$ascertainementmin<-I12obs/I12max
SPF_I_SURSAUD_H_covid19fr$ascertainementmax<-I12obs/I12min
SPF_I_SURSAUD_H_covid19fr$ascertainementsd<-sd(unique(SPF_I_SURSAUD_H_covid19fr$ascertainement))


###GET NAME
for (i in 1:length(SPF_I_SURSAUD_H_covid19fr$reg_id)){
  SPF_I_SURSAUD_H_covid19fr$IDname[i]<-as.character(chiffres$goodid[which(chiffres$names==SPF_I_SURSAUD_H_covid19fr$reg_id[i])])
}

### PB IDF TIME START =30
SPF_I_SURSAUD_H_covid19fr[which(SPF_I_SURSAUD_H_covid19fr$reg_id==unique(SPF_I_SURSAUD_H_covid19fr$reg_id)[1]),]
SPF_I_SURSAUD_H_covid19fr$day[which(SPF_I_SURSAUD_H_covid19fr$reg_id==unique(SPF_I_SURSAUD_H_covid19fr$reg_id)[1])]<-SPF_I_SURSAUD_H_covid19fr$day[which(SPF_I_SURSAUD_H_covid19fr$reg_id==unique(SPF_I_SURSAUD_H_covid19fr$reg_id)[1])]-30
SPF_I_SURSAUD_H_covid19fr<-SPF_I_SURSAUD_H_covid19fr[which(SPF_I_SURSAUD_H_covid19fr$day>=0),]

## PROPER OBS_ID
SPF_I_SURSAUD_H_covid19fr$obs_id<-ifelse(SPF_I_SURSAUD_H_covid19fr$obs_id=="I",1,2)

## REMOVE NA FOR OBS
SPF_I_SURSAUD_H_covid19fr<-SPF_I_SURSAUD_H_covid19fr[which(!is.na(SPF_I_SURSAUD_H_covid19fr$obs)),]

### SAVE DATASET
write.table(SPF_I_SURSAUD_H_covid19fr,file=paste("./Monolix_incidents/data_monolix_",date,".txt",sep=""),sep="\t",row.names = F,quote=F)


### VISU RAPIDE
dev.off()
par(mfrow=c(3,5))
for(i in unique(SPF_I_SURSAUD_H_covid19fr$reg_id)){
  plot(SPF_I_SURSAUD_H_covid19fr$day[which((as.character(SPF_I_SURSAUD_H_covid19fr$reg_id)==i)&(SPF_I_SURSAUD_H_covid19fr$obs_id==1))],SPF_I_SURSAUD_H_covid19fr$obs[which((SPF_I_SURSAUD_H_covid19fr$reg_id==i)&(SPF_I_SURSAUD_H_covid19fr$obs_id==1))],ylab="I",xlab="time")
}

par(mfrow=c(3,5))
for(i in unique(SPF_I_SURSAUD_H_covid19fr$reg_id)){
  plot(SPF_I_SURSAUD_H_covid19fr$day[which((as.character(SPF_I_SURSAUD_H_covid19fr$reg_id)==i)&(SPF_I_SURSAUD_H_covid19fr$obs_id==2))],SPF_I_SURSAUD_H_covid19fr$obs[which((SPF_I_SURSAUD_H_covid19fr$reg_id==i)&(SPF_I_SURSAUD_H_covid19fr$obs_id==2))],ylab="H",xlab="time")
}

