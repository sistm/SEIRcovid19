rm(list = ls())
date<-"20200403"
  

#Get the data by region
load("./data/SPF_I_SURSAUD_H_covid19fr.RData")
head(SPF_I_SURSAUD_H_covid19fr)
load("./data/popreg.RData")
head(popreg)
head(SPF_I_SURSAUD_H_covid19fr)


### SUPRESSION REGIONS
#Supress DOM TOM
supression<-c("REG-01","REG-02","REG-03","REG-04","REG-05","REG-06","REG-94")
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
# write.table(chiffres[,c("names","I12","I12min","I12max")],file="./data/sentinelle.txt")




I12<-c()
I12min<-c()
I12max<-c()
I12obs<-c()
for (i in 1:length(SPF_I_SURSAUD_H_covid19fr$reg_id)){
  I12[i]<-as.numeric(chiffres[which(as.character(chiffres$names)==as.character(SPF_I_SURSAUD_H_covid19fr$reg_id[i])),"I12"])
  I12min[i]<-as.numeric(chiffres[which(as.character(chiffres$names)==as.character(SPF_I_SURSAUD_H_covid19fr$reg_id[i])),"I12min"])
  I12max[i]<-as.numeric(chiffres[which(as.character(chiffres$names)==as.character(SPF_I_SURSAUD_H_covid19fr$reg_id[i])),"I12max"])
  I12obs[i]<-sum(SPF_I_SURSAUD_H_covid19fr[which((as.character(SPF_I_SURSAUD_H_covid19fr$date)%in%c("2020-03-16","2020-03-17","2020-03-18","2020-03-19","2020-03-20","2020-03-21","2020-03-22"))&(SPF_I_SURSAUD_H_covid19fr$reg_id==SPF_I_SURSAUD_H_covid19fr$reg_id[i])&(SPF_I_SURSAUD_H_covid19fr$obs_id=="I")),"obs"])
}
SPF_I_SURSAUD_H_covid19fr$ascertainement<-I12obs/(I12+I12obs)
SPF_I_SURSAUD_H_covid19fr$ascertainementmin<-I12obs/(I12max+I12obs)
SPF_I_SURSAUD_H_covid19fr$ascertainementmax<-I12obs/(I12min+I12obs)
SPF_I_SURSAUD_H_covid19fr$ascertainementsd<-sd(unique(SPF_I_SURSAUD_H_covid19fr$ascertainement))

###GET NAME
for (i in 1:length(SPF_I_SURSAUD_H_covid19fr$reg_id)){
  SPF_I_SURSAUD_H_covid19fr$IDname[i]<-as.character(chiffres$goodid[which(chiffres$names==SPF_I_SURSAUD_H_covid19fr$reg_id[i])])
}

### PB IDF TIME START =33 + I0 / H0
SPF_I_SURSAUD_H_covid19fr[which(SPF_I_SURSAUD_H_covid19fr$reg_id==unique(SPF_I_SURSAUD_H_covid19fr$reg_id)[1]),]
SPF_I_SURSAUD_H_covid19fr[40,7]<-86
SPF_I_SURSAUD_H_covid19fr[41,7]<-158
SPF_I_SURSAUD_H_covid19fr$day[which(SPF_I_SURSAUD_H_covid19fr$reg_id==unique(SPF_I_SURSAUD_H_covid19fr$reg_id)[1])]<-SPF_I_SURSAUD_H_covid19fr$day[which(SPF_I_SURSAUD_H_covid19fr$reg_id==unique(SPF_I_SURSAUD_H_covid19fr$reg_id)[1])]-33
SPF_I_SURSAUD_H_covid19fr<-SPF_I_SURSAUD_H_covid19fr[which(SPF_I_SURSAUD_H_covid19fr$day>=0),]
SPF_I_SURSAUD_H_covid19fr$init_I0[which(SPF_I_SURSAUD_H_covid19fr$reg_id==unique(SPF_I_SURSAUD_H_covid19fr$reg_id)[1])]<-25
SPF_I_SURSAUD_H_covid19fr$init_H0[which(SPF_I_SURSAUD_H_covid19fr$reg_id==unique(SPF_I_SURSAUD_H_covid19fr$reg_id)[1])]<-5

### PB NAQUITAINE H0
SPF_I_SURSAUD_H_covid19fr[which(SPF_I_SURSAUD_H_covid19fr$reg_id==unique(SPF_I_SURSAUD_H_covid19fr$reg_id)[2]),]
SPF_I_SURSAUD_H_covid19fr$init_H0[which(SPF_I_SURSAUD_H_covid19fr$reg_id==unique(SPF_I_SURSAUD_H_covid19fr$reg_id)[2])]<-1

### PB AURA H0
SPF_I_SURSAUD_H_covid19fr[which(SPF_I_SURSAUD_H_covid19fr$reg_id==unique(SPF_I_SURSAUD_H_covid19fr$reg_id)[3]),]
SPF_I_SURSAUD_H_covid19fr$init_H0[which(SPF_I_SURSAUD_H_covid19fr$reg_id==unique(SPF_I_SURSAUD_H_covid19fr$reg_id)[3])]<-3

### PB CENTRE H0
SPF_I_SURSAUD_H_covid19fr[which(SPF_I_SURSAUD_H_covid19fr$reg_id==unique(SPF_I_SURSAUD_H_covid19fr$reg_id)[4]),]
SPF_I_SURSAUD_H_covid19fr$init_H0[which(SPF_I_SURSAUD_H_covid19fr$reg_id==unique(SPF_I_SURSAUD_H_covid19fr$reg_id)[4])]<-1

### PB BFC H0
SPF_I_SURSAUD_H_covid19fr[which(SPF_I_SURSAUD_H_covid19fr$reg_id==unique(SPF_I_SURSAUD_H_covid19fr$reg_id)[5]),]
SPF_I_SURSAUD_H_covid19fr$init_H0[which(SPF_I_SURSAUD_H_covid19fr$reg_id==unique(SPF_I_SURSAUD_H_covid19fr$reg_id)[5])]<-10

### PB Normandie H0
SPF_I_SURSAUD_H_covid19fr[which(SPF_I_SURSAUD_H_covid19fr$reg_id==unique(SPF_I_SURSAUD_H_covid19fr$reg_id)[6]),]
SPF_I_SURSAUD_H_covid19fr$init_H0[which(SPF_I_SURSAUD_H_covid19fr$reg_id==unique(SPF_I_SURSAUD_H_covid19fr$reg_id)[6])]<-5

### PB HDF H0
SPF_I_SURSAUD_H_covid19fr[which(SPF_I_SURSAUD_H_covid19fr$reg_id==unique(SPF_I_SURSAUD_H_covid19fr$reg_id)[7]),]
SPF_I_SURSAUD_H_covid19fr$init_H0[which(SPF_I_SURSAUD_H_covid19fr$reg_id==unique(SPF_I_SURSAUD_H_covid19fr$reg_id)[7])]<-1

### PB grandest H0
SPF_I_SURSAUD_H_covid19fr[which(SPF_I_SURSAUD_H_covid19fr$reg_id==unique(SPF_I_SURSAUD_H_covid19fr$reg_id)[8]),]
SPF_I_SURSAUD_H_covid19fr$init_H0[which(SPF_I_SURSAUD_H_covid19fr$reg_id==unique(SPF_I_SURSAUD_H_covid19fr$reg_id)[8])]<-4

### PB Paysloire H0
SPF_I_SURSAUD_H_covid19fr[which(SPF_I_SURSAUD_H_covid19fr$reg_id==unique(SPF_I_SURSAUD_H_covid19fr$reg_id)[9]),]
SPF_I_SURSAUD_H_covid19fr$init_H0[which(SPF_I_SURSAUD_H_covid19fr$reg_id==unique(SPF_I_SURSAUD_H_covid19fr$reg_id)[9])]<-7

### PB Bretagne H0
SPF_I_SURSAUD_H_covid19fr[which(SPF_I_SURSAUD_H_covid19fr$reg_id==unique(SPF_I_SURSAUD_H_covid19fr$reg_id)[10]),]
SPF_I_SURSAUD_H_covid19fr$init_H0[which(SPF_I_SURSAUD_H_covid19fr$reg_id==unique(SPF_I_SURSAUD_H_covid19fr$reg_id)[10])]<-1

### PB Occitanie H0
SPF_I_SURSAUD_H_covid19fr[which(SPF_I_SURSAUD_H_covid19fr$reg_id==unique(SPF_I_SURSAUD_H_covid19fr$reg_id)[11]),]
SPF_I_SURSAUD_H_covid19fr$init_H0[which(SPF_I_SURSAUD_H_covid19fr$reg_id==unique(SPF_I_SURSAUD_H_covid19fr$reg_id)[11])]<-1

### PB PACA H0
SPF_I_SURSAUD_H_covid19fr[which(SPF_I_SURSAUD_H_covid19fr$reg_id==unique(SPF_I_SURSAUD_H_covid19fr$reg_id)[12]),]
SPF_I_SURSAUD_H_covid19fr$init_H0[which(SPF_I_SURSAUD_H_covid19fr$reg_id==unique(SPF_I_SURSAUD_H_covid19fr$reg_id)[12])]<-6

### PB Corse H0
SPF_I_SURSAUD_H_covid19fr[which(SPF_I_SURSAUD_H_covid19fr$reg_id==unique(SPF_I_SURSAUD_H_covid19fr$reg_id)[13]),]
SPF_I_SURSAUD_H_covid19fr$init_H0[which(SPF_I_SURSAUD_H_covid19fr$reg_id==unique(SPF_I_SURSAUD_H_covid19fr$reg_id)[13])]<-22

## PROPER OBS_ID
SPF_I_SURSAUD_H_covid19fr$obs_id<-ifelse(SPF_I_SURSAUD_H_covid19fr$obs_id=="I",1,2)

### LISSAGE
# SPF_I_SURSAUD_H_covid19fr$date<-as.character(SPF_I_SURSAUD_H_covid19fr$date)
# new<-SPF_I_SURSAUD_H_covid19fr[1,]
# new[,]<-NA
# new$lissage<-NA
# new$init_I0_lissage<-NA
# new$init_H0_lissage<-NA
# 
# for (region in unique(SPF_I_SURSAUD_H_covid19fr$reg_id)){
#   getobs1<-SPF_I_SURSAUD_H_covid19fr[which((SPF_I_SURSAUD_H_covid19fr$reg_id==region)&(SPF_I_SURSAUD_H_covid19fr$obs_id==1)),]
#   for (i in 1:(length(getobs1$reg_id)-2)){
#     temp<-round(mean(c(getobs1$obs[i+2],getobs1$obs[i+1],getobs1$obs[i]),na.rm = TRUE),0)
#     getobs1$lissage[i]<-ifelse(is.nan(temp),NA,temp)
#   }
#   temp<-round(mean(c(getobs1$obs[length(getobs1$reg_id)-1],getobs1$obs[length(getobs1$reg_id)]),na.rm = TRUE))
#   getobs1$lissage[length(getobs1$reg_id)-1]<-ifelse(is.nan(temp),NA,temp)
#   getobs1$lissage[length(getobs1$reg_id)]<- getobs1$obs[length(getobs1$reg_id)]
#   getobs1$init_I0_lissage<-getobs1$lissage[1]
#   getobs1$init_H0_lissage<-NA
#   
#   getobs2<-SPF_I_SURSAUD_H_covid19fr[which((SPF_I_SURSAUD_H_covid19fr$reg_id==region)&(SPF_I_SURSAUD_H_covid19fr$obs_id==2)),]
#   for (i in 1:(length(getobs2$reg_id)-2)){
#     temp<-round(mean(c(getobs2$obs[i+2],getobs2$obs[i+1],getobs2$obs[i]),na.rm = TRUE),0)
#     getobs2$lissage[i]<-ifelse(is.nan(temp),NA,temp)
#   }
#   temp<-round(mean(c(getobs2$obs[length(getobs2$reg_id)-1],getobs2$obs[length(getobs2$reg_id)]),na.rm = TRUE))
#   getobs2$lissage[length(getobs2$reg_id)-1]<-ifelse(is.nan(temp),NA,temp)
#   getobs2$lissage[length(getobs2$reg_id)]<- getobs2$obs[length(getobs2$reg_id)]
#   getobs2$init_I0_lissage<-NA
#   getobs2$init_H0_lissage<-getobs2$lissage[1]
#   
#   
#   getobs2$init_I0_lissage<-getobs1$init_I0_lissage[1]
#   getobs1$init_H0_lissage<-getobs2$init_H0_lissage[1]
#   
#   new<-rbind(new,getobs1)
#   new<-rbind(new,getobs2)
# }
# SPF_I_SURSAUD_H_covid19fr<-new
# SPF_I_SURSAUD_H_covid19fr[which(SPF_I_SURSAUD_H_covid19fr$reg_id==unique(SPF_I_SURSAUD_H_covid19fr$reg_id)[13]),]

## REMOVE NA FOR OBS
SPF_I_SURSAUD_H_covid19fr<-SPF_I_SURSAUD_H_covid19fr[which(!is.na(SPF_I_SURSAUD_H_covid19fr$obs)),]


### SAVE DATASET
write.table(SPF_I_SURSAUD_H_covid19fr,file=paste("./Monolix_final/data_monolix_",date,".txt",sep=""),sep="\t",row.names = F,quote=F)


### VISU RAPIDE
dev.off()
par(mfrow=c(3,4))
for(i in unique(SPF_I_SURSAUD_H_covid19fr$reg_id)){
  plot(SPF_I_SURSAUD_H_covid19fr$day[which((as.character(SPF_I_SURSAUD_H_covid19fr$reg_id)==i)&(SPF_I_SURSAUD_H_covid19fr$obs_id==1))],SPF_I_SURSAUD_H_covid19fr$obs[which((SPF_I_SURSAUD_H_covid19fr$reg_id==i)&(SPF_I_SURSAUD_H_covid19fr$obs_id==1))],ylab="I",xlab="time")
}

par(mfrow=c(3,4))
for(i in unique(SPF_I_SURSAUD_H_covid19fr$reg_id)){
  plot(SPF_I_SURSAUD_H_covid19fr$day[which((as.character(SPF_I_SURSAUD_H_covid19fr$reg_id)==i)&(SPF_I_SURSAUD_H_covid19fr$obs_id==2))],SPF_I_SURSAUD_H_covid19fr$obs[which((SPF_I_SURSAUD_H_covid19fr$reg_id==i)&(SPF_I_SURSAUD_H_covid19fr$obs_id==2))],ylab="H",xlab="time")
}


