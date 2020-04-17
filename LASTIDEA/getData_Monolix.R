rm(list = ls())
date<-"20200410"
date2<-"20200325"


#Get the data by region
load("./data/SPF_I_SURSAUD_H_covid19fr.RData")
head(SPF_I_SURSAUD_H_covid19fr)
load("./data/popreg.RData")
head(popreg)


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

chiffres$I13<-c(28068,9896,10086,11375,697,37406,22799,98012,8546,16762,19713,13129,24277,0,0,0,0,0)
chiffres$I13min<-c(23987,6809,7989,9533,462,32236,18690,86754,6522,13485,15880,10467,17210,0,0,0,0,0)
chiffres$I13max<-c(32149,12983,12183,13217,932,32149,26908,109270,10570,20039,23546,15791,31344,0,0,0,0,0)


chiffres$I14<-c(22463,4796,8804,11054,846,30008,17991,66082,8005,15413,16993,7767,17075,0,0,0,0,0)
chiffres$I14min<-c(18577,2972,6719,8758,535,24719,13748,56180,5677,11777,12839,5480,10803,0,0,0,0,0)
chiffres$I14max<-c(26349,6620,10889,13350,1157,35297,22234,75984,10333,19049,21147,10054,23347,0,0,0,0,0)

#write.table(chiffres[,c("names","I12","I12min","I12max","I13","I13min","I13max")],file="./data/sentinelle.txt")


I12<-c()
I12min<-c()
I12max<-c()
I12obs<-c()
I13<-c()
I13min<-c()
I13max<-c()
I13obs<-c()
I14<-c()
I14min<-c()
I14max<-c()
I14obs<-c()
for (i in 1:length(SPF_I_SURSAUD_H_covid19fr$reg_id)){
  I12[i]<-as.numeric(chiffres[which(as.character(chiffres$names)==as.character(SPF_I_SURSAUD_H_covid19fr$reg_id[i])),"I12"])
  I12min[i]<-as.numeric(chiffres[which(as.character(chiffres$names)==as.character(SPF_I_SURSAUD_H_covid19fr$reg_id[i])),"I12min"])
  I12max[i]<-as.numeric(chiffres[which(as.character(chiffres$names)==as.character(SPF_I_SURSAUD_H_covid19fr$reg_id[i])),"I12max"])
  
  I13[i]<-as.numeric(chiffres[which(as.character(chiffres$names)==as.character(SPF_I_SURSAUD_H_covid19fr$reg_id[i])),"I13"])
  I13min[i]<-as.numeric(chiffres[which(as.character(chiffres$names)==as.character(SPF_I_SURSAUD_H_covid19fr$reg_id[i])),"I13min"])
  I13max[i]<-as.numeric(chiffres[which(as.character(chiffres$names)==as.character(SPF_I_SURSAUD_H_covid19fr$reg_id[i])),"I13max"])
  
  I14[i]<-as.numeric(chiffres[which(as.character(chiffres$names)==as.character(SPF_I_SURSAUD_H_covid19fr$reg_id[i])),"I14"])
  I14min[i]<-as.numeric(chiffres[which(as.character(chiffres$names)==as.character(SPF_I_SURSAUD_H_covid19fr$reg_id[i])),"I14min"])
  I14max[i]<-as.numeric(chiffres[which(as.character(chiffres$names)==as.character(SPF_I_SURSAUD_H_covid19fr$reg_id[i])),"I14max"])
  
  I12obs[i]<-sum(SPF_I_SURSAUD_H_covid19fr[which((as.character(SPF_I_SURSAUD_H_covid19fr$date)%in%c("2020-03-16","2020-03-17","2020-03-18","2020-03-19","2020-03-20","2020-03-21","2020-03-22"))&(SPF_I_SURSAUD_H_covid19fr$reg_id==SPF_I_SURSAUD_H_covid19fr$reg_id[i])&(SPF_I_SURSAUD_H_covid19fr$obs_id=="I")),"obs"])
  I13obs[i]<-sum(SPF_I_SURSAUD_H_covid19fr[which((as.character(SPF_I_SURSAUD_H_covid19fr$date)%in%c("2020-03-23","2020-03-24","2020-03-25","2020-03-26","2020-03-27","2020-03-28","2020-03-29"))&(SPF_I_SURSAUD_H_covid19fr$reg_id==SPF_I_SURSAUD_H_covid19fr$reg_id[i])&(SPF_I_SURSAUD_H_covid19fr$obs_id=="I")),"obs"])
  I14obs[i]<-sum(SPF_I_SURSAUD_H_covid19fr[which((as.character(SPF_I_SURSAUD_H_covid19fr$date)%in%c("2020-03-30","2020-03-31","2020-04-01","2020-04-02","2020-04-03","2020-04-04","2020-04-05"))&(SPF_I_SURSAUD_H_covid19fr$reg_id==SPF_I_SURSAUD_H_covid19fr$reg_id[i])&(SPF_I_SURSAUD_H_covid19fr$obs_id=="I")),"obs"])
}
ascertainement<-I12obs/(I12+I12obs)
ascertainementmin<-I12obs/(I12max+I12obs)
ascertainementmax<-I12obs/(I12min+I12obs)
ascertainementsd<-sd(unique(SPF_I_SURSAUD_H_covid19fr$ascertainement))
print(paste("France rsent S12:",sum(unique(I12obs))/(sum(unique(I12obs))+sum(unique(I12)))))
meanr<-sum(unique(I12obs))/(sum(unique(I12obs))+sum(unique(I12)))

ascertainement13<-I13obs/(I13+I13obs)
ascertainementmin13<-I13obs/(I13max+I13obs)
ascertainementmax13<-I13obs/(I13min+I13obs)
ascertainementsd13<-sd(unique(SPF_I_SURSAUD_H_covid19fr$ascertainement13))
print(paste("France rsent S13:",sum(unique(I13obs))/(sum(unique(I13obs))+sum(unique(I13)))))

ascertainement14<-I14obs/(I14+I14obs)
ascertainementmin14<-I14obs/(I14max+I14obs)
ascertainementmax14<-I14obs/(I14min+I14obs)
ascertainementsd14<-sd(unique(SPF_I_SURSAUD_H_covid19fr$ascertainement14))
print(paste("France rsent S14:",sum(unique(I14obs))/(sum(unique(I14obs))+sum(unique(I14)))))

SPF_I_SURSAUD_H_covid19fr$ascertainement<-ascertainement
q95<-quantile(SPF_I_SURSAUD_H_covid19fr$ascertainement,0.975)  
q5<-quantile(SPF_I_SURSAUD_H_covid19fr$ascertainement,0.025)  

(1-(2*meanr)/(1-meanr))/5.1
(1-(2*q95)/(1-q95))/5.1
(1-(2*q5)/(1-q5))/5.1


# par(mfrow=c(1,1))
# plot(SPF_I_SURSAUD_H_covid19fr$ascertainement13,SPF_I_SURSAUD_H_covid19fr$ascertainement)
# summary(lm(SPF_I_SURSAUD_H_covid19fr$ascertainement13~SPF_I_SURSAUD_H_covid19fr$ascertainementmin-1))

###GET NAME
for (i in 1:length(SPF_I_SURSAUD_H_covid19fr$reg_id)){
  SPF_I_SURSAUD_H_covid19fr$IDname[i]<-as.character(chiffres$goodid[which(chiffres$names==SPF_I_SURSAUD_H_covid19fr$reg_id[i])])
}

# ### PB IDF TIME START =33 + I0 / H0
# SPF_I_SURSAUD_H_covid19fr[which(SPF_I_SURSAUD_H_covid19fr$reg_id==unique(SPF_I_SURSAUD_H_covid19fr$reg_id)[1]),]
# SPF_I_SURSAUD_H_covid19fr$init_I0[which(SPF_I_SURSAUD_H_covid19fr$reg_id==unique(SPF_I_SURSAUD_H_covid19fr$reg_id)[1])]<-4
# SPF_I_SURSAUD_H_covid19fr$init_H0[which(SPF_I_SURSAUD_H_covid19fr$reg_id==unique(SPF_I_SURSAUD_H_covid19fr$reg_id)[1])]<-1
# 
# ### PB NAQUITAINE H0
# SPF_I_SURSAUD_H_covid19fr[which(SPF_I_SURSAUD_H_covid19fr$reg_id==unique(SPF_I_SURSAUD_H_covid19fr$reg_id)[2]),]
# SPF_I_SURSAUD_H_covid19fr$init_H0[which(SPF_I_SURSAUD_H_covid19fr$reg_id==unique(SPF_I_SURSAUD_H_covid19fr$reg_id)[2])]<-1
# 
# ### PB AURA H0
# SPF_I_SURSAUD_H_covid19fr[which(SPF_I_SURSAUD_H_covid19fr$reg_id==unique(SPF_I_SURSAUD_H_covid19fr$reg_id)[3]),]
# SPF_I_SURSAUD_H_covid19fr$init_H0[which(SPF_I_SURSAUD_H_covid19fr$reg_id==unique(SPF_I_SURSAUD_H_covid19fr$reg_id)[3])]<-1
# 
# ### PB CENTRE H0
# SPF_I_SURSAUD_H_covid19fr[which(SPF_I_SURSAUD_H_covid19fr$reg_id==unique(SPF_I_SURSAUD_H_covid19fr$reg_id)[4]),]
# SPF_I_SURSAUD_H_covid19fr$init_H0[which(SPF_I_SURSAUD_H_covid19fr$reg_id==unique(SPF_I_SURSAUD_H_covid19fr$reg_id)[4])]<-1
# 
# ### PB BFC H0
# SPF_I_SURSAUD_H_covid19fr[which(SPF_I_SURSAUD_H_covid19fr$reg_id==unique(SPF_I_SURSAUD_H_covid19fr$reg_id)[5]),]
# SPF_I_SURSAUD_H_covid19fr$init_H0[which(SPF_I_SURSAUD_H_covid19fr$reg_id==unique(SPF_I_SURSAUD_H_covid19fr$reg_id)[5])]<-10
# 
# ### PB Normandie H0
# SPF_I_SURSAUD_H_covid19fr[which(SPF_I_SURSAUD_H_covid19fr$reg_id==unique(SPF_I_SURSAUD_H_covid19fr$reg_id)[6]),]
# SPF_I_SURSAUD_H_covid19fr$init_H0[which(SPF_I_SURSAUD_H_covid19fr$reg_id==unique(SPF_I_SURSAUD_H_covid19fr$reg_id)[6])]<-5
# 
# ### PB HDF H0
# SPF_I_SURSAUD_H_covid19fr[which(SPF_I_SURSAUD_H_covid19fr$reg_id==unique(SPF_I_SURSAUD_H_covid19fr$reg_id)[7]),]
# SPF_I_SURSAUD_H_covid19fr$init_H0[which(SPF_I_SURSAUD_H_covid19fr$reg_id==unique(SPF_I_SURSAUD_H_covid19fr$reg_id)[7])]<-1
# 
# ### PB grandest H0
# SPF_I_SURSAUD_H_covid19fr[which(SPF_I_SURSAUD_H_covid19fr$reg_id==unique(SPF_I_SURSAUD_H_covid19fr$reg_id)[8]),]
# SPF_I_SURSAUD_H_covid19fr$init_H0[which(SPF_I_SURSAUD_H_covid19fr$reg_id==unique(SPF_I_SURSAUD_H_covid19fr$reg_id)[8])]<-4
# 
# ### PB Paysloire H0
# SPF_I_SURSAUD_H_covid19fr[which(SPF_I_SURSAUD_H_covid19fr$reg_id==unique(SPF_I_SURSAUD_H_covid19fr$reg_id)[9]),]
# SPF_I_SURSAUD_H_covid19fr$init_H0[which(SPF_I_SURSAUD_H_covid19fr$reg_id==unique(SPF_I_SURSAUD_H_covid19fr$reg_id)[9])]<-7
# 
# ### PB Bretagne H0
# SPF_I_SURSAUD_H_covid19fr[which(SPF_I_SURSAUD_H_covid19fr$reg_id==unique(SPF_I_SURSAUD_H_covid19fr$reg_id)[10]),]
# SPF_I_SURSAUD_H_covid19fr$init_H0[which(SPF_I_SURSAUD_H_covid19fr$reg_id==unique(SPF_I_SURSAUD_H_covid19fr$reg_id)[10])]<-1
# 
# ### PB Occitanie H0
# SPF_I_SURSAUD_H_covid19fr[which(SPF_I_SURSAUD_H_covid19fr$reg_id==unique(SPF_I_SURSAUD_H_covid19fr$reg_id)[11]),]
# SPF_I_SURSAUD_H_covid19fr$init_H0[which(SPF_I_SURSAUD_H_covid19fr$reg_id==unique(SPF_I_SURSAUD_H_covid19fr$reg_id)[11])]<-1
# 
# ### PB PACA H0
# SPF_I_SURSAUD_H_covid19fr[which(SPF_I_SURSAUD_H_covid19fr$reg_id==unique(SPF_I_SURSAUD_H_covid19fr$reg_id)[12]),]
# SPF_I_SURSAUD_H_covid19fr$init_H0[which(SPF_I_SURSAUD_H_covid19fr$reg_id==unique(SPF_I_SURSAUD_H_covid19fr$reg_id)[12])]<-6
# 
# ### PB Corse H0
# SPF_I_SURSAUD_H_covid19fr[which(SPF_I_SURSAUD_H_covid19fr$reg_id==unique(SPF_I_SURSAUD_H_covid19fr$reg_id)[13]),]
# SPF_I_SURSAUD_H_covid19fr$init_H0[which(SPF_I_SURSAUD_H_covid19fr$reg_id==unique(SPF_I_SURSAUD_H_covid19fr$reg_id)[13])]<-22

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

### GET TIME SINCE CONFINEMENT
SPF_I_SURSAUD_H_covid19fr$timesinceconf<-ifelse(SPF_I_SURSAUD_H_covid19fr$isolation==0,0,SPF_I_SURSAUD_H_covid19fr$date-as.Date("2020-03-17"))


### SPLINES
library(splines)
splines2 <- ns(SPF_I_SURSAUD_H_covid19fr$day, knots=quantile(SPF_I_SURSAUD_H_covid19fr$day, 0.5))
SPF_I_SURSAUD_H_covid19fr$spline1<-splines2[,1]
SPF_I_SURSAUD_H_covid19fr$spline2<-splines2[,2]

### SAVE DATASET <25/03
SPF_I_SURSAUD_H_covid19fr_small<-SPF_I_SURSAUD_H_covid19fr[which(as.Date(as.character(SPF_I_SURSAUD_H_covid19fr$date))<=as.Date("2020-03-25")),]
write.table(SPF_I_SURSAUD_H_covid19fr_small,file=paste("./LASTIDEA/data_monolix_",date2,".txt",sep=""),sep="\t",row.names = F,quote=F)

### SAVE DATASET
SPF_I_SURSAUD_H_covid19fr$march25<-ifelse(as.Date(as.character(SPF_I_SURSAUD_H_covid19fr$date))<=as.Date("2020-03-25"),0,1)
write.table(SPF_I_SURSAUD_H_covid19fr,file=paste("./LASTIDEA/data_monolix_",date,".txt",sep=""),sep="\t",row.names = F,quote=F)

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



############## MONOLIX UPDATE
# date<-"20200410"
# SPF_I_SURSAUD_H_covid19fr<-read.table(file="./MONOLIX/data_monolix_20200410.txt",header=TRUE)
# indivparams<-read.table(file="./MONOLIX/outputMonolix/Final_20200325/IndividualParameters/estimatedIndividualParameters.txt",header=TRUE,sep=",")
# for (i in 1:length(SPF_I_SURSAUD_H_covid19fr$day)){
#   SPF_I_SURSAUD_H_covid19fr$b[i]<-indivparams$b1_mode[which(indivparams$id==SPF_I_SURSAUD_H_covid19fr$IDname[i])]
#   SPF_I_SURSAUD_H_covid19fr$Dq[i]<-indivparams$Dq_mode[which(indivparams$id==SPF_I_SURSAUD_H_covid19fr$IDname[i])]
#   SPF_I_SURSAUD_H_covid19fr$A0[i]<-indivparams$A0_mode[which(indivparams$id==SPF_I_SURSAUD_H_covid19fr$IDname[i])]
#   SPF_I_SURSAUD_H_covid19fr$E0[i]<-indivparams$E0_mode[which(indivparams$id==SPF_I_SURSAUD_H_covid19fr$IDname[i])]
# }
# write.table(SPF_I_SURSAUD_H_covid19fr,file=paste("./MONOLIX/data_monolix_update_",date,".txt",sep=""),sep="\t",row.names = F,quote=F)
