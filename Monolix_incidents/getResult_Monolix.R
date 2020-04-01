library(xtable)
library(ggplot2)

indivParams = read.table(paste(path,"/outputMonolix/",nameproject,"/IndividualParameters/estimatedIndividualParameters.txt",sep=""),header=TRUE,sep=",")
dir.create(paste(path,"outputMonolix/",nameproject,"/graphics",sep=""))
for (i in 1:length(indivParams$id)){
indivParams$r_mode[i]<-data$ascertainement[which((data$IDname==indivParams$id[i])&(data$day==0)&(data$obs_id==1))]
}
data<-read.table(paste(path,dataname,sep=""),sep="\t",header=TRUE)
 data$date<-lubridate::as_date(as.character(data$date))
 timesR0<-data[which((data$date=="2020-03-17")&(data$obs_id==1)),c("day","IDname")]

######### GET THE FITS
for (i in 1:length(indivParams$id)){
   print(i)

   b<-as.numeric(indivParams[i,c("b_mode")])
   r<-as.numeric(indivParams[i,c("r_mode")])
   dataregion<-data[which(data$IDname==as.character(indivParams[i,1])),]
   alpha<-1.5
   De<-5.2
   Di<-2.3
   Dq<-as.numeric(indivParams[i,c("Dq_mode")])
   Dh<-30
   popSize<-chiffres[which(chiffres$goodid==as.character(indivParams[i,1])),"size"]
   E0given<-as.numeric(indivParams[i,c("E0_mode")])
   A0given<-as.numeric(indivParams[i,c("A0_mode")])
   
   # i=2
   # initwithdata=TRUE
   # binit=as.numeric(indivParams[i,c("b_mode","r_mode")])
   # data=data[which(as.character(data$IDname)==as.character(indivParams[i,1])),]
   # alpha=1
   # De=5.2
   # Di=2.3
   # Dq=as.numeric(indivParams[i,"Dq_mode"])
   # Dh=30
   # popSize=chiffres[which(chiffres$goodid==as.character(indivParams[i,1])),"size"]
   # dailyMove=0.01*chiffres[which(chiffres$goodid==as.character(indivParams[i,1])),"size"]
   # timeconf=1000
   # lengthconf=1000
   # newdailyMove=0
   # factorreductrans=10
   # verbose = TRUE
   # optim_ols=FALSE
   # obs="2Y"
   #    E0given=as.numeric(indivParams[i,c("test_mode")])

   temp_monolix_estim<-seirah_estim(binit=c(b,r),
                                    data=dataregion,
                                    alpha=alpha,
                                    De=De,
                                    Di=Di,
                                    Dq=Dq,
                                    Dh=Dh,
                                    popSize=popSize,
                                    dailyMove=0.01*popSize,
                                    timeconf=1000,
                                    lengthconf=1000,
                                    newdailyMove=0,
                                    factorreductrans=10,
                                    verbose = TRUE,
                                    optim_ols=FALSE,
                                    obs="2Y",
                                    E0given=E0given,
                                    A0given=A0given)#,b2=as.numeric(indivParams[i,"b1_mode"]),r2=as.numeric(indivParams[i,"ascertainment_mode"]))

   jpeg(paste(path,"outputMonolix/",nameproject,"/graphics/FitI_",as.character(indivParams[i,1]),".jpg",sep=""))
   print(plot(temp_monolix_estim,type=1))
   dev.off()
   jpeg(paste(path,"outputMonolix/",nameproject,"/graphics/FitH_",as.character(indivParams[i,1]),".jpg",sep=""))
   print(plot(temp_monolix_estim,type=2)
   )
   dev.off()
}

 
######### GET R0
for (i in 1:length(indivParams$id)){
   temp_monolix_estim<-seirah_estim(binit=as.numeric(indivParams[i,c("b_mode","r_mode")]),
                                    data=data[which(data$IDname==as.character(indivParams[i,1])),],
                                    alpha=1,
                                    De=5.2,
                                    Di=2.3,
                                    Dq=as.numeric(indivParams[i,c("Dq_mode")]),
                                    Dh=30,
                                    popSize=chiffres[which(chiffres$goodid==as.character(indivParams[i,1])),"size"],
                                    dailyMove=0.01*chiffres[which(chiffres$goodid==as.character(indivParams[i,1])),"size"],
                                    timeconf=1000,
                                    lengthconf=1000,
                                    newdailyMove=0,
                                    factorreductrans=10,
                                    verbose = TRUE,
                                    optim_ols=FALSE,obs="2Y",E0given=as.numeric(indivParams[i,c("test_mode")]))
   
     Di=2.3
     alpha=1
     b=as.numeric(indivParams[i,2])
     Dq=as.numeric(indivParams[i,c("Dq_mode")])
     
    indivParams$R0[i]<-  Di*/(temp_monolix_estim$solution[timesR0$day[which(timesR0$IDname==indivParams$id[i])],"A"]+temp_monolix_estim$solution[timesR0$day[which(timesR0$IDname==indivParams$id[i])],"I"])*(alpha*temp_monolix_estim$solution[timesR0$day[which(timesR0$IDname==indivParams$id[i])],"A"]+Dq*temp_monolix_estim$solution[timesR0$day[which(timesR0$IDname==indivParams$id[i])],"I"]/(Di+Dq))
}
 
 xtable(indivParams[,c("id","R0")])
 
        