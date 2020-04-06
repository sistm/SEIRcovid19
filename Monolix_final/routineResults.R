######### GET THE SOLUTION FROM 0DE SOLVER
getSolution<-function(b,
                      r,
                      dataregion,
                      alpha,
                      De,
                      Di,
                      Dq,
                      Dh,
                      popSize,
                      E0given,
                      A0given,b2,tconf,lengthconf=1000,newdailyMove=0,pred=FALSE){

  # i=2
  # initwithdata=TRUE
  # binit=c(b,r)
  # data=dataregion
  # dailyMove=0.01*popSize
  # timeconf=tconf
  # lengthconf=1000
  # newdailyMove=0
  # factorreductrans=NULL
  # verbose = TRUE
  # optim_ols=FALSE
  # obs="2Y"
  # pred=FALSE

  temp_monolix_estim<-seirah_estim(binit=c(b,r),
                                   data=dataregion,
                                   alpha=alpha,
                                   De=De,
                                   Di=Di,
                                   Dq=Dq,
                                   Dh=Dh,
                                   popSize=popSize,
                                   dailyMove=0.01*popSize,
                                   timeconf=tconf,
                                   lengthconf=lengthconf,
                                   newdailyMove=newdailyMove,
                                   verbose = TRUE,
                                   optim_ols=FALSE,
                                   obs="2Y",
                                   E0given=E0given,
                                   A0given=A0given,
                                   b2=b2,
                                   pred=pred)

  return(temp_monolix_estim)

}




#### PLOT THE SOLUTION
getPlot<-function(temp_monolix_estim,nameproject,indivParamsreg){
  jpeg(paste(path,"outputMonolix/",nameproject,"/graphics/FitI_",as.character(indivParamsreg[1,1]),".jpg",sep=""))
  print(plot(temp_monolix_estim,type=1))
  dev.off()
  jpeg(paste(path,"outputMonolix/",nameproject,"/graphics/FitH_",as.character(indivParamsreg[1,1]),".jpg",sep=""))
  print(plot(temp_monolix_estim,type=2)
  )
  dev.off()

}


### GET THE R0
#indivParamsreg<-indivParams[i,]
getR0<-function(solution,indivParamsreg){

  res<-as.data.frame(matrix(NA,ncol=13,nrow=0))
  names(res)<-c("reg","date","time","R0","R0ICmin","R0ICmax","I","Imin","Imax","A","Amin","Amax","this")


  datestart<-solution$data$date[1]
  Dq<-as.numeric(indivParamsreg[1,"Dq_mode"])
  alpha<-solution$parameters$alpha
  Di<-solution$parameters$Di

  Dqmin<-as.numeric(indivParamsreg[1,"Dq_mode"])-1.96*as.numeric(indivParamsreg[1,"Dq_sd"])
  Dqmax<-as.numeric(indivParamsreg[1,"Dq_mode"])+1.96*as.numeric(indivParamsreg[1,"Dq_sd"])


  for (time in 1:100){

    if((time>=solution$parameters$timeconf)&(time<(solution$parameters$timeconf+solution$parameters$lengthconf))){
      b<-as.numeric(indivParamsreg[1,"b1_mode"])*exp(as.numeric(indivParamsreg[1,"beta_mode"]))
      bmin<-b-1.96*exp(as.numeric(indivParamsreg[1,"beta_mode"]))*as.numeric(indivParamsreg[1,"b1_sd"])
      bmax<-b+1.96*exp(as.numeric(indivParamsreg[1,"beta_mode"]))*as.numeric(indivParamsreg[1,"b1_sd"])
    }else{
      b<-as.numeric(indivParamsreg[1,"b1_mode"])
      bmin<-as.numeric(indivParamsreg[1,"b1_mode"])-1.96*as.numeric(indivParamsreg[1,"b1_sd"])
      bmax<-as.numeric(indivParamsreg[1,"b1_mode"])+1.96*as.numeric(indivParamsreg[1,"b1_sd"])
    }

    Aminmax<-solution$solution[which(solution$solution$time==time),"A"]-1.96*sqrt(solution$solution[which(solution$solution$time==time),"A"])
    Amaxmax<-solution$solution[which(solution$solution$time==time),"A"]+1.96*sqrt(solution$solution[which(solution$solution$time==time),"A"])

    Iminmax<-solution$solution[which(solution$solution$time==time),"I"]-1.96*sqrt(solution$solution[which(solution$solution$time==time),"I"])
    Imaxmax<-solution$solution[which(solution$solution$time==time),"I"]+1.96*sqrt(solution$solution[which(solution$solution$time==time),"I"])

    It<-solution$solution[which(solution$solution$time==time),"I"]

    At<-solution$solution[which(solution$solution$time==time),"A"]


    R0minmax<-Di*bmin/(Amaxmax+Imaxmax)*(alpha*Aminmax+(Dqmin*Iminmax)/(Di+Dqmax))
    R0maxmax<-Di*bmax/(Aminmax+Iminmax)*(alpha*Amaxmax+(Dqmax*Imaxmax)/(Di+Dqmin))
    R0<-Di*b/(It+At)*(alpha*At+Dq*It/(Di+Dq))
    this<-(alpha*At+Dq*It/(Di+Dq))
    res[time,]<-c(as.character(indivParamsreg[1,1]),as.character(datestart),time,R0,R0minmax,R0maxmax,It,Iminmax,Imaxmax,At,Aminmax,Amaxmax,this)
  }
  return(res)
}


### GET THE R0
#indivParamsreg<-indivParams[i,]
getPlotR0<-function(res,nameproject,indivParamsreg){
  jpeg(paste(path,"outputMonolix/",nameproject,"/graphics/R0_",as.character(indivParamsreg[1,1]),".jpg",sep=""))
  p<-ggplot(res, aes(x=as.numeric(time))) +
    geom_line(aes(y = as.numeric(R0)),col='black') +
    geom_ribbon(data=res,aes(ymin=as.numeric(R0ICmin),ymax=as.numeric(R0ICmax)),col="white",alpha=0.3, fill = "blue") +
    theme_classic() +
    ylim(0, max(c(as.numeric(res$R0),as.numeric(res$R0ICmin),as.numeric(res$R0ICmax)))) + xlim(0, 60) +
    ylab("Effective Reproductive Number")+
    xlab("Time")
  print(p)
  dev.off()
}

getPlotR0all <- function(R0table,nameproject){
  old.loc <- Sys.getlocale("LC_TIME")
  Sys.setlocale("LC_TIME", "en_GB.UTF-8")
  p <- plotR0all(R0table,nameproject)
  ggsave(plot=p, filename = paste0(path,"outputMonolix/", nameproject,"/graphics/R0_all.jpg"),
         device = "jpeg", dpi = 300, width=7, height=5.2)
  Sys.setlocale("LC_TIME",old.loc)
}

plotR0all <- function(R0table,nameproject){
  R0tableFRANCE<- data.frame(time=seq(as.Date("2020-03-11"), as.Date("2020-06-19"), "day"))
  for (i in 1:length(R0tableFRANCE$time)){
    R0tableFRANCE$A[i]<-sum(as.numeric(R0table$A[which((as.Date(R0table$date)+as.numeric(R0table$time))==as.Date(R0tableFRANCE$time[i])   )]))
    R0tableFRANCE$I[i]<-sum(as.numeric(R0table$I[which((as.Date(R0table$date)+as.numeric(R0table$time))==as.Date(R0tableFRANCE$time[i])   )]))
    R0tableFRANCE$Amin[i]<-sum(as.numeric(R0table$Amin[which((as.Date(R0table$date)+as.numeric(R0table$time))==as.Date(R0tableFRANCE$time[i])   )]))
    R0tableFRANCE$Imin[i]<-sum(as.numeric(R0table$Imin[which((as.Date(R0table$date)+as.numeric(R0table$time))==as.Date(R0tableFRANCE$time[i])   )]))
    R0tableFRANCE$Amax[i]<-sum(as.numeric(R0table$Amax[which((as.Date(R0table$date)+as.numeric(R0table$time))==as.Date(R0tableFRANCE$time[i])   )]))
    R0tableFRANCE$Imax[i]<-sum(as.numeric(R0table$Imax[which((as.Date(R0table$date)+as.numeric(R0table$time))==as.Date(R0tableFRANCE$time[i])   )]))
  }
  R0table$finaltime<-as.Date(R0table$date)+as.numeric(R0table$time)
  pop <- read.table("./Monolix_final/outputMonolix/final/populationParameters.txt",
                    sep=",",header=TRUE)

  b1<-pop$value[pop$parameter=="b1_pop"]
  b2<-b1/exp(-pop$value[pop$parameter=="beta_pop"])
  R0tableFRANCE$R0<-ifelse(as.Date(R0tableFRANCE$time)<as.Date("2020-03-17"),b1*Di/(R0tableFRANCE$A+R0tableFRANCE$I)*(alpha*R0tableFRANCE$A+pop$value[pop$parameter=="Dq_pop"]*R0tableFRANCE$I/(pop$value[pop$parameter=="Dq_pop"]+Di)),b2*Di/(R0tableFRANCE$A+R0tableFRANCE$I)*(alpha*R0tableFRANCE$A+pop$value[pop$parameter=="Dq_pop"]*R0tableFRANCE$I/(pop$value[pop$parameter=="Dq_pop"]+Di)))

  R0table$Region <- R0table$reg
  R0table$Region <- forcats::fct_recode(R0table$Region,
                                        "Île-de-France"="IDF",
                                        "Nouvelle Aquitaine" = "NAquitaine",
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

  p <- ggplot(R0table, aes(x=as.Date(finaltime))) +
    geom_line(aes(y = as.numeric(R0),color=Region, linetype="Region-wise value")) +
    geom_ribbon(data=R0table,aes(ymin=as.numeric(R0ICmin),ymax=as.numeric(R0ICmax),fill=Region, alpha="Confidence bound")) +
    geom_line(data=R0tableFRANCE,aes(x=time, y = as.numeric(R0), linetype="France\nnational average")) +
    scale_linetype_manual("", values = c(2, 1)) +
    scale_alpha_manual("", values = c(0.3)) +
    guides(color="none", fill="none", alpha=guide_legend(override.aes = list(fill="black", linetype=1))) +
    theme_bw() +
    facet_wrap(~Region, ncol = 3) +
    theme(strip.background = element_rect(fill="white")) +
    xlim(as.Date("2020-03-01"),as.Date("2020-05-01"))+
    ylab(expression(paste("Effective Reproductive Number ", R[0](t)))) +
    #ylim(1,5) +
    #ylim(0, max(c(as.numeric(R0table$R0),as.numeric(R0table$R0ICmin),as.numeric(R0table$R0ICmax))))
    xlab("Date") +
    theme(axis.text.x = element_text(angle=45, hjust=1)) +
    theme(panel.grid.minor = element_blank()) +
    theme(legend.position = "bottom" ) +
    NULL

  return(p)
}


###### GET INDICATOR TABLE
getindicators<-function(indivParams){
  indivParamsprint<-indivParams
  indivParamsprint$b1_mode<-round(indivParamsprint$b1_mode,2)
  indivParamsprint$b1_modemin<-round(indivParamsprint$b1_mode-1.96*indivParamsprint$b1_sd,2)
  indivParamsprint$b1_modemmax<-round(indivParamsprint$b1_mode+1.96*indivParamsprint$b1_sd,2)
  indivParamsprint$b1summary<-paste(indivParamsprint$b1_mode," [",indivParamsprint$b1_modemin,"; ",indivParamsprint$b1_modemmax,"]",sep="")

  indivParamsprint$Dq_mode<-round(indivParamsprint$Dq_mode,2)
  indivParamsprint$Dq_modemin<-round(indivParamsprint$Dq_mode-1.96*indivParamsprint$Dq_sd,2)
  indivParamsprint$Dq_modemmax<-round(indivParamsprint$Dq_mode+1.96*indivParamsprint$Dq_sd,2)
  indivParamsprint$Dqsummary<-paste(indivParamsprint$Dq_mode," [",indivParamsprint$Dq_modemin,"; ",indivParamsprint$Dq_modemmax,"]",sep="")

  indivParamsprint$E0_mode<-round(indivParamsprint$E0_mode,0)
  indivParamsprint$E0_modemin<-round(indivParamsprint$E0_mode-1.96*indivParamsprint$E0_sd,0)
  indivParamsprint$E0_modemmax<-round(indivParamsprint$E0_mode+1.96*indivParamsprint$E0_sd,0)
  indivParamsprint$E0summary<-paste(indivParamsprint$E0_mode," [",indivParamsprint$E0_modemin,"; ",indivParamsprint$E0_modemmax,"]",sep="")

  indivParamsprint$A0_mode<-round(indivParamsprint$A0_mode,0)
  indivParamsprint$A0_modemin<-round(indivParamsprint$A0_mode-1.96*indivParamsprint$A0_sd,0)
  indivParamsprint$A0_modemmax<-round(indivParamsprint$A0_mode+1.96*indivParamsprint$A0_sd,0)
  indivParamsprint$A0summary<-paste(indivParamsprint$A0_mode," [",indivParamsprint$A0_modemin,"; ",indivParamsprint$A0_modemmax,"]",sep="")

  indivParamsprint$R0<-round(as.numeric(indivParamsprint$R0),1)
  indivParamsprint$R0min<-round(as.numeric(indivParamsprint$R0min),1)
  indivParamsprint$R0max<-round(as.numeric(indivParamsprint$R0max),1)
  indivParamsprint$R0conf<-round(as.numeric(indivParamsprint$R0conf),1)
  indivParamsprint$R0minconf<-round(as.numeric(indivParamsprint$R0minconf),1)
  indivParamsprint$R0maxconf<-round(as.numeric(indivParamsprint$R0maxconf),1)
  indivParamsprint$R0summary<-paste(indivParamsprint$R0," [",indivParamsprint$R0min,"; ",indivParamsprint$R0max,"]",sep="")
  indivParamsprint$R0confsummary<-paste(indivParamsprint$R0conf," [",indivParamsprint$R0minconf,"; ",indivParamsprint$R0maxconf,"]",sep="")

  print(xtable(indivParamsprint[,c("id","b1summary","Dqsummary","E0summary","A0summary","R0summary","R0confsummary")]))

  print(xtable(indivParamsprint[,c("id","timestart","Icumul","Hcumul","popsize","r_sent")]))

}

getIHD<-function(solution,indivParamsreg){

  res<-as.data.frame(matrix(NA,ncol=17,nrow=0))
  names(res)<-c("reg","i","time","Iincident","Iincidentmin","Iincidentmax","Hincident","Hincidentmin","Hincidentmax","Dincident","Dincidentmin","Dincidentmax","ICUincident","ICUincidentmin","ICUincidentmax","infected","immunised")

  timeinterest<-unique(solution$data$day[which(as.character(solution$data$date)=="2020-03-11")])

  tauxICU=6662/(27432+6662) #Au 3/4/2020
  tauxICUmax=6662/(27432+6662)#Au 3/4/2020
  tauxICUmin=6662/(27432+6662)#Au 3/4/2020
  tauxDEATH=5091/64338#Au 3/4/2020
  tauxDEATHmax=5091/64338#Au 3/4/2020
  tauxDEATHmin=5091/64338#Au 3/4/2020

  for (i in (0:100)){
    thistime<-timeinterest+i

    immunised<-solution$solution$R[which(solution$solution$time==thistime)]

    infected<-solution$solution$E[which(solution$solution$time==thistime)]+solution$solution$I[which(solution$solution$time==thistime)]+solution$solution$A[which(solution$solution$time==thistime)]+solution$solution$H[which(solution$solution$time==thistime)]+solution$solution$R[which(solution$solution$time==thistime)]

    Iincident<-solution$solution$I[which(solution$solution$time==thistime)]+solution$solution$H[which(solution$solution$time==thistime)]+indivParamsreg[1,"r_sent"]*((1-tauxDEATH)*solution$solution$R[which(solution$solution$time==thistime)])
    Iincidentmax<-Iincident+1.96*(sqrt(solution$solution$I[which(solution$solution$time==thistime)]+solution$solution$H[which(solution$solution$time==thistime)]+(indivParamsreg[1,"r_sent"]*(1-tauxDEATHmin))**2*solution$solution$R[which(solution$solution$time==thistime)]))
    Iincidentmin<-Iincident-1.96*(sqrt(solution$solution$I[which(solution$solution$time==thistime)]+solution$solution$H[which(solution$solution$time==thistime)]+(indivParamsreg[1,"r_sent"]*(1-tauxDEATHmin))**2*solution$solution$R[which(solution$solution$time==thistime)]))

    Hincident<-solution$solution$H[which(solution$solution$time==thistime)]
    Hincidentmin<-Hincident-1.96*sqrt(Hincident)
    Hincidentmax<-Hincident+1.96*sqrt(Hincident)


    Dincident<-indivParamsreg[1,"r_sent"]*tauxDEATH*solution$solution$R[which(solution$solution$time==thistime)]
    Dincidentmin<-Dincident-1.96*indivParamsreg[1,"r_sent"]*tauxDEATHmin*sqrt(Dincident)
    Dincidentmax<-Dincident+1.96*indivParamsreg[1,"r_sent"]*tauxDEATHmax*sqrt(Dincident)


    ICUincident<-solution$solution$H[which(solution$solution$time==thistime)]*tauxICU
    ICUincidentmin<-ICUincident-1.96*tauxICUmin*sqrt(ICUincident)
    ICUincidentmax<-ICUincident+1.96*tauxICUmax*sqrt(ICUincident)

    res[i+1,]<-c(as.character(indivParamsreg[1,1]), i,thistime, Iincident,Iincidentmin,Iincidentmax,Hincident,Hincidentmin,Hincidentmax,Dincident,Dincidentmin,Dincidentmax,ICUincident,ICUincidentmin,ICUincidentmax,infected,immunised)

  }

  return(res)
}




#### GET % ASYMPTOMATIQUE
#indivParamsreg<-indivParams[1,]

getAsymptomatique<-function(solution,indivParamsreg){

  De<-solution$parameters$De
  r<-indivParamsreg[1,"ascertainment_mode"]
  sentinelle<-read.table("./data/sentinelle.txt",header=TRUE)
  Sent<-sentinelle$I12[which(sentinelle$names==as.character(solution$data$reg_id[1]))]/7
  Sentmax<-sentinelle$I12max[which(sentinelle$names==as.character(solution$data$reg_id[1]))]/7
  Sentmin<-sentinelle$I12min[which(sentinelle$names==as.character(solution$data$reg_id[1]))]/7

  dayS2<-solution$data$day[which((as.character(solution$data$date)%in%c("2020-03-16","2020-03-17","2020-03-18","2020-03-19","2020-03-20","2020-03-22","2020-03-23"))&(solution$data$obs_id==1))]
  Eaverage_S12<-mean(solution$solution$E[which(solution$solution$time%in%dayS2)])

  pctAss<-(Sent-r*Eaverage_S12)/((1-r)*Eaverage_S12)

  #
  # incidentI<-r*Eaverage_S12
  # incidentA<-(1-r)*Eaverage_S12
  # Iaverage_S12<-mean(solution$solution$I[which(solution$solution$time%in%dayS2)])
  # Aaverage_S12<-mean(solution$solution$A[which(solution$solution$time%in%dayS2)])
  # Iaverage_S12+Aaverage_S12
  #
  # Eaverage_S12*(1-r)/De
  # Eaverage_S12*(r)/De
  #
  #
  # NT<-Sent-r*Eaverage_S12/De
  # NTplusAS<-incidentA
  #
  # pctAss<-(1-NT/Eaverage_S12)
  return(pctAss)
}

# sd(indivParams$r_sent)
#
# ((1-indivParams$ascertainment_mode)-(indivParams$ascertainment_mode*indivParams$r_sent)/(1-indivParams$r_sent))/5.2
#
# mean(((1-indivParams$ascertainment_mode)-(indivParams$ascertainment_mode*indivParams$r_sent)/(1-indivParams$r_sent))/5.2)
# sd(((1-indivParams$ascertainment_mode)-(indivParams$ascertainment_mode*indivParams$r_sent)/(1-indivParams$r_sent))/5.2)
