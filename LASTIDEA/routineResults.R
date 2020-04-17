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
                      A0given,
                      b2,b3,
                      tconf,typecov,
                      lengthconf=1000,
                      newdailyMove=0,
                      pred=FALSE,bsd,Dqsd,E0sd,A0sd,betasd,beta2sd,CI=TRUE, ncores=1){


  # i=2
  # initwithdata=TRUE
  # binit=c(b,r)
  # data=dataregion
  # dailyMove=0
  # timeconf=tconf
  # lengthconf=1000
  # newdailyMove=0
  # factorreductrans=NULL
  # verbose = TRUE
  # optim_ols=FALSE
  # obs="2Y"
  # pred=FALSE
  # ncores=1

  temp_monolix_estim<-seirah_estim(binit=c(b,r),
                                   data=dataregion,
                                   alpha=alpha,
                                   De=De,
                                   Di=Di,
                                   Dq=Dq,
                                   Dh=Dh,
                                   popSize=popSize,
                                   dailyMove=0,
                                   timeconf=tconf,
                                   lengthconf=lengthconf,
                                   newdailyMove=newdailyMove,
                                   verbose = TRUE,
                                   optim_ols=FALSE,
                                   obs="2Y",
                                   E0given=E0given,
                                   A0given=A0given,
                                   b2=b2,
                                   pred=pred,typecov=typecov,b3=b3)

  if(CI){
    temp<-seirah_confidence_interval_response(b,bsd,Dq,Dqsd,E0given,E0sd,A0given,A0sd,b2,betasd,b3,beta2sd,De,Di,Dh,r,alpha,0,dataregion$init_I0[1],dataregion$init_H0[1],popSize,newdailyMove,0,1000,tconf,lengthconf,typecov, ncores)
   
    #  temp_monolix_estim$solution$Smin<-pmax(0,temp$Smin)
    # temp_monolix_estim$solution$Emin<-pmax(0,temp$Emin)
    # temp_monolix_estim$solution$Imin<-pmax(0,temp$Imin)
    # temp_monolix_estim$solution$Rmin<-pmax(0,temp$Rmin)
    # temp_monolix_estim$solution$Amin<-pmax(0,temp$Amin)
    # temp_monolix_estim$solution$Hmin<-pmax(0,temp$Hmin)
    # 
    # temp_monolix_estim$solution$Smax<-pmax(0,temp$Smax)
    # temp_monolix_estim$solution$Emax<-pmax(0,temp$Emax)
    # temp_monolix_estim$solution$Imax<-pmax(0,temp$Imax)
    # temp_monolix_estim$solution$Rmax<-pmax(0,temp$Rmax)
    # temp_monolix_estim$solution$Amax<-pmax(0,temp$Amax)
    # temp_monolix_estim$solution$Hmax<-pmax(0,temp$Hmax)


     temp_monolix_estim$solution$Smin<-pmax(0,temp$Smin-1.96*sqrt(pmax(0,temp_monolix_estim$solution$S)))
     temp_monolix_estim$solution$Emin<-pmax(0,temp$Emin-1.96*sqrt(pmax(0,temp_monolix_estim$solution$E)))
     temp_monolix_estim$solution$Imin<-pmax(0,temp$Imin-1.96*sqrt(pmax(0,temp_monolix_estim$solution$I)))
     temp_monolix_estim$solution$Rmin<-pmax(0,temp$Rmin-1.96*sqrt(pmax(0,temp_monolix_estim$solution$R)))
     temp_monolix_estim$solution$Amin<-pmax(0,temp$Amin-1.96*sqrt(pmax(0,temp_monolix_estim$solution$A)))
     temp_monolix_estim$solution$Hmin<-pmax(0,temp$Hmin-1.96*sqrt(pmax(0,temp_monolix_estim$solution$H)))
    
     temp_monolix_estim$solution$Smax<-pmax(0,temp$Smax+1.96*sqrt(pmax(0,temp_monolix_estim$solution$S)))
     temp_monolix_estim$solution$Emax<-pmax(0,temp$Emax+1.96*sqrt(pmax(0,temp_monolix_estim$solution$E)))
     temp_monolix_estim$solution$Imax<-pmax(0,temp$Imax+1.96*sqrt(pmax(0,temp_monolix_estim$solution$I)))
     temp_monolix_estim$solution$Rmax<-pmax(0,temp$Rmax+1.96*sqrt(pmax(0,temp_monolix_estim$solution$R)))
     temp_monolix_estim$solution$Amax<-pmax(0,temp$Amax+1.96*sqrt(pmax(0,temp_monolix_estim$solution$A)))
     temp_monolix_estim$solution$Hmax<-pmax(0,temp$Hmax+1.96*sqrt(pmax(0,temp_monolix_estim$solution$H)))
  }


  # temp_monolix_estim$solution$Smin<-pmax(0,temp$Smin-1.96*sqrt(pmax(0,temp_monolix_estim$solution$S)))
  #  temp_monolix_estim$solution$Emin<-pmax(0,temp$Emin-1.96*sqrt(pmax(0,temp_monolix_estim$solution$E)))
  #  temp_monolix_estim$solution$Imin<-pmax(0,temp$Imin-1.96*sqrt(pmax(0,temp_monolix_estim$solution$I)))
  #  temp_monolix_estim$solution$Rmin<-pmax(0,temp$Rmin-1.96*sqrt(pmax(0,temp_monolix_estim$solution$R)))
  #  temp_monolix_estim$solution$Amin<-pmax(0,temp$Amin-1.96*sqrt(pmax(0,temp_monolix_estim$solution$A)))
  #  temp_monolix_estim$solution$Hmin<-pmax(0,temp$Hmin-1.96*sqrt(pmax(0,temp_monolix_estim$solution$H)))

  #  temp_monolix_estim$solution$Smax<-pmax(0,temp$Smax+1.96*sqrt(pmax(0,temp_monolix_estim$solution$S)))
  #  temp_monolix_estim$solution$Emax<-pmax(0,temp$Emax+1.96*sqrt(pmax(0,temp_monolix_estim$solution$E)))
  #  temp_monolix_estim$solution$Imax<-pmax(0,temp$Imax+1.96*sqrt(pmax(0,temp_monolix_estim$solution$I)))
  #  temp_monolix_estim$solution$Rmax<-pmax(0,temp$Rmax+1.96*sqrt(pmax(0,temp_monolix_estim$solution$R)))
  #  temp_monolix_estim$solution$Amax<-pmax(0,temp$Amax+1.96*sqrt(pmax(0,temp_monolix_estim$solution$A)))
  #  temp_monolix_estim$solution$Hmax<-pmax(0,temp$Hmax+1.96*sqrt(pmax(0,temp_monolix_estim$solution$H)))

  return(temp_monolix_estim)

}




#### PLOT THE SOLUTION
getPlot<-function(temp_monolix_estim,nameproject,indivParamsreg,path){
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
#indivParamsregUP<-indivParamsUP[i,]
getR0<-function(solution,indivParamsreg,typecov,timings,indivParamsregUP,solutionUPDATED){

  res<-as.data.frame(matrix(NA,ncol=12,nrow=0))
  names(res)<-c("reg","date","time","R0","R0ICmin","R0ICmax","I","Imin","Imax","A","Amin","Amax")


  datestart<-solution$data$date[1]
  Dq<-as.numeric(indivParamsreg[1,"Dq_mode"])
  alpha<-solution$parameters$alpha
  Di<-solution$parameters$Di

  Dqmin<-as.numeric(indivParamsreg[1,"Dq_mode"])-1.96*as.numeric(indivParamsreg[1,"Dq_sd"])
  Dqmax<-as.numeric(indivParamsreg[1,"Dq_mode"])+1.96*as.numeric(indivParamsreg[1,"Dq_sd"])

  for (time in 1:365){
    if (typecov=="constant"){
      if((time>=solution$parameters$timeconf)&(time<(solution$parameters$timeconf+8))){
        b<-exp(log(as.numeric(indivParamsreg[1,"b1_mode"]))+as.numeric(indivParamsreg[1,"betat1_mode"]))
        bmin<-exp(log(as.numeric(indivParamsreg[1,"b1_mode"]))+as.numeric(indivParamsreg[1,"betat1_mode"])-1.96*sqrt((as.numeric(indivParamsreg[1,"b1_sd"])/as.numeric(indivParamsreg[1,"b1_mode"]))**2+as.numeric(indivParamsreg[1,"betat1_sd"])**2))
        bmax<-exp(log(as.numeric(indivParamsreg[1,"b1_mode"]))+as.numeric(indivParamsreg[1,"betat1_mode"])+1.96*sqrt((as.numeric(indivParamsreg[1,"b1_sd"])/as.numeric(indivParamsreg[1,"b1_mode"]))**2+as.numeric(indivParamsreg[1,"betat1_sd"])**2))
      }else{
        if((time>=solution$parameters$timeconf+8)&(time<(solution$parameters$timeconf+solution$parameters$lengthconf))){
          b<-exp(log(as.numeric(indivParamsreg[1,"b1_mode"]))+as.numeric(indivParamsreg[1,"betat1_mode"])+as.numeric(indivParamsregUP[1,"betat2_mode"]))
          bmin<-exp(log(as.numeric(indivParamsreg[1,"b1_mode"]))+as.numeric(indivParamsreg[1,"betat1_mode"])+as.numeric(indivParamsregUP[1,"betat2_mode"])-1.96*sqrt((as.numeric(indivParamsreg[1,"b1_sd"])/as.numeric(indivParamsreg[1,"b1_mode"]))**2+as.numeric(indivParamsreg[1,"betat1_sd"])**2+as.numeric(indivParamsregUP[1,"betat2_sd"])**2))
          bmax<-exp(log(as.numeric(indivParamsreg[1,"b1_mode"]))+as.numeric(indivParamsreg[1,"betat1_mode"])+as.numeric(indivParamsregUP[1,"betat2_mode"])+1.96*sqrt((as.numeric(indivParamsreg[1,"b1_sd"])/as.numeric(indivParamsreg[1,"b1_mode"]))**2+as.numeric(indivParamsreg[1,"betat1_sd"])**2+as.numeric(indivParamsregUP[1,"betat2_sd"])**2))
        }else{
          b<-as.numeric(indivParamsreg[1,"b1_mode"])
          bmin<-as.numeric(indivParamsreg[1,"b1_mode"])-1.96*as.numeric(indivParamsreg[1,"b1_sd"])
          bmax<-as.numeric(indivParamsreg[1,"b1_mode"])+1.96*as.numeric(indivParamsreg[1,"b1_sd"])
        }
    }
    }

   
    if((time>=solution$parameters$timeconf+8)){
      Aminmax<-solutionUPDATED$solution[which(solution$solution$time==time),"Amin"]
      Amaxmax<-solutionUPDATED$solution[which(solution$solution$time==time),"Amax"]
      
      Iminmax<-solutionUPDATED$solution[which(solution$solution$time==time),"Imin"]
      Imaxmax<-solutionUPDATED$solution[which(solution$solution$time==time),"Imax"]
      
      It<-solutionUPDATED$solution[which(solution$solution$time==time),"I"]
      
      At<-solutionUPDATED$solution[which(solution$solution$time==time),"A"]
    }else{
      Aminmax<-solution$solution[which(solution$solution$time==time),"Amin"]
      Amaxmax<-solution$solution[which(solution$solution$time==time),"Amax"]
      
      Iminmax<-solution$solution[which(solution$solution$time==time),"Imin"]
      Imaxmax<-solution$solution[which(solution$solution$time==time),"Imax"]
      
      It<-solution$solution[which(solution$solution$time==time),"I"]
      
      At<-solution$solution[which(solution$solution$time==time),"A"]
    }



    R0minmax<-max(0,Di*bmin/(Amaxmax+Imaxmax)*(alpha*Aminmax+(Dqmin*Iminmax)/(Di+Dqmax)))
    R0maxmax<-max(0,Di*bmax/(Aminmax+Iminmax)*(alpha*Amaxmax+(Dqmax*Imaxmax)/(Di+Dqmin)))
    R0<-Di*b/(It+At)*(alpha*At+Dq*It/(Di+Dq))
    

    res[time,]<-c(as.character(indivParamsreg[1,1]),as.character(datestart),time,R0,R0minmax,R0maxmax,It,Iminmax,Imaxmax,At,Aminmax,Amaxmax)
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
    geom_hline(yintercept = 1)+
    theme_classic() +
    ylim(0, max(c(as.numeric(res$R0[1:50]),as.numeric(res$R0ICmin[1:50]),as.numeric(res$R0ICmax[1:50])))) + xlim(0, 50) +
    ylab("Effective Reproductive Number")+
    xlab("Time")
  print(p)
  dev.off()
}






full_region_names <- function(x){
  forcats::fct_recode(x,
                      "Île-de-France"="IDF",
                      "Nouvelle-Aquitaine" = "NAquitaine",
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
}




plotSolutionAll <- function(solutions_list, nameproject, log_scale=FALSE, prop_geo=FALSE){
  library(dplyr)
  library(patchwork)
  sol_est_list <- lapply(solutions_list,
                         function(x){
                           sol_obstime <- x$solution[which(x$solution[,"time"] %in% x$data$day), ]
                           sol_obstime$day<-sol_obstime$time
                           sol_obstime$.Hmodest<-sol_obstime$I/x$parameters$Dq
                           sol_obstime$.Imodest<-x$parameters$ascertainment*sol_obstime$E/x$parameters$De
                           sol_obstime$.Hmodestmin<-sol_obstime$Imin/x$parameters$Dq
                           sol_obstime$.Hmodestmax<-sol_obstime$Imax/x$parameters$Dq
                           sol_obstime$.Imodestmin<-x$parameters$ascertainment*sol_obstime$Emin/x$parameters$De
                           sol_obstime$.Imodestmax<-x$parameters$ascertainment*sol_obstime$Emax/x$parameters$De
                           sol_obstime$IDname<-sol_obstime$reg
                           return(sol_obstime)
                         })

  data_list <- lapply(solutions_list, "[[", "data")
  all_data_df <- do.call(rbind.data.frame, data_list)
  all_fit_df <- do.call(rbind.data.frame, sol_est_list) %>%
    select(-time, -reg)

  all_fit_df_meltlist <- lapply(c("S", "E", "I", "R", "A", "H", ".Hmodest", ".Imodest"), FUN=function(comp){
    all_fit_df %>% select(IDname, date, day, starts_with(all_of(comp))) %>%
      mutate(obs_id = as.character(comp)) %>%
      rename(obs = comp,
             obs_min = glue::glue({comp}, "min"),
             obs_max = glue::glue({comp}, "max"))
  })
  all_fit_df_2plot <- do.call(rbind.data.frame, all_fit_df_meltlist)

  all_data_df$obs_id <- forcats::fct_recode(factor(all_data_df$obs_id),
                                            "Incident confirmed cases" = "1",
                                            "Incident hospitalized cases" = "2"
  )
  all_fit_df_2plot$obs_id <- forcats::fct_recode(factor(all_fit_df_2plot$obs_id),
                                                 "Incident confirmed cases" = ".Imodest",
                                                 "Incident hospitalized cases" = ".Hmodest"
  )
  all_data_df$IDname <- full_region_names(all_data_df$IDname)
  all_fit_df_2plot$IDname <- full_region_names(all_fit_df_2plot$IDname)


  if(prop_geo){
    all_fit_df_2plot$pop_size <- popreg[match(all_fit_df_2plot$IDname, popreg$maille_nom), "population"]
    all_data_df$pop_size <- popreg[match(all_data_df$IDname, popreg$maille_nom), "population"]
    all_data_df <- all_data_df %>% mutate(obs = obs/pop_size)
    all_fit_df_2plot <- all_fit_df_2plot %>% mutate(obs = obs/pop_size,
                                                obs_min = obs_min/pop_size,
                                                obs_max = obs_max/pop_size)
    ylabel <- "Incidence rate"
    baseline <- NULL

  }else{
    ylabel <- "Incidence number"
    baseline <- geom_hline(yintercept = 1)
  }

  #adding the max value all the time to ensure that scales match
  Imax_obs <- max(all_data_df %>% filter(obs_id == "Incident confirmed cases") %>% pull(obs))
  Imax_sim <- max(all_fit_df_2plot %>% filter(obs_id == "Incident confirmed cases") %>% pull(obs))
  Hmax_obs <- max(all_data_df %>% filter(obs_id == "Incident hospitalized cases") %>% pull(obs))
  Hmax_sim <- max(all_fit_df_2plot %>% filter(obs_id == "Incident hospitalized cases") %>% pull(obs))

  dataObs2plot_1 <- all_data_df %>% filter(IDname %in% levels(all_data_df$IDname)[1:6])
  dataObs2plot_2 <- all_data_df %>% filter(IDname %in% levels(all_data_df$IDname)[7:12])
  dataSim2plot_1 <- all_fit_df_2plot %>% filter(obs_id %in% c("Incident confirmed cases", "Incident hospitalized cases"),
                                                IDname %in% levels(all_data_df$IDname)[1:6]) %>% select(date, IDname, obs_id, obs, obs_min, obs_max)
  dataSim2plot_2 <- all_fit_df_2plot %>% filter(obs_id %in% c("Incident confirmed cases", "Incident hospitalized cases"),
                                                IDname %in% levels(all_data_df$IDname)[7:12]) %>% select(date, IDname, obs_id, obs, obs_min, obs_max)
  dataObs2plot_1$show <- TRUE
  dataObs2plot_1 <- rbind.data.frame((dataObs2plot_1 %>% filter(obs_id == "Incident confirmed cases"))[1,],
                                     (dataObs2plot_1 %>% filter(obs_id == "Incident hospitalized cases"))[1,],
                                     dataObs2plot_1)
  dataObs2plot_1[1:2, "obs"] <- c(max(Imax_obs, Imax_sim), max(Hmax_obs, Hmax_sim))
  dataObs2plot_1[1:2, "show"] <- rep(FALSE, 2)
  dataObs2plot_2$show <- TRUE
  dataObs2plot_2 <- rbind.data.frame((dataObs2plot_2 %>% filter(obs_id == "Incident confirmed cases"))[1,],
                                     (dataObs2plot_2 %>% filter(obs_id == "Incident hospitalized cases"))[1,],
                                     dataObs2plot_2)
  dataObs2plot_2[1:2, "obs"] <- c(max(Imax_obs, Imax_sim), max(Hmax_obs, Hmax_sim))
  dataObs2plot_2[1:2, "show"] <- rep(FALSE, 2)

  for(i in unique(dataSim2plot_1$IDname)){
    for (o in unique(dataSim2plot_2$obs_id)){
      date_obs_min <- dataObs2plot_1 %>% filter(IDname==i, obs_id==o) %>% pull(date) %>% min()
      dataSim2plot_1 <-  dataSim2plot_1 %>% filter(!(IDname==i & date < date_obs_min & obs_id==o))
    }
  }
  for(i in unique(dataSim2plot_2$IDname)){
    for (o in unique(dataSim2plot_2$obs_id)){
      date_obs_min <- dataObs2plot_2 %>% filter(IDname==i, obs_id==o) %>% pull(date) %>% min()
      dataSim2plot_2 <-  dataSim2plot_2 %>% filter(!(IDname==i & date<date_obs_min & obs_id==o))
    }
  }


  p1 <- ggplot(dataObs2plot_1, aes(x=date, y=obs, group=IDname)) +
    baseline +
    geom_point(aes(color="Observed", shape=show)) +
    geom_line(data = dataSim2plot_1,
              aes(linetype="Estimate"), color="red3") +
    geom_ribbon(data = dataSim2plot_1,
                aes(ymin = obs_min, ymax=obs_max, alpha="95% CI"), fill="red3")+
    scale_shape_manual(values=c(NA, 16)) +
    scale_alpha_manual(values=c(0.3)) +
    scale_color_manual(values="black") +
    facet_grid(obs_id~IDname, scales = "free_y") +
    theme_bw() +
    guides(color="none", linetype="none", shape="none", alpha="none") +
    theme(legend.position = "bottom") +
    ylab("") +
    xlab(NULL) +
    theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
    theme(strip.background = element_rect(fill="white"),
          strip.text = element_text(size=8)) +
    NULL
  p2 <- ggplot(dataObs2plot_2, aes(x=date, y=obs, group=IDname)) +
    baseline +
    geom_point(aes(color="Observed", shape=show)) +
    geom_line(data = dataSim2plot_2,
              aes(linetype="Estimate"), color="red3") +
    geom_ribbon(data = dataSim2plot_2,
                aes(ymin = obs_min, ymax=obs_max, alpha="95% CI"), fill="red3")+
    scale_shape_manual(values=c(NA, 16)) +
    scale_alpha_manual(values=c(0.3)) +
    scale_color_manual(values="black") +
    facet_grid(obs_id~IDname, scales = "free_y") +
    theme_bw() +
    guides(color=guide_legend(title=""), linetype=guide_legend(title=""),
           alpha=guide_legend(title=""), shape="none") +
    theme(legend.position = "bottom") +
    ylab(ylabel) +
    xlab("Date") +
    theme(axis.text.x = element_text(angle=45, hjust=1),
          axis.title.y = element_text(hjust=1.4)) +
    theme(strip.background = element_rect(fill="white"),
          strip.text = element_text(size=8)) +
    NULL
  if(!log_scale){
    plot_res <- p1/p2
  }else{
    plot_res <- (p1 + scale_y_log10())/(p2 + scale_y_log10())
  }

  return(plot_res)
}

getPlotSolutionAll <- function(solutions_list, nameproject, log_scale=FALSE, prop_geo = FALSE){

  if(log_scale){
    logindicator <- "_logscale"
  }else{
    logindicator <- ""
  }

  if(prop_geo){
    rateindicator <- "_rate"
  }else{
    rateindicator <- ""
  }

  old.loc <- Sys.getlocale("LC_TIME")
  Sys.setlocale("LC_TIME", "en_GB.UTF-8")
  p <- plotSolutionAll(solutions_list,nameproject, log_scale, prop_geo)
  ggsave(plot=p, filename = paste0(path,"outputMonolix/", nameproject,"/graphics/fit_all", logindicator, rateindicator, ".jpg"),
         device = "jpeg", dpi = 300, width=10, height=8)
  Sys.setlocale("LC_TIME",old.loc)
}



plotR0all <- function(R0table,nameproject,path,timingdays,typecov, Di, alpha,
                      facet_scales=c("fixed", "free_y", "free_x", "free")){

  if(length(facet_scales)>1){
    facet_scales <- facet_scales[1]
  }
  stopifnot(facet_scales %in% c("fixed", "free_y", "free_x", "free"))

  R0tableFRANCE<- data.frame(time=seq(as.Date("2020-03-11"), as.Date("2020-06-19"), "day"))
  for (i in 1:length(R0tableFRANCE$time)){
    R0tableFRANCE$A[i]<-sum(as.numeric(R0table$A[which((as.Date(R0table$date)+as.numeric(R0table$time))==as.Date(R0tableFRANCE$time[i])   )]))
    R0tableFRANCE$I[i]<-sum(as.numeric(R0table$I[which((as.Date(R0table$date)+as.numeric(R0table$time))==as.Date(R0tableFRANCE$time[i])   )]))
    R0tableFRANCE$Amin[i]<-sum(as.numeric(R0table$Amin[which((as.Date(R0table$date)+as.numeric(R0table$time))==as.Date(R0tableFRANCE$time[i])   )]))
    R0tableFRANCE$Imin[i]<-sum(as.numeric(R0table$Imin[which((as.Date(R0table$date)+as.numeric(R0table$time))==as.Date(R0tableFRANCE$time[i])   )]))
    R0tableFRANCE$Amax[i]<-sum(as.numeric(R0table$Amax[which((as.Date(R0table$date)+as.numeric(R0table$time))==as.Date(R0tableFRANCE$time[i])   )]))
    R0tableFRANCE$Imax[i]<-sum(as.numeric(R0table$Imax[which((as.Date(R0table$date)+as.numeric(R0table$time))==as.Date(R0tableFRANCE$time[i])   )]))
  }
  R0table$finaltime <- as.Date(R0table$date)+as.numeric(R0table$time)
  pop <- read.table(paste(path,"outputMonolix/", nameproject,"/populationParameters.txt",sep=""),
                    sep=",",header=TRUE)
  if(typecov=="constant"){
    b1 <- pop$value[pop$parameter=="b1_pop"]
    b2 <- pop$value[pop$parameter=="betat1_pop"]


    R0tableFRANCE$R0 <- ifelse(as.Date(R0tableFRANCE$time) < as.Date("2020-03-17"),
                               b1*Di/(R0tableFRANCE$A+R0tableFRANCE$I)*(alpha*R0tableFRANCE$A+pop$value[pop$parameter=="Dq_pop"]*R0tableFRANCE$I/(pop$value[pop$parameter=="Dq_pop"]+Di)),
                               b1*exp(b2)*Di/(R0tableFRANCE$A+R0tableFRANCE$I)*(alpha*R0tableFRANCE$A+pop$value[pop$parameter=="Dq_pop"]*R0tableFRANCE$I/(pop$value[pop$parameter=="Dq_pop"]+Di))
    )
  }
  if(typecov=="parametric"){
    b1<-pop$value[pop$parameter=="b1_pop"]
    b2<-pop$value[pop$parameter=="betat1_pop"]


    R0tableFRANCE$R0<-ifelse(as.Date(R0tableFRANCE$time)<as.Date("2020-03-17"),
                             b1*Di/(R0tableFRANCE$A+R0tableFRANCE$I)*(alpha*R0tableFRANCE$A+pop$value[pop$parameter=="Dq_pop"]*R0tableFRANCE$I/(pop$value[pop$parameter=="Dq_pop"]+Di)),
                             ifelse( as.Date(R0tableFRANCE$time)<(as.Date("2020-03-17")+timingdays),
                                     b1*exp(b2*as.numeric(R0tableFRANCE$time-as.Date("2020-03-17")))*Di/(R0tableFRANCE$A+R0tableFRANCE$I)*(alpha*R0tableFRANCE$A+pop$value[pop$parameter=="Dq_pop"]*R0tableFRANCE$I/(pop$value[pop$parameter=="Dq_pop"]+Di)),b1*exp(b2*(timingdays))*Di/(R0tableFRANCE$A+R0tableFRANCE$I)*(alpha*R0tableFRANCE$A+pop$value[pop$parameter=="Dq_pop"]*R0tableFRANCE$I/(pop$value[pop$parameter=="Dq_pop"]+Di))
                             ))
  }

  R0table$Region <- full_region_names(R0table$reg)
  R0table$date <- as.Date(R0table$date)
  R0table$finaltime <- as.Date(R0table$finaltime)

  R0table %>% group_by(reg) %>% top_n(-1, finaltime) %>% select(date, finaltime)

  p <- ggplot(R0table %>% filter(finaltime > as.Date("2020-03-01"), finaltime < as.Date("2020-03-25")),
              aes(x=finaltime)) +
    geom_hline(aes(yintercept = 1, alpha="France\nnational average"), linetype="dotted", color="grey35") +
    geom_hline(aes(yintercept = 1), linetype="dotted", color="grey35") +
    geom_line(aes(y = as.numeric(R0), linetype="Region-wise value\n(95% CI)", color="data up to\n2020-03-25")) +
    geom_line(data = R0table %>% filter(finaltime > as.Date("2020-03-01"), finaltime < as.Date("2020-04-06")),
              aes(y = as.numeric(R0UP), linetype="Region-wise value\n(95% CI)", color="data up to\n2020-04-06")) +
    geom_line(data=R0tableFRANCE %>% filter(time > as.Date("2020-03-01"), time < as.Date("2020-03-25")),
              aes(x=time, y = as.numeric(R0), linetype="France\nnational average")) +
    geom_ribbon(aes(ymin=as.numeric(R0ICmin),ymax=as.numeric(R0ICmax), fill="data up to\n2020-03-25",
                    alpha="Region-wise value\n(95% CI)")) +
    geom_ribbon(data = R0table %>% filter(finaltime > as.Date("2020-03-01"), finaltime < as.Date("2020-04-06")),
                aes(ymin=as.numeric(R0ICminUP),ymax=as.numeric(R0ICmaxUP), alpha="Region-wise value\n(95% CI)", fill="data up to\n2020-04-06")) +
    scale_linetype_manual("", values = c(2, 1)) +
    scale_alpha_manual("", values = c(0, 0.2)) +
    scale_fill_manual(" ", values=c("red3", "skyblue"))+
    scale_color_manual(" ", values=c("red3", "skyblue")) +
    #guides(linetype=guide_legend(override.aes = list(fill=c("white", "grey50")))) +
    theme_bw() +
    facet_wrap(~Region, ncol = 3, scales = facet_scales) +
    theme(strip.background = element_rect(fill="white")) +
    ylab(expression(paste("Effective Reproductive Number ", R[eff](t, xi)))) +
    ylim(0,NA) +
    #ylim(0, max(c(as.numeric(R0table$R0),as.numeric(R0table$R0ICmin),as.numeric(R0table$R0ICmax))))
    xlab("Date") +
    theme(axis.text.x = element_text(angle=45, hjust=1)) +
    theme(panel.grid.minor = element_blank()) +
    theme(legend.position = "bottom", legend.direction = "vertical") +
    NULL

  return(p)
}



getPlotR0all <- function(R0table, nameproject,path,timingdays,typecov, Di, alpha,
                         facet_scales=c("fixed", "free_y", "free_x", "free")){

  old.loc <- Sys.getlocale("LC_TIME")
  Sys.setlocale("LC_TIME", "en_GB.UTF-8")

  p <- plotR0all(R0table, nameproject,path,timingdays,typecov, Di, alpha, facet_scales)
  ggsave(plot=p, filename = paste0(path,"outputMonolix/", nameproject,"/graphics/R0_all.jpg"),
         device = "jpeg", dpi = 500, width=7, height=5.2)

  Sys.setlocale("LC_TIME",old.loc)
}

###### GET INDICATOR TABLE
getindicators<-function(indivParams){
  library(dplyr)
  indivParamsprint <- data.frame("id" = full_region_names(indivParams$id))

  indivParamsprint$b1_modemin <- format(round(indivParams$b1_mode-1.96*indivParams$b1_sd,2), nsmall = 2)
  indivParamsprint$b1_modemmax <- format(round(indivParams$b1_mode+1.96*indivParams$b1_sd,2), nsmall = 2)
  indivParamsprint$b1_mode <- format(round(indivParams$b1_mode,2), nsmall = 2)
  indivParamsprint$b1summary <- paste(indivParamsprint$b1_mode," [",indivParamsprint$b1_modemin,";",indivParamsprint$b1_modemmax,"]",sep="")

  indivParamsprint$Dq_modemin <- format(round(indivParams$Dq_mode-1.96*indivParams$Dq_sd,2), nsmall = 2)
  indivParamsprint$Dq_modemmax <- format(round(indivParams$Dq_mode+1.96*indivParams$Dq_sd,2), nsmall = 2)
  indivParamsprint$Dq_mode<- format(round(indivParams$Dq_mode,2), nsmall = 2)
  indivParamsprint$Dqsummary <- paste(indivParamsprint$Dq_mode," [",indivParamsprint$Dq_modemin,";",indivParamsprint$Dq_modemmax,"]",sep="")

  indivParamsprint$E0_modemin <- format(round(indivParams$E0_mode-1.96*indivParams$E0_sd,0), nsmall = 0, big.mark   = ",")
  indivParamsprint$E0_modemmax <- format(round(indivParams$E0_mode+1.96*indivParams$E0_sd,0), nsmall = 0, big.mark   = ",")
  indivParamsprint$E0_mode <- format(round(indivParams$E0_mode,0), nsmall = 0, big.mark   = ",")
  indivParamsprint$E0summary <- paste(indivParamsprint$E0_mode," [",indivParamsprint$E0_modemin,";",indivParamsprint$E0_modemmax,"]",sep="")

 # indivParamsprint$A0_modemin <- format(round(indivParams$A0_mode-1.96*indivParams$A0_sd,0), nsmall = 0, big.mark   = ",")
#  indivParamsprint$A0_modemmax <- format(round(indivParams$A0_mode+1.96*indivParams$A0_sd,0), nsmall = 0, big.mark   = ",")
 # indivParamsprint$A0_mode <- format(round(indivParams$A0_mode,0), nsmall = 0, big.mark   = ",")
#  indivParamsprint$A0summary <- paste(indivParamsprint$A0_mode," [",indivParamsprint$A0_modemin,";",indivParamsprint$A0_modemmax,"]",sep="")

  indivParamsprint$betat_modemin <- format(round(indivParams$betat1_mode-1.96*indivParams$betat1_sd, 2), nsmall = 2)
  indivParamsprint$betat_modemmax <- format(round(indivParams$betat1_mode+1.96*indivParams$betat1_sd, 2), nsmall = 2)
  indivParamsprint$betat_mode <- format(round(indivParams$betat1_mode, 2), nsmall = 2)
  indivParamsprint$betatsummary <- paste(indivParamsprint$betat_mode," [",indivParamsprint$betat_modemin,";",indivParamsprint$betat_modemmax,"]",sep="")


  indivParamsprint$R0 <- format(round(as.numeric(indivParams$R0),1), nsmall=1)
  indivParamsprint$R0min <- format(round(as.numeric(indivParams$R0min),1), nsmall=1)
  indivParamsprint$R0max <- format(round(as.numeric(indivParams$R0max),1), nsmall=1)
  indivParamsprint$R0conf <- format(round(as.numeric(indivParams$R0conf),1), nsmall=1)
  indivParamsprint$R0minconf <- format(round(as.numeric(indivParams$R0minconf),1), nsmall=1)
  indivParamsprint$R0maxconf <- format(round(as.numeric(indivParams$R0maxconf),1), nsmall=1)
  indivParamsprint$R0conf2 <- format(round(as.numeric(indivParams$R0conf2),1), nsmall=1)
  indivParamsprint$R0minconf2 <- format(round(as.numeric(indivParams$R0minconf2),1), nsmall=1)
  indivParamsprint$R0maxconf2 <- format(round(as.numeric(indivParams$R0maxconf2),1), nsmall=1)

  indivParamsprint$R0summary <- paste(indivParamsprint$R0," [",indivParamsprint$R0min,";",indivParamsprint$R0max,"]",sep="")
  indivParamsprint$R0confsummary <- paste(indivParamsprint$R0conf," [",indivParamsprint$R0minconf,";",indivParamsprint$R0maxconf,"]",sep="")
  indivParamsprint$R0conf2summary <- paste(indivParamsprint$R0conf2," [",indivParamsprint$R0minconf2,";",indivParamsprint$R0maxconf2,"]",sep="")

  indivParamsprint <- indivParamsprint %>% arrange(id)

  print(xtable(indivParamsprint[,c("id","b1summary","Dqsummary","E0summary","R0summary","R0confsummary","R0conf2summary")]),
               include.rownames=FALSE)
  indivParamsprint$timestart <- indivParams$timestart
  indivParamsprint$Icumul <- format(indivParams$Icumul, nsmall = 0, big.mark   = ",")
  indivParamsprint$Hcumul <- format(indivParams$Hcumul, nsmall = 0, big.mark   = ",")
  indivParamsprint$popsize <- format(indivParams$popsize, nsmall = 0, big.mark   = ",")
  indivParamsprint$ICUcapacity <- format(indivParams$ICUcapacity, nsmall = 0, big.mark   = ",")
  indivParamsprint$r_sent <- format(round(indivParams$r_sent, digits = 3), nsmall = 3, big.mark   = ",")

  print(xtable(indivParamsprint[,c("id","timestart","Icumul","Hcumul","popsize","ICUcapacity","r_sent")]),
        include.rownames=FALSE)

}

#indivParamsreg<-indivParams[i,]
getIHD<-function(solution,indivParamsreg){

  res<-as.data.frame(matrix(NA,ncol=21,nrow=0))
  names(res)<-c("reg","i","time","immunised","immunisedmin","immunisedmax","infected","infectedmin","infectedmax","Dincident","Dincidentmin","Dincidentmax","ICUincident","ICUincidentmin","ICUincidentmax","Iincident","Iincidentmin","Iincidentmax","Hincident","Hincidentmin","Hincidentmax" )

  timeinterest<-unique(solution$data$day[which(as.character(solution$data$date)=="2020-03-11")])

  tauxICU=0.25
  tauxICUmax=6662/(27432+6662)#Au 3/4/2020
  tauxICUmin=6662/(27432+6662)#Au 3/4/2020
  tauxDEATH=0.05
  tauxDEATHmax=5091/64338#Au 3/4/2020
  tauxDEATHmin=5091/64338#Au 3/4/2020

  for (i in (0:800)){
    thistime<-timeinterest+i

    immunised<-solution$solution$R[which(solution$solution$time==thistime)]
    immunisedmin<-solution$solution$Rmin[which(solution$solution$time==thistime)]
    immunisedmax<-solution$solution$Rmax[which(solution$solution$time==thistime)]

    infected<-solution$solution$E[which(solution$solution$time==thistime)]+solution$solution$I[which(solution$solution$time==thistime)]+solution$solution$A[which(solution$solution$time==thistime)]+solution$solution$H[which(solution$solution$time==thistime)]+solution$solution$R[which(solution$solution$time==thistime)]
    infectedmin<-solution$solution$Emin[which(solution$solution$time==thistime)]+solution$solution$Imin[which(solution$solution$time==thistime)]+solution$solution$Amin[which(solution$solution$time==thistime)]+solution$solution$Hmin[which(solution$solution$time==thistime)]+solution$solution$Rmin[which(solution$solution$time==thistime)]
    infectedmax<-solution$solution$Emax[which(solution$solution$time==thistime)]+solution$solution$Imax[which(solution$solution$time==thistime)]+solution$solution$Amax[which(solution$solution$time==thistime)]+solution$solution$Hmax[which(solution$solution$time==thistime)]+solution$solution$Rmax[which(solution$solution$time==thistime)]

    ## Cumul
    Iincident<-solution$solution$I[which(solution$solution$time==thistime)]+solution$solution$H[which(solution$solution$time==thistime)]+indivParamsreg[1,"r_sent"]*solution$solution$R[which(solution$solution$time==thistime)]
    Iincidentmax<-solution$solution$Imax[which(solution$solution$time==thistime)]+solution$solution$Hmax[which(solution$solution$time==thistime)]+indivParamsreg[1,"r_sent"]*solution$solution$Rmax[which(solution$solution$time==thistime)]
    Iincidentmin<-solution$solution$Imin[which(solution$solution$time==thistime)]+solution$solution$Hmin[which(solution$solution$time==thistime)]+indivParamsreg[1,"r_sent"]*solution$solution$Rmin[which(solution$solution$time==thistime)]

    Hincident<-solution$solution$H[which(solution$solution$time==thistime)]
    Hincidentmin<-solution$solution$Hmin[which(solution$solution$time==thistime)]
    Hincidentmax<-solution$solution$Hmax[which(solution$solution$time==thistime)]


    Dincident<-tauxDEATH*solution$solution$R[which(solution$solution$time==thistime)]
    Dincidentmin<-tauxDEATH*solution$solution$Rmin[which(solution$solution$time==thistime)]
    Dincidentmax<-tauxDEATH*solution$solution$Rmax[which(solution$solution$time==thistime)]


    ICUincident<-solution$solution$H[which(solution$solution$time==thistime)]*tauxICU
    ICUincidentmin<-solution$solution$Hmin[which(solution$solution$time==thistime)]*tauxICU
    ICUincidentmax<-solution$solution$Hmax[which(solution$solution$time==thistime)]*tauxICU

    res[i+1,]<-c(as.character(indivParamsreg[1,1]), i,thistime,immunised,immunisedmin,immunisedmax,infected,infectedmin,infectedmax,Dincident,Dincidentmin,Dincidentmax,ICUincident,ICUincidentmin,ICUincidentmax,Iincident,Iincidentmin,Iincidentmax,Hincident,Hincidentmin,Hincidentmax )

  }

  return(res)
}




#### GET % ASYMPTOMATIQUE
#indivParamsreg<-indivParams[1,]
#
# getAsymptomatique<-function(solution,indivParamsreg){
#
#   De<-solution$parameters$De
#   r<-indivParamsreg[1,"ascertainment_mode"]
#   sentinelle<-read.table("./data/sentinelle.txt",header=TRUE)
#   Sent<-sentinelle$I12[which(sentinelle$names==as.character(solution$data$reg_id[1]))]/7
#   Sentmax<-sentinelle$I12max[which(sentinelle$names==as.character(solution$data$reg_id[1]))]/7
#   Sentmin<-sentinelle$I12min[which(sentinelle$names==as.character(solution$data$reg_id[1]))]/7
#
#   dayS2<-solution$data$day[which((as.character(solution$data$date)%in%c("2020-03-16","2020-03-17","2020-03-18","2020-03-19","2020-03-20","2020-03-22","2020-03-23"))&(solution$data$obs_id==1))]
#   Eaverage_S12<-mean(solution$solution$E[which(solution$solution$time%in%dayS2)])
#
#   pctAss<-(Sent-r*Eaverage_S12)/((1-r)*Eaverage_S12)
#
#   #
#   # incidentI<-r*Eaverage_S12
#   # incidentA<-(1-r)*Eaverage_S12
#   # Iaverage_S12<-mean(solution$solution$I[which(solution$solution$time%in%dayS2)])
#   # Aaverage_S12<-mean(solution$solution$A[which(solution$solution$time%in%dayS2)])
#   # Iaverage_S12+Aaverage_S12
#   #
#   # Eaverage_S12*(1-r)/De
#   # Eaverage_S12*(r)/De
#   #
#   #
#   # NT<-Sent-r*Eaverage_S12/De
#   # NTplusAS<-incidentA
#   #
#   # pctAss<-(1-NT/Eaverage_S12)
#   return(pctAss)
# }
#
# # sd(indivParams$r_sent)
# #
# # ((1-indivParams$ascertainment_mode)-(indivParams$ascertainment_mode*indivParams$r_sent)/(1-indivParams$r_sent))/5.2
# #
# # mean(((1-indivParams$ascertainment_mode)-(indivParams$ascertainment_mode*indivParams$r_sent)/(1-indivParams$r_sent))/5.2)
# # sd(((1-indivParams$ascertainment_mode)-(indivParams$ascertainment_mode*indivParams$r_sent)/(1-indivParams$r_sent))/5.2)
#
#
#
#


plotPredictionShortterm <- function(predictions,predictionsUPDATED,predictionsNOEFFECT,nameproject, logscale=TRUE){
  datagouv<-read.table("./data/datagouv.txt",header=TRUE)
  datagouv$time<-as.Date(datagouv$time)

  datapred<-data.frame(time=seq(as.Date("2020-03-11"), as.Date("2020-04-30"), "day"))
  datapred$i<-seq(0,(length(datapred$time)-1),by=1)

  datapredUPDATED<-data.frame(time=seq(as.Date("2020-03-11"), as.Date("2020-04-30"), "day"))
  datapredUPDATED$i<-seq(0,(length(datapredUPDATED$time)-1),by=1)

  datapredNOEFFECT<-data.frame(time=seq(as.Date("2020-03-11"), as.Date("2020-04-30"), "day"))
  datapredNOEFFECT$i<-seq(0,(length(datapredNOEFFECT$time)-1),by=1)

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

    datapredNOEFFECT$infected[i+1]<-sum(as.numeric(predictionsNOEFFECT$infected[which(predictionsNOEFFECT$i==i)]))
    datapredNOEFFECT$immunised[i+1]<-sum(as.numeric(predictionsNOEFFECT$immunised[which(predictionsNOEFFECT$i==i)]))
    datapredNOEFFECT$Iincident[i+1]<-sum(as.numeric(predictionsNOEFFECT$Iincident[which(predictionsNOEFFECT$i==i)]))
    datapredNOEFFECT$Iincidentmin[i+1]<-sum(as.numeric(predictionsNOEFFECT$Iincidentmin[which(predictionsNOEFFECT$i==i)]))
    datapredNOEFFECT$Iincidentmax[i+1]<-sum(as.numeric(predictionsNOEFFECT$Iincidentmax[which(predictionsNOEFFECT$i==i)]))
    datapredNOEFFECT$Hincident[i+1]<-sum(as.numeric(predictionsNOEFFECT$Hincident[which(predictionsNOEFFECT$i==i)]))
    datapredNOEFFECT$Hincidentmin[i+1]<-sum(as.numeric(predictionsNOEFFECT$Hincidentmin[which(predictionsNOEFFECT$i==i)]))
    datapredNOEFFECT$Hincidentmax[i+1]<-sum(as.numeric(predictionsNOEFFECT$Hincidentmax[which(predictionsNOEFFECT$i==i)]))
    datapredNOEFFECT$Dincident[i+1]<-sum(as.numeric(predictionsNOEFFECT$Dincident[which(predictionsNOEFFECT$i==i)]))
    datapredNOEFFECT$Dincidentmin[i+1]<-sum(as.numeric(predictionsNOEFFECT$Dincidentmin[which(predictionsNOEFFECT$i==i)]))
    datapredNOEFFECT$Dincidentmax[i+1]<-sum(as.numeric(predictionsNOEFFECT$Dincidentmax[which(predictionsNOEFFECT$i==i)]))
    datapredNOEFFECT$ICUincident[i+1]<-sum(as.numeric(predictionsNOEFFECT$ICUincident[which(predictionsNOEFFECT$i==i)]))
    datapredNOEFFECT$ICUincidentmin[i+1]<-sum(as.numeric(predictionsNOEFFECT$ICUincidentmin[which(predictionsNOEFFECT$i==i)]))
    datapredNOEFFECT$ICUincidentmax[i+1]<-sum(as.numeric(predictionsNOEFFECT$ICUincidentmax[which(predictionsNOEFFECT$i==i)]))

    datapredUPDATED$infected[i+1]<-sum(as.numeric(predictionsUPDATED$infected[which(predictionsUPDATED$i==i)]))
    datapredUPDATED$immunised[i+1]<-sum(as.numeric(predictionsUPDATED$immunised[which(predictionsUPDATED$i==i)]))
    datapredUPDATED$Iincident[i+1]<-sum(as.numeric(predictionsUPDATED$Iincident[which(predictionsUPDATED$i==i)]))
    datapredUPDATED$Iincidentmin[i+1]<-sum(as.numeric(predictionsUPDATED$Iincidentmin[which(predictionsUPDATED$i==i)]))
    datapredUPDATED$Iincidentmax[i+1]<-sum(as.numeric(predictionsUPDATED$Iincidentmax[which(predictionsUPDATED$i==i)]))
    datapredUPDATED$Hincident[i+1]<-sum(as.numeric(predictionsUPDATED$Hincident[which(predictionsUPDATED$i==i)]))
    datapredUPDATED$Hincidentmin[i+1]<-sum(as.numeric(predictionsUPDATED$Hincidentmin[which(predictionsUPDATED$i==i)]))
    datapredUPDATED$Hincidentmax[i+1]<-sum(as.numeric(predictionsUPDATED$Hincidentmax[which(predictionsUPDATED$i==i)]))
    datapredUPDATED$Dincident[i+1]<-sum(as.numeric(predictionsUPDATED$Dincident[which(predictionsUPDATED$i==i)]))
    datapredUPDATED$Dincidentmin[i+1]<-sum(as.numeric(predictionsUPDATED$Dincidentmin[which(predictionsUPDATED$i==i)]))
    datapredUPDATED$Dincidentmax[i+1]<-sum(as.numeric(predictionsUPDATED$Dincidentmax[which(predictionsUPDATED$i==i)]))
    datapredUPDATED$ICUincident[i+1]<-sum(as.numeric(predictionsUPDATED$ICUincident[which(predictionsUPDATED$i==i)]))
    datapredUPDATED$ICUincidentmin[i+1]<-sum(as.numeric(predictionsUPDATED$ICUincidentmin[which(predictionsUPDATED$i==i)]))
    datapredUPDATED$ICUincidentmax[i+1]<-sum(as.numeric(predictionsUPDATED$ICUincidentmax[which(predictionsUPDATED$i==i)]))
  }
  datapred$time<-as.Date(datapred$time)
  datapredUPDATED$time<-as.Date(datapredUPDATED$time)
  datapredNOEFFECT$time<-as.Date(datapredNOEFFECT$time)

  log_title <- ""
  if(logscale){
    log_title <- " (log-scale)"
  }

  p1 <- ggplot(datapred, aes(x=time, y=Iincident)) +
    xlab("Date") + geom_vline(xintercept = as.Date("2020-03-25"), linetype=2, color="red3") +
    geom_vline(xintercept = as.Date("2020-04-06"), linetype=2, color="skyblue") +
    geom_line(aes(col="confinement with data\nup to 2020-03-25")) +
    geom_line(data=datapredNOEFFECT, aes(color="no intervention")) +
    geom_line(data=datapredUPDATED, aes(color="confinement with data\nup to 2020-04-06")) +
    geom_ribbon(data=datapredNOEFFECT,aes(ymin = Iincidentmin, ymax = Iincidentmax,
                                          fill = "no intervention",
                                          alpha="no intervention"))+
    geom_ribbon(data=datapredUPDATED,aes(ymin = Iincidentmin, ymax = Iincidentmax,
                                         fill = "confinement with data\nup to 2020-04-06",
                                         alpha="confinement with data\nup to 2020-04-06"))+
    geom_ribbon(data=datapred,aes(ymin = Iincidentmin, ymax = Iincidentmax,
                                  fill = "confinement with data\nup to 2020-03-25",
                                  alpha="confinement with data\nup to 2020-03-25")) +
    geom_point(data=datagouv, aes(x=time,y=Iobs, shape="Source: Santé\nPublique France")) +
    scale_shape("Observations") +
    scale_alpha_manual("Estimate (95% CI)", values=c(0.25, 0.25, 0.25),
                       breaks = c("no intervention", "confinement with data\nup to 2020-03-25",
                                  "confinement with data\nup to 2020-04-06")) +
    scale_color_manual("Estimate (95% CI)", values=c("purple", "red3", "skyblue"),
                       breaks = c("no intervention", "confinement with data\nup to 2020-03-25",
                                  "confinement with data\nup to 2020-04-06")) +
    scale_fill_manual("Estimate (95% CI)", values=c("purple", "red3", "skyblue"),
                      breaks = c("no intervention", "confinement with data\nup to 2020-03-25",
                                 "confinement with data\nup to 2020-04-06")) +
    theme_classic() +
    ylab(paste0("National cumulative incidence\nof ascertained cases", log_title)) +
    ggtitle("France")

  p2 <- ggplot(datapred, aes(x=time, y=Hincident)) +
    xlab("Date") + geom_vline(xintercept = as.Date("2020-03-25"), linetype=2, color="red3") +
    geom_vline(xintercept = as.Date("2020-04-06"), linetype=2, color="skyblue") +
    geom_line(aes(col="confinement with data\nup to 2020-03-25")) +
    geom_line(data=datapredNOEFFECT, aes(color="no intervention")) +
    geom_line(data=datapredUPDATED, aes(color="confinement with data\nup to 2020-04-06")) +
    geom_ribbon(data=datapredNOEFFECT,aes(ymin = Hincidentmin, ymax = Hincidentmax,
                                          fill = "no intervention",
                                          alpha="no intervention"))+
    geom_ribbon(data=datapredUPDATED,aes(ymin = Hincidentmin, ymax = Hincidentmax,
                                         fill = "confinement with data\nup to 2020-04-06",
                                         alpha="confinement with data\nup to 2020-04-06"))+
    geom_ribbon(data=datapred,aes(ymin = Hincidentmin, ymax = Hincidentmax,
                                  fill = "confinement with data\nup to 2020-03-25",
                                  alpha="confinement with data\nup to 2020-03-25")) +
    geom_point(data=datagouv, aes(x=time,y=Hobs, shape="Source: Santé\nPublique France")) +
    scale_shape("Observations") +
    scale_alpha_manual("Estimate (95% CI)", values=c(0.25, 0.25, 0.25),
                       breaks = c("no intervention", "confinement with data\nup to 2020-03-25",
                                  "confinement with data\nup to 2020-04-06")) +
    scale_color_manual("Estimate (95% CI)", values=c("purple", "red3", "skyblue"),
                       breaks = c("no intervention", "confinement with data\nup to 2020-03-25",
                                  "confinement with data\nup to 2020-04-06")) +
    scale_fill_manual("Estimate (95% CI)", values=c("purple", "red3", "skyblue"),
                      breaks = c("no intervention", "confinement with data\nup to 2020-03-25",
                                 "confinement with data\nup to 2020-04-06")) +
    theme_classic() +
    ylab(paste0("National prevalence\nof hospitalized cases", log_title)) +
    ggtitle("France")

  p3 <- ggplot(datapred, aes(x=time, y=ICUincident)) +
    xlab("Date") + geom_vline(xintercept = as.Date("2020-03-25"), linetype=2, color="red3") +
    geom_vline(xintercept = as.Date("2020-04-06"), linetype=2, color="skyblue") +
    geom_line(aes(col="confinement with data\nup to 2020-03-25")) +
    geom_line(data=datapredNOEFFECT, aes(color="no intervention")) +
    geom_line(data=datapredUPDATED, aes(color="confinement with data\nup to 2020-04-06")) +
    geom_ribbon(data=datapredNOEFFECT,aes(ymin = ICUincidentmin, ymax = ICUincidentmax,
                                          fill = "no intervention",
                                          alpha="no intervention"))+
    geom_ribbon(data=datapredUPDATED,aes(ymin = ICUincidentmin, ymax = ICUincidentmax,
                                         fill = "confinement with data\nup to 2020-04-06",
                                         alpha="confinement with data\nup to 2020-04-06"))+
    geom_ribbon(data=datapred,aes(ymin = ICUincidentmin, ymax = ICUincidentmax,
                                  fill = "confinement with data\nup to 2020-03-25",
                                  alpha="confinement with data\nup to 2020-03-25")) +
    geom_point(data=datagouv, aes(x=time,y=ICUobs, shape="Source: Santé\nPublique France")) +
    scale_shape("Observations") +
    scale_alpha_manual("Estimate (95% CI)", values=c(0.25, 0.25, 0.25),
                       breaks = c("no intervention", "confinement with data\nup to 2020-03-25",
                                  "confinement with data\nup to 2020-04-06")) +
    scale_color_manual("Estimate (95% CI)", values=c("purple", "red3", "skyblue"),
                       breaks = c("no intervention", "confinement with data\nup to 2020-03-25",
                                  "confinement with data\nup to 2020-04-06")) +
    scale_fill_manual("Estimate (95% CI)", values=c("purple", "red3", "skyblue"),
                      breaks = c("no intervention", "confinement with data\nup to 2020-03-25",
                                 "confinement with data\nup to 2020-04-06")) +
    theme_classic() +
    ylab(paste0("National prevalence\nof ICU cases", log_title)) +
    ggtitle("France")

  p4 <- ggplot(datapred, aes(x=time, y=Dincident)) +
    xlab("Date") + geom_vline(xintercept = as.Date("2020-03-25"), linetype=2, color="red3") +
    geom_vline(xintercept = as.Date("2020-04-06"), linetype=2, color="skyblue") +
    geom_line(aes(col="confinement with data\nup to 2020-03-25")) +
    geom_line(data=datapredNOEFFECT, aes(color="no intervention")) +
    geom_line(data=datapredUPDATED, aes(color="confinement with data\nup to 2020-04-06")) +
    geom_ribbon(data=datapredNOEFFECT,aes(ymin = Dincidentmin, ymax = Dincidentmax,
                                          fill = "no intervention",
                                          alpha="no intervention"))+
    geom_ribbon(data=datapredUPDATED,aes(ymin = Dincidentmin, ymax = Dincidentmax,
                                         fill = "confinement with data\nup to 2020-04-06",
                                         alpha="confinement with data\nup to 2020-04-06"))+
    geom_ribbon(data=datapred,aes(ymin = Dincidentmin, ymax = Dincidentmax,
                                  fill = "confinement with data\nup to 2020-03-25",
                                  alpha="confinement with data\nup to 2020-03-25")) +
    geom_point(data=datagouv, aes(x=time,y=Dobs, shape="Source: Santé\nPublique France")) +
    scale_shape("Observations") +
    scale_alpha_manual("Estimate (95% CI)", values=c(0.25, 0.25, 0.25),
                       breaks = c("no intervention", "confinement with data\nup to 2020-03-25",
                                  "confinement with data\nup to 2020-04-06")) +
    scale_color_manual("Estimate (95% CI)", values=c("purple", "red3", "skyblue"),
                       breaks = c("no intervention", "confinement with data\nup to 2020-03-25",
                                  "confinement with data\nup to 2020-04-06")) +
    scale_fill_manual("Estimate (95% CI)", values=c("purple", "red3", "skyblue"),
                      breaks = c("no intervention", "confinement with data\nup to 2020-03-25",
                                 "confinement with data\nup to 2020-04-06")) +
    theme_classic() +
    ylab(paste0("National cumulative incidence\nof death", log_title)) +
    ggtitle("France")

  if(logscale){
    p1 <- p1 + scale_y_log10()
    p2 <- p2 + scale_y_log10()
    p3 <- p3 + scale_y_log10()
    p4 <- p4 + scale_y_log10()
  }

  return(list(p1, p2, p3, p4))
}

getPlotPredictionShortterm <- function(predictions,predictionsUPDATED,predictionsNOEFFECT,nameproject, logscale=TRUE){


  if(logscale){
    logindicator <- "_logscale"
  }else{
    logindicator <- ""
  }

  library(patchwork)

  old.loc <- Sys.getlocale("LC_TIME")
  Sys.setlocale("LC_TIME", "en_GB.UTF-8")

  plotList <- plotPredictionShortterm(predictions,predictionsUPDATED,predictionsNOEFFECT,nameproject, logscale)
  p1 <- plotList[[1]]
  p2 <- plotList[[2]]
  p3 <- plotList[[3]]
  p4 <- plotList[[4]]

  p1 + (p2 +ggtitle("")) + plot_layout(guides = "collect")
  ggsave(file = paste(path,"outputMonolix/",nameproject,"/graphics/shortterm", logindicator, ".jpg",sep=""),
         device = "jpeg", dpi =300, width=11, height=4)

  (p1 + (p2+ggtitle("")))/((p3+ggtitle("")) + (p4+ggtitle(""))) + plot_layout(guides = "collect")
  ggsave(file = paste(path,"outputMonolix/",nameproject,"/graphics/shortterm_all", logindicator, ".jpg",sep=""),
         device = "jpeg", dpi = 300, width=11, height=8)

  Sys.setlocale("LC_TIME",old.loc)

}
