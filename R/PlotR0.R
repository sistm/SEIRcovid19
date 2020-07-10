#' @export

PlotR0<-function(ode_list,R0_formula,R0min_formula,R0max_formula,ci=TRUE){
  data<-read.table(ode_list[[1]]$DataInfo$File,sep=ode_list[[1]]$DataInfo$Sep,header=TRUE)
  InputNames<-colnames(data)
  timename<-InputNames[ode_list[[1]]$DataInfo$HeaderType=="time"]
  idname<-InputNames[ode_list[[1]]$DataInfo$HeaderType=="id"]
  ObsIdName<-InputNames[ode_list[[1]]$DataInfo$HeaderType=="obsid"]
  ObservationName<-InputNames[ode_list[[1]]$DataInfo$HeaderType=="observation"]
  indivParams <-read.table(paste(here::here(),'/MonolixFile/',"/outputMonolix/",ode_list[[1]]$nameproject,"/IndividualParameters/estimatedIndividualParameters.txt",sep=""),header=TRUE,sep=",")


  GetR0WithExp<-function(solution,parameter,timeparam,exp,name){
    with(as.list(c(solution,parameter,timeparam)),{
      R0<-data.frame((eval(parse(text=exp))))
      names(R0)<-name
      return(R0)
    })
  }

  GetR0ICWithExp<-function(ICmin,ICmax,parameter,timeparamMin,timeparamMax,exp,name,solution){
    with(as.list(c(ICmin,ICmax,parameter,timeparamMin,timeparamMax,solution)),{
      R0<-data.frame((eval(parse(text=exp))))
      names(R0)<-name
      return(R0)
    })
  }

  AddSuffixName<-function(mylist,offset,suffix){
    actual_names<-colnames(mylist)
    for (i in (1+offset):length(actual_names)){
      actual_names[[i]]<-paste(actual_names[[i]],suffix,sep="")
    }
    colnames(mylist)<-actual_names
    return(mylist)
  }


  #Init
  R0_id<-list()
  # Loop over ID
  for (id in 1:length(ode_id)){
    #Compute R0
    R0_sim<-GetR0WithExp(ode_id[[id]]$solution,ode_id[[id]]$parameter,ode_id[[id]]$TimeDependantParameter,R0_formula,"R0")
    # Add time
    R0_sim<-cbind(R0_sim,ode_id[[id]]$solution$time)
    colnames(R0_sim)[dim(R0_sim)[2]]<-timename

    # Add ID
    R0_sim<-cbind(R0_sim,rep(indivParams$id[id],dim(R0_sim)[[1]]))
    colnames(R0_sim)[dim(R0_sim)[2]]<-"id"
    # Add suffixe for evaluation of the R0_min/max exp
    ICmin<-ode_id[[id]]$solution-1.96*sqrt(ode_id[[id]]$solution) # or ICmin
    ICmin<-AddSuffixName(ICmin,0,"_min")
    ICmax<-ode_id[[id]]$solution+1.96*sqrt(ode_id[[id]]$solution) # or ICmax
    ICmax<-AddSuffixName(ICmax,0,"_max")
    Pmin<-ode_id[[id]]$ParamICmin
    Pmin<-AddSuffixName(Pmin,0,"_min")
    Pmax<-ode_id[[id]]$ParamICmax
    Pmax<-AddSuffixName(Pmax,0,"_max")
    #Compute R0_min
    R0_min<-GetR0ICWithExp(ICmin,ICmax,ode_id[[id]]$parameter,Pmin,Pmax,R0min_formula,"R0_min",ode_id[[id]]$solution)
    R0_sim<-cbind(R0_sim,R0_min)
    # Compute R0_max
    R0_max<-GetR0ICWithExp(ICmin,ICmax,ode_id[[id]]$parameter,Pmin,Pmax,R0max_formula,"R0_max",ode_id[[id]]$solution)
    R0_sim<-cbind(R0_sim,R0_max)

    # Add observation in order to have date
    Observation<-unique(ode_id[[id]]$ObsData)
    R0_id[[id]] <-merge(Observation, R0_sim, by = timename)
    R0_id[[id]]$popsize <- ode_id[[id]]$parameter[names(ode_id[[id]]$parameter)=='popsize']
    ode_list[[id]]$R0<-R0_id[[id]]

  }

  R0DataFrame <- do.call(rbind.data.frame, R0_id)
  R0DataFrame$date<-as.Date(R0DataFrame$date)
  all_date<-unique(R0DataFrame$date)
  # Compute R0 national
  for (idate in 1:length(all_date)){
    data_per_date<-R0DataFrame[which(R0DataFrame$date==all_date[idate]),]
    R0DataFrame[which(R0DataFrame$date==all_date[idate]),"R0_national"]<-sum(data_per_date$R0*data_per_date$popsize)/sum(data_per_date$popsize)
  }


  # Create Plot Dir
  if (dir.exists(paste0(here::here(),'/MonolixFile/outputMonolix/',ode_list[[1]]$nameproject,"/graphics/"))){

  }
  else{
    dir.create(paste0(here::here(),'/MonolixFile/outputMonolix/',ode_list[[1]]$nameproject,"/graphics/"))
  }


  old.loc   <-   Sys.getlocale("LC_TIME")
  Sys.setlocale("LC_TIME", "en_GB.UTF-8")

  # Do plot
  if(ci){
    R0DataFrame$id<-full_region_names(R0DataFrame$id)
  p<-ggplot(R0DataFrame, aes_(x=as.name("date"),y=as.name("R0"),group=as.name("id"))) +
    geom_line(aes(linetype="Region-wise value\n(95% CI)"),color="red3") +
    geom_line(aes_(y=as.name("R0_national"),linetype="France\nnational average"),color="black")+
    scale_linetype_manual("", values = c(2, 1)) +
    geom_ribbon(aes_(ymin = as.name("R0_min"), ymax=as.name("R0_max"),alpha="Region-wise value\n(95% CI)"), fill="red3")+
    facet_grid(vars(id), scales = "free_y") + facet_wrap(~ id, ncol=3)+
    geom_hline(yintercept = 1)+
    scale_alpha_manual(values=c(0.3)) +
    theme_bw() +
    theme(strip.background = element_rect(fill="white")) +
    ylab(expression(paste("Effective Reproductive Number ", R[e](t, xi[i])))) +
    ylim(c(0,5)) +
    guides(linetype=guide_legend(title=""),alpha=guide_legend(title=""))+
    theme(legend.position = "bottom") +
    theme(axis.text.x = element_text(angle=45, hjust=1)) +
    theme(axis.text.y = element_text(size=8)) +
    theme(strip.background = element_rect(fill="white"),
          strip.text = element_text(size=8))
  }else{
    p<-ggplot(R0DataFrame, aes_(x=as.name("date"),y=as.name("R0"),group=as.name("id"))) +
      geom_line(aes(linetype="Region-wise value\n(95% CI)"),color="red3") +
      geom_line(aes_(y=as.name("R0_national"),linetype="France\nnational average"),color="black")+
      scale_linetype_manual("", values = c(2, 1)) +
      facet_grid(vars(id), scales = "free_y") + facet_wrap(~ id, ncol=3)+
      geom_hline(yintercept = 1)+
      scale_alpha_manual(values=c(0.3)) +
      theme_bw() +
      theme(strip.background = element_rect(fill="white")) +
      ylab(expression(paste("Effective Reproductive Number ", R[e](t, xi[i])))) +
      ylim(c(0,5)) +
      guides(linetype=guide_legend(title=""),alpha=guide_legend(title=""))+
      theme(legend.position = "bottom") +
      theme(axis.text.x = element_text(angle=45, hjust=1)) +
      theme(axis.text.y = element_text(size=8)) +
      theme(strip.background = element_rect(fill="white"),
            strip.text = element_text(size=8))
  }

  ggsave(plot=p, filename = paste0(here::here(),'/MonolixFile/outputMonolix/',ode_list[[1]]$nameproject,"/graphics/","R0Plot.pdf"),
         width=10, height=8, device = "pdf")
  ggsave(plot=p, filename = paste0(here::here(),'/MonolixFile/outputMonolix/',ode_list[[1]]$nameproject,"/graphics/","R0Plot.jpg"),
         width=10, height=8, device = "jpg", dpi=300)

  Sys.setlocale("LC_TIME", old.loc)

  return(ode_list)

}
