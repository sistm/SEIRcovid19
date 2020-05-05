PlotSolutionOverObservation<-function(ode_list,ModelObservationBloc,is_normalize=0){
  
  # Init result list
  ObservationResult <- vector(mode = "list", length = length(ModelObservationBloc))
  name_variable<-list()
  
  # Look for variable name
  CutObservation<-strsplit(ModelObservationBloc,'=')
  
  # Read Input data
  data<-read.table(ode_list[[1]]$DataInfo$File,sep=ode_list[[1]]$DataInfo$Sep,header=TRUE)
  InputNames<-colnames(data)
  timename<-InputNames[ode_list[[1]]$DataInfo$HeaderType=="time"]
  idname<-InputNames[ode_list[[1]]$DataInfo$HeaderType=="id"]
  ObsIdName<-InputNames[ode_list[[1]]$DataInfo$HeaderType=="obsid"]
  ObservationName<-InputNames[ode_list[[1]]$DataInfo$HeaderType=="observation"]
  
  # Read IndivParams, only use for id name
  indivParams <-read.table(paste(here::here(),'/MonolixFile/',"/outputMonolix/",ode_list[[1]]$nameproject,"/IndividualParameters/estimatedIndividualParameters.txt",sep=""),header=TRUE,sep=",")
  
  # Evaluate a string 
  GetStatWithExp<-function(solution,parameter,exp,name){
    with(as.list(c(solution,parameter)),{
      State<-data.frame((eval(parse(text=exp))))
      names(State)<-name
      return(State)
    })
  }
  
  # Loop over the observation
  # One plot per obs
  for (iobs in 1:length(ModelObservationBloc)){
    # Look over id
    for (id in 1:length(ode_list)){
      name_variable[iobs]<-CutObservation[[iobs]][1]
      
      # Get the simulated value
      Obssim<-GetStatWithExp(ode_list[[id]]$solution,ode_list[[id]]$parameter,ModelObservationBloc[iobs],name_variable[iobs])
      # Get the ICmin
      Obssim<-cbind(Obssim,GetStatWithExp(ode_list[[id]]$ICmin,ode_list[[id]]$parameter,ModelObservationBloc[iobs],paste(name_variable[iobs],"_min",sep="")))
      # Get the ICmax
      Obssim<-cbind(Obssim,GetStatWithExp(ode_list[[id]]$ICmax,ode_list[[id]]$parameter,ModelObservationBloc[iobs],paste(name_variable[iobs],"_max",sep="")))
      # Store time
      Obssim<-cbind(Obssim,ode_list[[id]]$solution$time)
      colnames(Obssim)[dim(Obssim)[2]]<-timename
      
      #Store ID name
      Obssim<-cbind(Obssim,rep(indivParams$id[id],dim(Obssim)[[1]]))
      colnames(Obssim)[dim(Obssim)[2]]<-"id"
      
      # Store the correspondant obs
      Observation<-ode_list[[id]]$ObsData[which(ode_list[[id]]$ObsData[,ObsIdName]==iobs),]
      ObservationResult[[iobs]][[id]] <-merge(Observation, Obssim, by = timename)
      
      ObservationResult[[iobs]][[id]]$popsize <- ode_list[[id]]$parameter[names(ode_list[[id]]$parameter)=='popsize']
      # Store the result in the system
      ode_list[[id]]$ObsSimu[[iobs]]<-ObservationResult[[iobs]][[id]]
    }
    
    # Concatenate over ID
    ObservationDataFrame <- do.call(rbind.data.frame, ObservationResult[[iobs]])
    # Be sure to be a date
    ObservationDataFrame$date<-as.Date(ObservationDataFrame$date)
    # DO plot
    if (dir.exists(paste0(here::here(),'/MonolixFile/outputMonolix/',ode_list[[1]]$nameproject,"/graphics/"))){
      
    }
    else{
      dir.create(paste0(here::here(),'/MonolixFile/outputMonolix/',ode_list[[1]]$nameproject,"/graphics/"))
    }
    if (is_normalize==1){
      
      ObservationDataFrame$date<-as.Date(ObservationDataFrame$date)
      ObservationDataFrame[,ObservationName]<-ObservationDataFrame[,ObservationName]/ObservationDataFrame$popsize*100
      ObservationDataFrame[,name_variable[[iobs]]]<-ObservationDataFrame[,name_variable[[iobs]]]/ObservationDataFrame$popsize*100
      ObservationDataFrame[,paste(name_variable[[iobs]],"_min",sep="")]<-ObservationDataFrame[,paste(name_variable[[iobs]],"_min",sep="")]/ObservationDataFrame$popsize*100
      ObservationDataFrame[,paste(name_variable[[iobs]],"_max",sep="")]<-ObservationDataFrame[,paste(name_variable[[iobs]],"_max",sep="")]/ObservationDataFrame$popsize*100
      
      p1 <- ggplot(ObservationDataFrame, aes_(x=as.name("date"), y=as.name(ObservationName), group=as.name("id"))) +
        geom_point(aes(color = "Observed"))+
        scale_shape_manual(values=c(NA, 16)) +
        scale_color_manual(values="black") +
        geom_line(aes_(x=as.name("date"), y=as.name(name_variable[[iobs]]),linetype="Estimate"), color="red3") +
        geom_ribbon(aes_(ymin = as.name(paste(name_variable[[iobs]],"_min",sep="")), ymax=as.name(paste(name_variable[[iobs]],"_max",sep="")),alpha="95 %CI"), fill="red3")+
        scale_alpha_manual(values=c(0.3)) +
        facet_grid(vars(id), scales = "free_y") + facet_wrap(~ id, nrow=2)+
        guides(color=guide_legend(title=""), linetype=guide_legend(title=""),
               alpha=guide_legend(title=""))+
        theme(legend.position = "bottom") +
        ylab(paste(map[[iobs]]," in %",sep="")) +
        xlab("Day") +
        theme(axis.text.x = element_text(angle=45, hjust=1))+
        theme(strip.background = element_rect(fill="white"),
              strip.text = element_text(size=8))
      ggsave(plot=p1, filename = paste0(here::here(),'/MonolixFile/outputMonolix/',ode_list[[1]]$nameproject,"/graphics/", map[[iobs]], "_norm.jpg"), width=10, height=8)
      
      
    }else{
      p1 <- ggplot(ObservationDataFrame, aes_(x=as.name("date"), y=as.name(ObservationName), group=as.name("id"))) +
        geom_point(aes(color = "Observed"))+
        scale_shape_manual(values=c(NA, 16)) +
        scale_color_manual(values="black") +
        geom_line(aes_(x=as.name("date"), y=as.name(name_variable[[iobs]]),linetype="Estimate"), color="red3") +
        geom_ribbon(aes_(ymin = as.name(paste(name_variable[[iobs]],"_min",sep="")), ymax=as.name(paste(name_variable[[iobs]],"_max",sep="")),alpha="95 %CI"), fill="red3")+
        scale_alpha_manual(values=c(0.3)) +
        facet_grid(vars(id), scales = "free_y") + facet_wrap(~ id, nrow=2)+
        guides(color=guide_legend(title=""), linetype=guide_legend(title=""),
               alpha=guide_legend(title=""))+
        theme(legend.position = "bottom") +
        ylab(map[[iobs]]) +
        xlab("Day") +
        theme(axis.text.x = element_text(angle=45, hjust=1),
              axis.title.y = element_text(hjust=1.4)) +
        theme(strip.background = element_rect(fill="white"),
              strip.text = element_text(size=8))
      #Save plot
      
      
    }
    
  }
  return(ode_list)
  
}