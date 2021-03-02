#' @import patchwork
#' @importFrom forcats fct_recode
#' @export

PlotArticleFit <- function(ode_list, ModelObservationBloc, is_normalize){


  ObservationResult <- vector(mode = "list", length = 2)
  ObservationName <- "obs"
  name_variable <- list()

  # Look for variable name
  CutObservation <- strsplit(ModelObservationBloc,'=')

  for (iobs in 1:2){
    name_variable[iobs] <- CutObservation[[iobs]][1]
    for (id in 1:length(ode_list)){

      ObservationResult[[iobs]][[id]] <- ode_list[[id]]$ObsSimu[[iobs]]
    }
    ObservationDataFrame <- do.call(rbind.data.frame, ObservationResult[[iobs]])
    ObservationDataFrame$date <- as.Date(ObservationDataFrame$date)
    ObservationDataFrame$id <- full_region_names(ObservationDataFrame$id)
    ObservationDataFrame$obs_id <- fct_recode(factor(ObservationDataFrame$obs_id),
                                                       "Incident confirmed cases" = "1",
                                                       "Incident hospitalized cases" = "2"
    )
    if (is_normalize){
      ObservationDataFrame[,ObservationName] <- ObservationDataFrame[,ObservationName]/ObservationDataFrame$popsize*100
      ObservationDataFrame[,name_variable[[iobs]]] <- ObservationDataFrame[,name_variable[[iobs]]]/ObservationDataFrame$popsize*100
      ObservationDataFrame[,paste(name_variable[[iobs]],"_min",sep="")] <- ObservationDataFrame[,paste(name_variable[[iobs]],"_min",sep="")]/ObservationDataFrame$popsize*100
      ObservationDataFrame[,paste(name_variable[[iobs]],"_max",sep="")] <- ObservationDataFrame[,paste(name_variable[[iobs]],"_max",sep="")]/ObservationDataFrame$popsize*100
    }

    if (iobs==1){
      dataObs2plot_1 <- ObservationDataFrame
    }else{
      dataObs2plot_2 <- ObservationDataFrame

    }
  }
  ylabel <- "Incidence number"
  baseline <- geom_hline(yintercept = 1)
  dataObs2plot_1 <- dataObs2plot_1[,c("obs","obs_id","date","Isim","Isim_min","Isim_max","id")]
  colnames(dataObs2plot_1) <- c("obs","obs_id","date","Sim","Sim_min","Sim_max","id")
  dataObs2plot_2 <- dataObs2plot_2[,c("obs","obs_id","date","Hsim","Hsim_min","Hsim_max","id")]
  colnames(dataObs2plot_2) <- c("obs","obs_id","date","Sim","Sim_min","Sim_max","id")

  all_data <- rbind(dataObs2plot_1,dataObs2plot_2)
  dataObs2plot_1$id <- full_region_names(dataObs2plot_1$id)
  dataObs2plot_2$id <- full_region_names(dataObs2plot_2$id)

  dataObs2plot_1 <- all_data %>% filter(id %in% levels(all_data$id)[1:6])
  dataObs2plot_2 <- all_data %>% filter(id %in% levels(all_data$id)[7:12])

  Imax_obs <- max(all_data %>% filter(obs_id == "Incident confirmed cases") %>% pull(obs))
  Imax_sim <- max(all_data %>% filter(obs_id == "Incident confirmed cases") %>% pull(Sim_max))
  Hmax_obs <- max(all_data %>% filter(obs_id == "Incident hospitalized cases") %>% pull(obs))
  Hmax_sim <- max(all_data %>% filter(obs_id == "Incident hospitalized cases") %>% pull(Sim_max))

  dataObs2plot_1$show <- TRUE
  dataObs2plot_1 <- rbind(cbind.data.frame(obs = c(max(Imax_obs, Imax_sim), max(Hmax_obs, Hmax_sim)),
                                obs_id = c("Incident confirmed cases", "Incident hospitalized cases"),
                                date= rep(min(all_data$date), 2),
                                Sim = c(NA, NA),
                                Sim_min = c(NA, NA),
                                Sim_max = c(NA, NA),
                                id = dataObs2plot_1$id[1:2],
                                show = rep(FALSE, 2)),
                          dataObs2plot_1)

  dataObs2plot_2$show <- TRUE
  dataObs2plot_2 <- rbind(cbind.data.frame(obs = c(max(Imax_obs, Imax_sim), max(Hmax_obs, Hmax_sim)),
                                           obs_id = c("Incident confirmed cases", "Incident hospitalized cases"),
                                           date= rep(min(all_data$date), 2),
                                           Sim = c(NA, NA),
                                           Sim_min = c(NA, NA),
                                           Sim_max = c(NA, NA),
                                           id = dataObs2plot_2$id[1:2],
                                           show = rep(FALSE, 2)),
                          dataObs2plot_2)

  old.loc <- Sys.getlocale("LC_TIME")
  Sys.setlocale("LC_TIME", "en_GB.UTF-8")

  p1 <- ggplot(dataObs2plot_1, aes(x=date, y=obs, group=id)) +
    baseline +
    geom_point(aes(color="Observed", shape=show) )+
    geom_line(data = dataObs2plot_1,
              aes(y=Sim,linetype="Estimate"), color="red3") +
    geom_ribbon(data = dataObs2plot_1,
                aes(ymin = Sim_min, ymax=Sim_max, alpha="95% CI"), fill="red3")+
    scale_shape_manual(values=c(NA, 16)) +
    scale_alpha_manual(values=c(0.3)) +
    scale_color_manual(values="black") +
    facet_grid(obs_id~id, scales = "free_y") +
    theme_bw() +
    guides(color="none", linetype="none", shape="none", alpha="none") +
    theme(legend.position = "bottom") +
    ylab("") +
    xlab(NULL) +
    theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
    theme(strip.background = element_rect(fill="white"),
          strip.text = element_text(size=8)) +
    NULL
  p2 <- ggplot(dataObs2plot_2, aes(x=date, y=obs, group=id)) +
    baseline +
    geom_point(aes(color="Observed", shape=show)) +
    geom_line(data = dataObs2plot_2,
              aes(y=Sim,linetype="Estimate"), color="red3") +
    geom_ribbon(data = dataObs2plot_2,
                aes(ymin = Sim_min, ymax=Sim_max, alpha="95% CI"), fill="red3")+
    scale_shape_manual(values=c(NA, 16)) +
    scale_alpha_manual(values=c(0.3)) +
    scale_color_manual(values="black") +
    facet_grid(obs_id~id, scales = "free_y") +
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

  plot_res <- p1/p2
  ggsave(plot=plot_res, filename = paste0(here::here(),'/MonolixFile/outputMonolix/',ode_list[[1]]$nameproject,"/graphics/","ArticlePlot.jpg"),
         width=10, height=8, device="jpeg", dpi = 300)
  ggsave(plot=plot_res, filename = paste0(here::here(),'/MonolixFile/outputMonolix/',ode_list[[1]]$nameproject,"/graphics/","ArticlePlot.pdf"),
         width=10.3, height=8.24)

  Sys.setlocale("LC_TIME", old.loc)
  return(plot_res)
}


