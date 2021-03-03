#' @importFrom forcats fct_recode
#' @export
PlotArticleR0 <- function(ode_list, write_graph_2file = TRUE){

  R0_id <- list()
  for (id in 1:length(ode_list)){
    R0_id[[id]] <- ode_list[[id]]$R0

  }
  R0DataFrame  <-  do.call(rbind.data.frame, R0_id)
  R0DataFrame$date <- as.Date(R0DataFrame$date)
  all_date <- unique(R0DataFrame$date)
  # Compute R0 national
  for (idate in 1:length(all_date)){
    data_per_date <- R0DataFrame[which(R0DataFrame$date==all_date[idate]), ]
    R0DataFrame[which(R0DataFrame$date==all_date[idate]), "R0_national"] <- sum(data_per_date$R0*data_per_date$popsize)/sum(data_per_date$popsize)
  }

  R0DataFrame$id <- full_region_names(R0DataFrame$id)
  R0DataFrame$color <- "Before lockdown"
  R0DataFrame$date <- as.Date(R0DataFrame$date)
  R0DataFrame$color[R0DataFrame$date>as.Date("2020-03-16")] <- "After lockdown\ndata up to 2020-03-24"
  R0DataFrame$color[R0DataFrame$date>(as.Date("2020-03-16")+7)] <- "After lockdown\ndata up to 2020-03-31"
  R0DataFrame$color[R0DataFrame$date>(as.Date("2020-03-16")+14)] <- "After lockdown\ndata up to 2020-05-11"


  R0DataFrame$Inter  <-  factor(R0DataFrame$color, levels=c("Before lockdown",
                                                          "After lockdown\ndata up to 2020-03-24",
                                                          "After lockdown\ndata up to 2020-03-31",
                                                          "After lockdown\ndata up to 2020-05-11"),
                              ordered = TRUE)

  R0before <- R0DataFrame[R0DataFrame$color=="Before lockdown", ]
  R0FirstWeek <- R0DataFrame[R0DataFrame$color=="After lockdown\ndata up to 2020-03-24", ]
  R0SecondWeek <- R0DataFrame[R0DataFrame$color=="After lockdown\ndata up to 2020-03-31", ]
  R0AfterSecondWeek <- R0DataFrame[R0DataFrame$color=="After lockdown\ndata up to 2020-05-11", ]


  temp  <-  R0FirstWeek[R0FirstWeek$date == as.Date("2020-03-17"), ]
  temp$color <- "Before lockdown"
  R0before  <-  dplyr::bind_rows(R0before, temp)

  temp  <-  R0SecondWeek[R0SecondWeek$date == as.Date("2020-03-24"), ]
  temp$color <- "After lockdown\ndata up to 2020-03-24"
  R0FirstWeek  <-  dplyr::bind_rows(R0FirstWeek, temp)

  temp  <-  R0AfterSecondWeek[R0AfterSecondWeek$date == as.Date("2020-03-31"), ]
  temp$color <- "After lockdown\ndata up to 2020-03-31"
  R0SecondWeek  <-  dplyr::bind_rows(R0SecondWeek, temp)


  old.loc   <-   Sys.getlocale("LC_TIME")
  Sys.setlocale("LC_TIME", "en_GB.UTF-8")

  p <- ggplot(R0DataFrame, aes_(x=as.name("date"), y=as.name("R0"), group=as.name("id"))) +
    geom_line(aes(linetype="Region-wise value\n(95% CI)", color=Inter)) +
    geom_line(aes_(y=as.name("R0_national"), linetype="France\nnational average"), color="black")+
    scale_linetype_manual("", values = c(2, 1)) +
    geom_ribbon(data=R0before, aes(ymin=R0_min, ymax=R0_max, alpha="Region-wise value\n(95% CI)"), fill="#440154FF")+
    geom_ribbon(data=R0FirstWeek, aes(ymin=R0_min, ymax=R0_max, alpha="Region-wise value\n(95% CI)"), fill="#31688EFF")+
    geom_ribbon(data=R0SecondWeek, aes(ymin=R0_min, ymax=R0_max, alpha="Region-wise value\n(95% CI)"), fill="#35B779FF")+
    geom_ribbon(data=R0AfterSecondWeek, aes(ymin=R0_min, ymax=R0_max, alpha="Region-wise value\n(95% CI)"), fill="#FDE725FF")+

    facet_grid(vars(id), scales = "free_y") + facet_wrap(~ id, ncol=3)+
    geom_hline(yintercept = 1)+
    scale_alpha_manual(values=c(0.3)) +
    theme_bw() +
    theme(strip.background = element_rect(fill="white")) +
    ylab(expression(paste("Effective Reproductive Number ", R[e](t, xi[i])))) +
    ylim(c(0, 5)) +
    guides(linetype=guide_legend(title=""), alpha="none", color=guide_legend(title=""), fill="none")+
    theme(legend.position = "bottom") +
    theme(axis.text.x = element_text(angle=45, hjust=1)) +
    theme(axis.text.y = element_text(size=8)) +
    theme(strip.background = element_rect(fill="white"),
          strip.text = element_text(size=8))

  if(write_graph_2file){
    ggsave(plot=p, filename = paste0(here::here(), '/MonolixFile/outputMonolix/', ode_list[[1]]$nameproject, "/graphics/", "ArticleR0.jpg"), width=10, height=8)
  }

  Sys.setlocale("LC_TIME", old.loc)

  return(p)

}


