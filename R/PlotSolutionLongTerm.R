#' @importFrom colorspace scale_color_discrete_qualitative scale_fill_discrete_qualitative
#' @import here
#' @export
PlotSolutionLongTerm<-function(ode_list){

  indivParams <-read.table(paste(here::here(),'/MonolixFile/',"/outputMonolix/",ode_list[[1]]$nameproject,"/IndividualParameters/estimatedIndividualParameters.txt",sep=""),header=TRUE,sep=",")

  solutions_list <- list()
  solutionmin<-list()
  solutionmax<-list()


  for (id in 1:length(ode_list)){
    solution <- ode_list[[id]]$LongTerm
    solution$popsize <- ode_list[[id]]$parameter[names(ode_list[[id]]$parameter)=='popsize']

    solutions_list[[id]] <- solution
    solutions_list[[id]]$reg<-as.character(indivParams$id[id])
    solutions_list[[id]]$date <- solution$date

    solutionmin[[id]]<-ode_list[[id]]$LongTermMin
    solutionmin[[id]]$date <- solution$date
    solutionmin[[id]]$popsize <- solution$popsize
    solutionmin[[id]]$reg <- as.character(indivParams$id[id])

    solutionmax[[id]]<-ode_list[[id]]$LongTermMax
    solutionmax[[id]]$date <- solution$date
    solutionmax[[id]]$popsize <- solution$popsize
    solutionmax[[id]]$reg <- as.character(indivParams$id[id])

  }


  solutions_allsim <- do.call(rbind.data.frame,solutions_list) %>% select(-c("time")) %>% reshape2::melt(id.vars=c("date", "reg", "popsize"))
  solutions_allmin <- do.call(rbind.data.frame,solutionmin)  %>% reshape2::melt(id.vars=c("date", "reg","popsize"), value.name  = "value.min")
  solutions_allmax <- do.call(rbind.data.frame,solutionmax) %>% reshape2::melt(id.vars=c("date", "reg","popsize"), value.name  = "value.max")
  solutions_2plot <- cbind.data.frame(solutions_allsim,
                                      "value.min" = solutions_allmin$value.min,
                                      "value.max" = solutions_allmax$value.max)

  p <- ggplot(solutions_2plot, aes(fill=reg, x=date)) +
    geom_line(aes(y=value/popsize, colour = reg)) +
    geom_ribbon(aes(ymin=value.min/popsize, ymax=value.max/popsize), alpha = 0.3) +
    geom_vline(aes(xintercept=as.Date("2020-03-17"), linetype="Lockdown start")) +
    geom_vline(aes(xintercept=as.Date("2020-05-11"),  linetype="Lockdown lift")) +
    scale_linetype_manual("", values=c(2,3), breaks=c("Lockdown start", "Lockdown lift")) +
    xlim(c(as.Date(min(solutions_2plot$date)), as.Date(max(solutions_2plot$date)))) +
    theme_bw() +
    scale_y_continuous(labels = scales::percent_format(accuracy = 0.01)) +
    facet_wrap(~variable, scales="free_y", ncol=2) +
    ylab("Proportion of region population") +
    xlab("Date") +
    colorspace::scale_color_discrete_qualitative(name = "Region", palette = "Dark3") +
    colorspace::scale_fill_discrete_qualitative(name = "Region", palette = "Dark3") +
    theme(legend.text = element_text(size = 8),
          legend.title = element_text(size = 10)) +
    theme(legend.position = "bottom", legend.box = "vertical")
  if (dir.exists(paste0(here::here(),'/MonolixFile/outputMonolix/',ode_list[[1]]$nameproject,"/graphics/"))){

  }
  else{
    dir.create(paste0(here::here(),'/MonolixFile/outputMonolix/',ode_list[[1]]$nameproject,"/graphics/"))
  }

  ggsave(plot=p, filename = paste0(here::here(),'/MonolixFile/outputMonolix/',ode_list[[1]]$nameproject,"/graphics/","LongTermSolution.jpg"),
         width=10, height=8)
  ggsave(plot=p, filename = paste0(here::here(),'/MonolixFile/outputMonolix/',ode_list[[1]]$nameproject,"/graphics/","LongTermSolution.pdf"),
         width=10, height=8)

  #return(solutions_2plot)
  return(list(solution=solutions_list,solution_min=solutionmin,solution_max=solutionmax))

}
