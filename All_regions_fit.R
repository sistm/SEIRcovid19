rm(list=ls())

region_data <- get_data_covid19_bylevel()
region_pop <- read.delim("data/population_regionsFR_INED1erjan2020.tsv")

library(dplyr)
res <- list()
for (r in names(region_data)){
  pop_reg <- region_pop %>%
    filter(maille_code == r) %>%
    pull(population)
  res[[r]] <- seirah_estim(c(log(1.75),log(0.41)),
                    data=region_data[[r]],
                    popSize=pop_reg,
                    dailyMove=0.1*pop_reg,
                    verbose=FALSE)
  message(r, " fitted")
}


all_plots <- list()
for (r in names(region_data)){
  all_plots[[r]] <- plot(res[[r]]) + ggtitle(r)
}

library(cowplot)
cowplot::plot_grid(plotlist = all_plots, ncol=3)
