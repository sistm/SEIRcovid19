#
#
# data_FR <- utils::read.csv(file = "data/raw/donnees-hospitalieres-nouveaux-covid19-2020-04-10-19h00.csv",
#                     sep=";")
#
# library(dplyr)
# data_FR$jour <- as.Date(data_FR$jour)
# met_dep <- as.character(1:95)
# met_dep[sapply(met_dep, nchar)==1] <- paste0("0", met_dep[sapply(met_dep, nchar)==1])
# data_FR2plot <- data_FR %>% filter(dep %in% met_dep) %>% select(jour, incid_hosp) %>%
#   group_by(jour) %>% summarise(hospitalisation_incident = sum(incid_hosp, na.rm=TRUE)) %>%
#   rename(date = jour)
#
# sursaud2plot <- sursaud_covid19 %>% filter(grepl("REG", maille_code)) %>%
#   select(date_de_passage, nbre_hospit_corona) %>%
#   group_by(date_de_passage) %>%
#   summarise(hospitalisation_incident = sum(nbre_hospit_corona, na.rm=TRUE)) %>%
#   rename(date = date_de_passage)
#
# #top_n(data_FR2plot, 5)
# #top_n(sursaud2plot,5)
# data_FR2plot$source <- "Santé Publique France"
# sursaud2plot$source <- "SurSaUD"
# data_FR2plot$H <- cumsum(data_FR2plot$hospitalisation_incident)
# sursaud2plot$H <- cumsum(sursaud2plot$hospitalisation_incident)
# data2plot <- rbind.data.frame(data_FR2plot, sursaud2plot)
#
# library(ggplot2)
# old.loc <- Sys.getlocale("LC_TIME")
# Sys.setlocale("LC_TIME", "en_GB.UTF-8")
# ggplot(data2plot %>% filter(H>0),
#        aes(x=date, y=H)) +
#   geom_point(aes(color = source)) +
#   #geom_col(position = position_dodge2(width = 0.9, preserve = "single"),aes(fill=source)) +
#   #scale_y_log10() +
#   colorspace::scale_color_discrete_diverging(name="Data source") +
#   colorspace::scale_fill_discrete_diverging(name="Data source") +
#   ylab("Cumulative incidence of COVID-19 hospitalizations") +
#   xlab("Date") +
#   theme_bw() +
#   NULL
# ggsave("SURSAUDvsSPF.pdf", height=4.5, width=7)
# Sys.setlocale("LC_TIME",old.loc)
