#' Geodes Sante Publique France
#'
#'@examples
#'data(geodesSPF)
#'
#' # # Fromatting data ----
#' # library(dplyr)
#'
#' # ## Regions ----
#' # path <- "data/raw/sursaud-covid19-quotidien-2020-04-21-19h00.xlsx"
#' # sursaud_covid19_reg <- readxl::read_xlsx(path,
#' #                                         sheet="reg"
#' # )
#' # sursaud_covid19_reg_tot <- sursaud_covid19_reg %>%
#' #    filter(sursaud_cl_age_corona == 0) %>%
#' #    select(reg, date_de_passage, nbre_pass_corona,
#' #            nbre_pass_tot, nbre_hospit_corona, nbre_acte_corona,
#' #            nbre_acte_tot) %>%
#' #    mutate(date_de_passage = lubridate::as_date(date_de_passage))
#' #
#'
#' # sursaud_covid19_reg_tot_ord <- sursaud_covid19_reg_tot %>%
#' #   arrange(reg, date_de_passage)
#' #
#' # sursaud_covid19_reg_tot_ord$reg <- paste0("REG-", sursaud_covid19_reg_tot_ord$reg)
#' # colnames(sursaud_covid19_reg_tot_ord)[1] <- "maille_code"
#'
#' # ## Departements ----
#' # sursaud_covid19_dpt <- readxl::read_xlsx(path,
#' #                                          sheet="dep"
#' # )
#' # sursaud_covid19_dpt_tot <- sursaud_covid19_reg %>%
#' #    filter(sursaud_cl_age_corona == 0) %>%
#' #    select(reg, date_de_passage, nbre_pass_corona,
#' #            nbre_pass_tot, nbre_hospit_corona, nbre_acte_corona,
#' #            nbre_acte_tot) %>%
#' #    mutate(date_de_passage = lubridate::as_date(date_de_passage))
#' #
#' # sursaud_covid19_dpt_tot_ord <- sursaud_covid19_dpt_tot %>%
#' #   arrange(dep, date_de_passage)
#' #
#' # sursaud_covid19_dpt_tot_ord$dep <- paste0("DPT-", sursaud_covid19_dpt_tot_ord$dep)
#' # colnames(sursaud_covid19_dpt_tot_ord)[1] <- "maille_code"
#' #
#' # sursaud_covid19 <- as.data.frame(rbind.data.frame(sursaud_covid19_reg_tot_ord,
#' # sursaud_covid19_dpt_tot_ord))
#' # save(sursaud_covid19, file = "data/sursaud_covid19.RData")
#'
#'@source datagouv ? geodesSPF <- read.table("./data/geodesSPF.csv", header=TRUE, sep=";")
#'
#'
#'
"geodesSPF"

