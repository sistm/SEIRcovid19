#' Emergency Covid-19 numbers
#'
#'A dataset containing the  Emergency Covid-19 related counts in France during
#'the Covid epidemics as of 2020-04-21 at 19:00 (Paris time).
#'
#' @format A data frame with 6,897 rows and 7 variables:
#' \describe{
#'   \item{maille_cd}{administrative code}
#'   \item{date_de_passage}{Date of ER visits}
#'   \item{nbre_pass_tot}{Total number of ER visits}
#'   \item{nbre_pass_corona}{Number of ER visits for Covid-19 suspicion}
#'   \item{nbre_hospit_corona}{Number of Hospitalization for Covid-19 suspicion}
#'   \item{nbre_acte_corona}{Number of 'acte SOS medecin' for Covid-19 suspicion}
#'   \item{nbre_acte_tot}{Total number of 'acte SOS medecin'}
#' }
#'
#'@examples
#'data(sursaud_covid19)
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
#'@source SURSAUD database share by Santé Publique France \url{https://www.data.gouv.fr/fr/datasets/r/941ff2b4-ea24-4cdf-b0a7-655f2a332fb2}
#'stored in \code{'data/raw/sursaud-covid19-quotidien-2020-04-21-19h00.xlsx'}
#'
#'
"sursaud_covid19"

