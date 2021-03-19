#' ImportSivic
#'
#' @description Import SIVIC data, datamanage it and return a nice dataframe.
#'
#' @param file The sivic file.
#' @param level Level of interest. Default is "regions".
#' @param REG_selected The area selected.
#' @param recuperation The recuperation argument passed to prepare4Monolix().
#' @param nametokeep The nametokeep argument passed to prepare4Monolix().
#'
#' @return A dataframe with SIVIC data.
#' @export
#'
#' @examples
#' \dontrun{
#' sivic <- ImportSivic(file = "data/sivic_panel_reg_latest (7).csv")
#' save(sivic, file = "data/sivic.rdata")
#' }
ImportSivic <- function(file,
                        level = "regions",
                        REG_selected = c("REG-11", "REG-24", "REG-27",
                                         "REG-28","REG-32", "REG-44",
                                         "REG-52", "REG-53", "REG-75",
                                         "REG-76", "REG-84", "REG-93",
                                         "DEP-16", "DEP-17", "DEP-19",
                                         "DEP-23", "DEP-24", "DEP-33",
                                         "DEP-40", "DEP-47", "DEP-64",
                                         "DEP-79", "DEP-86", "DEP-87"),
                        recuperation = c("hospit_current", "hospit_inflow"),
                        nametokeep = c("popsize", "epidemicsStart", "day",
                                       "lockdown", "timeSinceConf", "timeSinceDeConf",
                                       "rsent", "Dh")){

  SIVIC_REG<-getDataSIVIC(file = file,
                          level=level)

  dfResult <- getPopulationSize(SIVIC_REG) %>%
    getrsent() %>%
    filter(id %in% REG_selected) %>%
    left_join(SEIRcovid19FR::biom_kalman, by = c("id"="id")) %>%
    getEpidemicsStarts(.) %>%
    #DATA_TO_WORK<-getlockdownInfo(DATA_TO_WORK)
    getlockdownInfoReconf(.) %>%
    prepare4Monolix(.,recuperation,nametokeep)

  return(dfResult)
}