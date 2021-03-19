#' getDataSIVIC
#'
#' @description Get the SIVIC data and perform some cleaning.
#'
#' @param file The raw sivic data file.
#' @param level The level of interest. Default is 'region'.
#'
#' @return A dataframe with the following columns: id, date death_inflow_from_rea,
#' death_inflow_from_hospit, death_inflow, death_cumul, hospit_inflow, hospit_outflow,
#' hospit_current
#'
#' @importFrom stringr str_replace
#' @importFrom utils read.csv
#' @import dplyr
#'
#' @examples
#' \dontrun{
#' SIVIC_REG<-getDataSIVIC(file = "sivic_panel_reg_latest (7).csv")
#' }
getDataSIVIC <- function(file,
                         level = "region"){

  namecol<-substr(level, 1,3)

  sivicdrees_tb <-read.csv(file) %>%
    select(ends_with(namecol), date, statut, ends_with("_mean")) %>%
    mutate(date = as.Date(date))

  deces <- sivicdrees_tb %>%
    filter(statut == "D\u00E9c\u00E8s") %>%
    mutate("death_inflow_from_rea" = predict_from_rea_mean,
           "death_inflow_from_hospit" = predict_entrees_mean - predict_from_rea_mean) %>%
    select(-starts_with("predict_"), -statut)

  deces$death_inflow<-deces$death_inflow_from_rea+deces$death_inflow_from_hospit
  deces<-deces[order(deces[,namecol], deces[,"date"]),]
  deces$death_cumul<-NA
  deces$death_cumul[1]<-deces$death_inflow[1]
  for(i in 2:length(deces$death_inflow)){
    if(deces[i,namecol]==deces[i-1,namecol]){
      deces$death_cumul[i]<-deces$death_cumul[i-1]+deces$death_inflow[i]
    }else{
      deces$death_cumul[i]<-deces$death_inflow[i]
    }
  }

  hospit <- sivicdrees_tb %>% filter(statut == "Toutes hospitalisations") %>%
    mutate("hospit_inflow" = predict_entrees_mean,
           "hospit_outflow" = predict_sorties_mean) %>%
    select(-starts_with("predict_"), -statut)
  hospit<-hospit[order(hospit[,namecol], hospit[,"date"]),]
  hospit$hospi_current<-NA
  hospit$hospi_current[1]=0
  for (i in 2:length(hospit$date)){
    if(hospit[i,namecol]==hospit[i-1,namecol]){
      hospit$hospi_current[i]<-max(0,hospit$hospi_current[i-1]+hospit$hospit_inflow[i-1]-hospit$hospit_outflow[i-1])
    }else{
      hospit$hospi_current[i]<-0
    }
  }

  sivic_dress <- deces %>%
    full_join(hospit, by = c(namecol, "date"))
  sivic_dress<-as.data.frame(sivic_dress)

  if(namecol=="reg"){
    sivic_dress[,namecol]<-paste("REG-",str_replace(format(sivic_dress[,namecol],width=2)," ", "0"),sep="")

  }else{
    sivic_dress[,namecol]<-paste("DEP-",sivic_dress[,namecol],sep="")
  }


  names(sivic_dress)<-c("id",	"date","death_inflow_from_rea",	"death_inflow_from_hospit",	"death_inflow",	"death_cumul","hospit_inflow"	,"hospit_outflow", "hospit_current"	)

  return(sivic_dress)
}
