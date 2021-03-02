#' @import readxl
#' @export
getDataSIVIC<-function(file,workingdirectory,level){
  sivicdrees_tb <- readxl::read_xlsx(file, sheet = level)
    namecol<-substr(level, 1,3)

    sivicdrees_tb <- sivicdrees_tb %>%
    select(ends_with(namecol), date, statut, ends_with("_mean"))
  sivicdrees_tb$date <- as.Date(sivicdrees_tb$date)

  deces <- sivicdrees_tb %>% filter(statut == "Décès") %>%
    mutate("death_inflow_from_rea" = predict_from_rea_mean,
           "death_inflow_from_hospit" = predict_entrees_mean - predict_from_rea_mean) %>%
    select(-starts_with("predict_"), -statut)
  rad <- sivicdrees_tb %>% filter(statut == "Retour à domicile") %>%
   mutate("recovered_inflow_from_rea" = predict_from_rea_mean,
         "recovered_inflow_from_hospit" = predict_entrees_mean - predict_from_rea_mean) %>%
    select(-starts_with("predict_"), -statut)
  rea <- sivicdrees_tb %>% filter(statut == "Réanimation/SI") %>%
    mutate("ICU_inflow" = predict_entrees_mean,
          "ICU_outflow" = predict_sorties_mean) %>%
   select(-starts_with("predict_"), -statut)
  hospit <- sivicdrees_tb %>% filter(statut == "Toutes hospitalisations") %>%
   mutate("hospit_inflow" = predict_entrees_mean,
          "hospit_outflow" = predict_sorties_mean) %>%
   select(-starts_with("predict_"), -statut)

  sivic_dress <- deces %>% full_join(rad, by = c(namecol, "date")) %>%
    full_join(rea, by = c(namecol, "date")) %>%
    full_join(hospit, by = c(namecol, "date"))

  sivic_dress<-as.data.frame(sivic_dress)

  write.table(sivic_dress,file=paste(workingdirectory,"SIVIC_",level,"_",date,".txt",sep=""),sep="\t",row.names = F,quote=F)

  return(sivic_dress)
  }


getDataCIRE<-function(file,workingdirectory){

  cire_tb <- readxl::read_xlsx(file, sheet = "suivi_cas_conf_NA")
  cire_tb[cire_tb$Nb == 2237, "département site preleveur"] <- 33
  cire_tb[cire_tb$Nb == 3872, "département site preleveur"] <- 33

  cire_tb <- cire_tb %>%
    select(-Nb, -sexe, -`date de naissance`, -age, -CP, -COMMUNE, -`labo test`,
           -`site preleveur`, -`date début des symptomes`,
           -`rea (0ui/n0n)`, -`hospitalisation (0ui/n0n/s0rti)`) %>%
    rename(dep = `Département de résidence`) %>%
    mutate(date = as.Date(date_labo)) %>%
    select(-date_labo) %>%
    select(-EHPAD) %>%
    group_by(date, dep) %>%
    arrange(date, dep)


  departements_NA <- c("16", "17", "19", "23", "24", "33", "40", "47", "64", "79", "86", "87")
  cire_tb[!(cire_tb$dep %in% departements_NA) & (cire_tb$`département site preleveur` %in% departements_NA), "dep"] <-
    cire_tb[!(cire_tb$dep %in% departements_NA) & (cire_tb$`département site preleveur` %in% departements_NA), "département site preleveur"]
  cire_tb[is.na(cire_tb$dep), "dep"] <- cire_tb[is.na(cire_tb$dep), "département site preleveur"]

  cire_tb <- cire_tb[!is.na(cire_tb$dep), ]

  cire_tb <- cire_tb %>%
    select(-`département site preleveur`) %>%
    summarise(PCRpositive_inflow = n())
  cire_tb$dep <- as.character(cire_tb$dep)

  date_cire <- seq.Date(from = min(sivic_dress$date), to = max(cire_tb$date), by=1)
  cire_df <- cbind.data.frame(dep = rep(departements_NA, times=length(date_cire)),
                              date = rep(date_cire, each=length(departements_NA)),
                              PCRpositive_inflow = 0)
  cire_df <- left_join(cire_df, cire_tb, by=c("dep", "date"))
  cire_df <- cire_df %>%
    mutate(PCRpositive_inflow = rowSums(cire_df[, c("PCRpositive_inflow.x", "PCRpositive_inflow.y")], na.rm=TRUE)) %>%
    select(-PCRpositive_inflow.x, -PCRpositive_inflow.y)

  cire_df <- cire_df %>%
    group_by(dep) %>%
    arrange(date) %>%
    mutate(PCRpositive_cumul = cumsum(PCRpositive_inflow)) %>%
    arrange(dep)

}

