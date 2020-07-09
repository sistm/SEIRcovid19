#' Prepare data from Santé Publique France and Sursaud for monolix
#'
#' @import reshape2 dplyr
#' @export
#'
#' @examples
#' SPF_I_SURSAUD_H_covid19fr <- prepare_data_4monolix(update_from_source = FALSE)
#' save(SPF_I_SURSAUD_H_covid19fr, file = "SPF_I_SURSAUD_H_covid19fr.RData")
#'
prepare_data_4monolix <- function(update_from_source = TRUE){
  all <- get_data_covid19_bylevel(level = "region", source3 = "SPF", update_from_source = TRUE)

  prep1reg <- function(x){
  temp <- x %>%
    mutate(reg_id = maille_code, I = cas_confirmes_incident, H = hospitalisation_incident) %>%
    select(reg_id, date, day, I, H)

  I0 <- temp %>% top_n(-1, date) %>% pull(I)
  H0 <- temp %>% top_n(-1, date) %>% pull(H)
  if(is.na(I0) | I0 == 0){
    I0 <- 1
  }
  if(is.na(H0)){
    H0 <- 0
  }

  x_final <- temp %>%
    mutate(init_I0 = I0, init_H0 = H0) %>%
    reshape2::melt(id.vars=c("reg_id", "date", "day", "init_I0", "init_H0"), variable.name="obs_id",
                   value.name = "obs") %>%
    mutate(confinement = 1*(date > "2020-03-17"))
  }

  all_prep <- lapply(names(all), prep1reg)

  all_df <- do.call(rbind, all_prep)
  rownames(all_df) <- NULL
  all_df <- all_df[!is.na(all_df), ]

  return(all_df)
}
