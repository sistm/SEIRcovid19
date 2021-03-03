#' full_region_names
#'
#' @description Function to recode ugly region labels to nice region labels.
#'
#' @param x A factor vector
#'
#' @return A factor vector of the same length as provided with nice labels
#' @export
#'
#' @examples
#' vec_ugly <- c("IDF", "NAquitaine", "AURA",
#' "Centre", "BFC", "Normandie", "HDF",
#' "GrandEst", "PaysLoire", "Bretagne",
#' "Occitanie", "PACA")
#' 
#' full_region_names(as.factor(vec_ugly))
#' 
#' 
full_region_names  <-  function(x){
  fct_recode(x,
             "\u00cele-de-France"="IDF",
             "Nouvelle-Aquitaine" = "NAquitaine",
             "Auvergne-Rh\u00f4ne-Alpes" = "AURA",
             "Centre-Val de Loire" = "Centre",
             "Bourgogne-Franche-Comt\u00e9" = "BFC",
             "Normandie" = "Normandie",
             "Hauts-de-France" = "HDF",
             "Grand Est" = "GrandEst",
             "Pays de la Loire" = "PaysLoire",
             "Bretagne" = "Bretagne",
             "Occitanie" = "Occitanie",
             "Provence-Alpes-C\u00f4te d'Azur" = "PACA"
  )
}
