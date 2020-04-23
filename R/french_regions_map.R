#' Map of French region
#'
#' @export
french_regions_map <- function(fill_info_df,
                               legend_name){

  library(ggplot2)
  library(maptools)
  library(dplyr)

  # library(rgdal)
  # region_shp <- rgdal::readOGR(
  #   dsn= "~/Downloads/regions-20180101-shp/" ,
  #   layer="regions-20180101",
  #   verbose=TRUE
  # )
  # region_shp@data$id <- rownames(region_shp@data)
  # reg_points <- ggplot2::fortify(region_shp, region="id")
  # reg_map_df <- dplyr::left_join(reg_points, region_shp@data, by="id")
  # reg_map_df <- reg_map_df %>% rename(name = nom)
  # reg_map_df$name <- as.character(reg_map_df$name)
  # save(reg_map_df, file = "data/FrenchRegionsMap.RData")
  data(FrenchRegionsMap)

  reg_map_df2plot <- reg_map_df %>% filter(name %in% c("Auvergne-Rhône-Alpes", "Bourgogne-Franche-Comté",
                                                 "Bretagne", "Centre-Val de Loire", "Grand Est",
                                                 "Hauts-de-France", "Île-de-France", "Normandie",
                                                 "Nouvelle-Aquitaine", "Occitanie",
                                                 "Pays de la Loire", "Provence-Alpes-Côte d'Azur")
  )

  reg_map_df2plot <- left_join(reg_map_df2plot, fill_info, by = c("name"))

  #subsampling at 1/30
  #sel <- c(TRUE, rep(FALSE, 29))
  sel <- TRUE

  p <- ggplot(reg_map_df2plot[sel,], aes(x = long, y = lat, group=group)) +
    geom_polygon(aes(fill=fill_value, color=fill_value), alpha=0.93, size=0.08) +
    scale_fill_gradientn(name = legend_name, colours = c("lightcyan", "skyblue1", "red3", "red4"),
                         breaks=c(0,2,4,6), minor_breaks=c(1,3,5),
                         labels = paste0(c(0,2,4,6), "%"),
                         limits = c(0,6)) +
    scale_color_gradientn(name = legend_name, colours = c("lightcyan", "skyblue1", "red3", "red4"),
                         breaks=c(0,2,4,6), minor_breaks=c(1,3,5),
                         labels = paste0(c(0,2,4,6), "%"),
                         limits = c(0,6)) +
    coord_map() +
    theme_void() +
    ggtitle("")

  return(p)
}

