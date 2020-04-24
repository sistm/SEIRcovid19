#' Map of French region
#'
#' @export
french_regions_map <- function(fill_info_df,
                               mytitle,
                               one_out_of=30,
                               show_labels = TRUE,
                               col_labels = "white",
                               size_labels = 5){

  library(ggplot2)
  library(maptools)
  library(dplyr)

  # library(rgdal)
  # region_shp <- rgdal::readOGR(
  #   dsn= "~/Downloads/regions-20180101-shp/" ,
  #   layer="regions-20180101",
  #   verbose=TRUE
  # )
  # reg_map_centroids_df <- as.data.frame(coordinates(region_shp))
  # colnames(reg_map_centroids_df) <- c("long", "lat")
  # reg_map_centroids_df$id <- rownames(region_shp@data)
  # reg_map_centroids_df$name <- as.character(region_shp@data$nom)
  # region_shp@data$id <- rownames(region_shp@data)
  # reg_points <- ggplot2::fortify(region_shp, region="id")
  # reg_map_df <- dplyr::left_join(reg_points, region_shp@data, by="id")
  # reg_map_df <- reg_map_df %>% rename(name = nom)
  # reg_map_df$name <- as.character(reg_map_df$name)
  # save(reg_map_df, reg_map_centroids_df, file = "data/FrenchRegionsMap.RData")
  data(FrenchRegionsMap)

  reg_map_df2plot <- reg_map_df %>% filter(name %in% c("Auvergne-Rhône-Alpes", "Bourgogne-Franche-Comté",
                                                       "Bretagne", "Centre-Val de Loire", "Grand Est",
                                                       "Hauts-de-France", "Île-de-France", "Normandie",
                                                       "Nouvelle-Aquitaine", "Occitanie",
                                                       "Pays de la Loire", "Provence-Alpes-Côte d'Azur")
  )
  reg_map_centroids_df2plot <- reg_map_centroids_df %>% filter(name %in% c("Auvergne-Rhône-Alpes", "Bourgogne-Franche-Comté",
                                                                           "Bretagne", "Centre-Val de Loire", "Grand Est",
                                                                           "Hauts-de-France", "Île-de-France", "Normandie",
                                                                           "Nouvelle-Aquitaine", "Occitanie",
                                                                           "Pays de la Loire", "Provence-Alpes-Côte d'Azur")
  )
  reg_map_centroids_df2plot <- reg_map_centroids_df2plot %>% rename(group = id)
  reg_map_df2plot <- left_join(reg_map_df2plot, fill_info, by = c("name"))
  reg_map_centroids_df2plot <- left_join(reg_map_centroids_df2plot, fill_info, by = c("name"))
  #subsampling
  sel <- c(TRUE, rep(FALSE, times = one_out_of-1))

  p <- ggplot(reg_map_df2plot[sel,], aes(x = long, y = lat, group=group)) +
    geom_polygon(aes(fill=fill_value, color=fill_value), alpha=0.93, size=0.08) +
    scale_fill_gradientn(name = "", colours = c("aliceblue", "lightcyan", "lightskyblue", "red4"), #c("white", "lightcyan", "lightblue1", "lightskyblue", "red3", "red4"),
                         breaks=c(0,2,4,6), minor_breaks=c(1,3,5),
                         labels = paste0(c(0,2,4,6), "%"),
                         limits = c(0,6)) +
    scale_color_gradientn(name = "", colours = c("aliceblue", "lightcyan", "lightskyblue", "red4"),
                          breaks=c(0,2,4,6), minor_breaks=c(1,3,5),
                          labels = paste0(c(0,2,4,6), "%"),
                          limits = c(0,6)) +
    coord_map() +
    ggtitle(mytitle) +
    theme_void() +
    theme(legend.text = element_text(size=15),
          legend.position = "bottom") +
    theme(plot.title = element_text(hjust=0.5))

  if(show_labels){
    p <- p +
      new_scale("color") +
      geom_text(data = reg_map_centroids_df2plot, aes(label=format(fill_value),
                                                             color=fill_value>5),
                       size=size_labels, vjust=0.7) +
      scale_color_manual(guide = FALSE, values = c("black", "white"))
  }
  return(p)
}

