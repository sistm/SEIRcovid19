#' Map of French region
#'
#' @import ggplot2
#' @import dplyr
#' @import maptools
#'
#' @export
french_regions_map <- function(fill_info_df,
                               mytitle,
                               one_out_of=30,
                               show_labels = TRUE,
                               col_labels = "white",
                               size_labels = 5,
                               contour = NULL){

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
  reg_map_df2plot <- left_join(reg_map_df2plot, fill_info_df, by = c("name"))
  reg_map_centroids_df2plot <- left_join(reg_map_centroids_df2plot, fill_info_df, by = c("name"))
  #subsampling
  sel <- c(TRUE, rep(FALSE, times = one_out_of-1))

  p <- ggplot(data = reg_map_df2plot[sel,], aes(x = long, y = lat, group=group))

  if(is.null(contour)){
    p <- p +
      geom_polygon(aes(fill=fill_value, color=fill_value), alpha=0.93, size=0.08)
  }else{
    p <- p +
      geom_polygon(aes(fill=fill_value), color=contour, alpha=0.93, size=0.08)
  }
  p <- p +
    scale_fill_gradientn(name = "", colours = c("#F9FCFF", "#EEF8FF", "#E2FDFF", "#CCF5FF", "#ADE4FD", "#98D8FB", "#8DC3EC", "#B97790", "#BB0000", "#8B0000"),
                         breaks=c(0,5,10),
                         labels = paste0(c(0,5,10), "%"),
                         limits = c(0,10)) +
    scale_color_gradientn(name = "", colours = c("#F9FCFF", "#EEF8FF", "#E2FDFF", "#CCF5FF", "#ADE4FD", "#98D8FB", "#8DC3EC", "#B97790", "#BB0000", "#8B0000"),
                          breaks=c(0,5,10),
                          labels = paste0(c(0,5,10), "%"),
                          limits = c(0,10)) +
    coord_map() +
    ggtitle(mytitle) +
    theme_void() +
    theme(legend.text = element_text(size=15),
          legend.position = "bottom") +
    theme(plot.title = element_text(hjust=0.5))

          if(show_labels){
            p <- p +
              geom_text(data = reg_map_centroids_df2plot, aes(label=paste(format(round(fill_value,1),nsmall=1),"%",sep="")),
                        size=size_labels, vjust=0.7)
          }
          return(p)
}

