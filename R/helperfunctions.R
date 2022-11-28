#' A function to plot a metric for a set of cities or areas of interest in facets
#'
#' @base_dt the sf dataframe containing basemap geometries
#' @overlay_dt the sf dataframe containing the data to be overlayed on `base_dt`
#' @plot_title the title for the plot
#' @ind_colname call name of the indicator to be plotted
#' @area_colname the column name containing the groups to be faceted
#' @filename the filename to store the results (including file extension like xxx.png or xxx.pdf)

ggplot_multareas <- function(base_dt,
                             overlay_dt,
                             plot_title,
                             ind_colname,
                             area_colname,
                             filename){

  ggplot() +
    geom_sf(data = base_dt) +
    geom_sf(data = overlay_dt,
            aes(fill = ind_colname)) +
    scale_fill_viridis_c(option = "plasma") +
    facet_wrap(paste0("~", area_colname))
    ggtitle(plot_title) +
    theme(plot.title = element_text(hjust = 0.5))
  ggsave(filename = filename)



}
