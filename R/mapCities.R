#' Create a map of model grid
#'
#' @param plot_model A character string with the name of the model to plot
#' @inheritParams gen_hw_set
#'
#' @examples
#' out <- system.file(package = "futureheatwaves")
#' map_grid(plot_model = "bcc1", out = out)
#'
#' @export
#'
#' @importFrom dplyr %>%
map_grid <- function(plot_model, out){
        cities <- read.csv(paste(out, "locationList.csv", sep = "/")) %>%
                dplyr::mutate(long = long - 360,
                             long_grid = long_grid - 360) %>%
                dplyr::filter(model == plot_model)

        latlong <- unique(cities[ , c("long_grid", "lat_grid")])
        states <- ggplot2::map_data("state")

        map <- ggplot2::ggplot()
        map <- map + ggplot2::geom_polygon(data = states,
                                  ggplot2::aes(x = long, y = lat, group = group),
                                  colour = "lightgray", fill = "white")
        map <- map + ggplot2::geom_point(data = latlong,
                                         ggplot2::aes(x = long_grid, y = lat_grid),
                                         color = 132, alpha = 0.6)
        map <- map + ggplot2::geom_segment(data = cities,
                                           ggplot2::aes(x = long, y = lat,
                                                     xend = long_grid,
                                                     yend = lat_grid),
                                  size = 0.9, alpha = 0.6, color = 132)
        map <- map + ggplot2::coord_map("albers", lat0=30, lat1=40) +
                ggthemes::theme_map()
        map <- map + ggplot2::ggtitle(plot_model)
        return(map)
}
