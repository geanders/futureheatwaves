#' Create a map of model grid
#'
#' @param plot_model A character string with the name of the model to plot
#' @inheritParams gen_hw_set
#'
#' @export
#'
#' @importFrom dplyr %>%
map_grid <- function(plot_model, out){
        cities <- read.csv(paste(out, "locationList.csv", sep = "/")) %>%
                mutate(long = long - 360,
                       long_grid = long_grid - 360) %>%
                filter(model == plot_model)

        latlong <- unique(cities[ , c("long_grid", "lat_grid")])
        states <- map_data("state")

        map <- ggplot()
        map <- map + geom_polygon(data = states,
                                  aes(x = long, y = lat, group = group),
                                  colour = "lightgray", fill = "white")
        map <- map + geom_point(data = latlong,
                                aes(x = long_grid, y = lat_grid),
                                color = 132, alpha = 0.6)
        map <- map + geom_segment(data = cities, aes(x = long, y = lat,
                                                     xend = long_grid,
                                                     yend = lat_grid),
                                  size = 0.9, alpha = 0.6, color = 132)
        map <- map + coord_map("albers", lat0=30, lat1=40) + theme_map()
        map <- map + ggtitle(plot_model)
        return(map)
}
