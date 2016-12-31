#' Create a map of model grid
#'
#' This function creates a map of the grid points of a climate model used for
#' the study locations and draws lines connecting each study city to its climate
#' model grid point. It currently can only be used for studies within the
#' United States.
#'
#' @param plot_model A character string with the name of the model to plot
#' @inheritParams apply_all_models
#'
#' @return A \code{ggplot2} object with a map of grid points for the climate
#'    model that were used in processing extreme events for the study locations,
#'    with a line drawn from each study locations to the grid point used for
#'    it.
#'
#' @note This function creates a \code{ggplot2} object, so the output can be
#'    edited using \code{ggplot2} functions.
#'
#' @examples
#' out <- system.file("extdata/example_results", package = "futureheatwaves")
#' map_grid(plot_model = "bcc1", out = out)
#'
#' @export
#'
#' @importFrom dplyr %>%
map_grid <- function(plot_model, out){
        cities <- utils::read.csv(paste(out, "locationList.csv", sep = "/"),
                           col.names = c("city","lat", "lon",
                                         "lat_grid", "lon_grid", "model")) %>%
                dplyr::mutate_(lon = ~ lon - 360,
                               lon_grid = ~ lon_grid - 360) %>%
                dplyr::filter_(~ model == plot_model)

        latlong <- unique(cities[ , c("lon_grid", "lat_grid")])
        states <- ggplot2::map_data("state")

        map <- ggplot2::ggplot()
        map <- map + ggplot2::geom_polygon(data = states,
                                  ggplot2::aes_string(x = "long", y = "lat",
                                               group = "group"),
                                  colour = "lightgray", fill = "white")
        map <- map + ggplot2::geom_point(data = latlong,
                                         ggplot2::aes_string(x = "lon_grid",
                                                             y = "lat_grid"),
                                         color = 132, alpha = 0.6)
        map <- map + ggplot2::geom_segment(data = cities,
                                           ggplot2::aes_string(x = "lon",
                                                               y = "lat",
                                                               xend = "lon_grid",
                                                               yend = "lat_grid"),
                                  size = 0.9, alpha = 0.6, color = 132)
        map <- map + ggplot2::coord_map("albers", lat0 = 30, lat1 = 40) +
                ggthemes::theme_map()
        map <- map + ggplot2::ggtitle(plot_model)
        return(map)
}

#' Create an interactive map of model grid
#'
#' This function creates an interactive map of the grid points of a
#' climate model used for the study locations and draws lines connecting each
#' study city to its climate model grid point. This map is made using the
#' \code{leaflet} package.
#'
#' @inheritParams map_grid
#' @inheritParams apply_all_models
#'
#' @return A \code{leaflet} object with a map of grid points for the climate
#'    model that were used in processing extreme events for the study locations,
#'    with a line drawn from each study locations to the grid point used for
#'    it. This map can be explored interactively.
#'
#' @examples
#' out <- system.file("extdata/example_results", package = "futureheatwaves")
#' map_grid_leaflet(plot_model = "bcc1", out = out)
#'
#' @export
#'
#' @importFrom dplyr %>%
map_grid_leaflet <- function(plot_model, out){
        cities <- utils::read.csv(paste(out, "locationList.csv", sep = "/"),
                                  col.names = c("city","lat", "lon",
                                                "lat_grid", "lon_grid",
                                                "model")) %>%
                dplyr::filter_(~ model == plot_model)

        cities <- cities %>%
                dplyr::mutate_(city_popup = ~ paste0("<b>City:</b> ",
                                                     city,
                                                     "<br/>",
                                                     "<b>Latitude:</b> ",
                                                     lat,
                                                     "<br/>",
                                                     "<b>Longitude:</b> ",
                                                     lon))

        grid_data <- cities %>%
                dplyr::select_(~ lat_grid, ~ lon_grid, ~ model) %>%
                dplyr::distinct_() %>%
                dplyr::mutate_(grid_popup = ~ paste0("<b>Latitude:</b> ",
                                                     lat_grid,
                                                     "<br/>",
                                                     "<b>Longitude:</b> ",
                                                     lon_grid,
                                                     "<br/>",
                                                     "<b>Model:</b> ",
                                                     model))

        map_lines <- cities %>%
                dplyr::select_(~ city, ~ lat, ~ lon, ~ lat_grid, ~ lon_grid) %>%
                tidyr::gather_(key_col = "key", value_col = "value",
                               gather_cols = c("lat", "lon", "lat_grid",
                                               "lon_grid")) %>%
                dplyr::mutate_(city_or_grid =
                                       ~ ifelse(stringr::str_detect(key, "grid"),
                                                "grid", "city"),
                               key = ~ gsub("_grid", "", key)) %>%
                tidyr::spread_(key_col = "key", value_col = "value")

        out_map <- leaflet::leaflet() %>%
                leaflet::addTiles() %>%
                leaflet::addCircleMarkers(data = cities, lng = ~lon, lat = ~lat,
                                    popup = ~ city_popup) %>%
                leaflet::addCircleMarkers(data = grid_data,
                                          lng = ~lon_grid, lat = ~lat_grid,
                                          color = "red", popup = ~ grid_popup)
        for(map_city in unique(map_lines$city)){
                out_map <- out_map %>%
                        leaflet::addPolylines(data = dplyr::filter_(map_lines,
                                                            ~ city == map_city),
                                     lat = ~lat,
                                     lng = ~lon)
        }
        return(out_map)
}

