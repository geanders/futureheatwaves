# Identify and characterize heat waves in the example data provided
# with the `futureheatwaves` package
library(futureheatwaves)
projection_dir_location <- system.file("extdata/cmip5",
                                       package = "futureheatwaves")
city_file_location <- system.file("extdata/cities.csv",
                                  package = "futureheatwaves")

gen_hw_set(out = "example_results",
           dataFolder = projection_dir_location ,
           dataDirectories = list("historical" = c(1990, 1999),
                                  "rcp85" = c(2060, 2079)),
           citycsv = city_file_location,
           coordinateFilenames = "latitude_longitude_NorthAmerica_12mo.csv",
           tasFilenames = "tas_NorthAmerica_12mo.csv",
           timeFilenames = "time_NorthAmerica_12mo.csv")

# Define and apply a function to measure mean temperature during
# heat waves in all the climate model ensemble members of the example
# data.
average_mean_temp <- function(hw_datafr){
        out <- mean(hw_datafr$mean.var)
        return(out)
}

out <- system.file("extdata/example_results", package = "futureheatwaves")

apply_all_models(out = out, FUN = average_mean_temp)
apply_all_models(out = out, FUN = average_mean_temp, city_specific = TRUE)

# Create an interactive leaflet map with study locations and matching
# climate model grid points for example data
map_grid_leaflet(plot_model = "bcc1", out = out)
