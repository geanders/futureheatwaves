## ----echo = FALSE, message = FALSE---------------------------------------
library(futureheatwaves)

## ------------------------------------------------------------------------
system.file("cities.csv", package = "futureheatwaves")

## ------------------------------------------------------------------------
system.file("cmip5", package = "futureheatwaves")

## ----eval = FALSE--------------------------------------------------------
#  projection_dir_location <- system.file("cmip5", package = "futureheatwaves")
#  city_file_location <- system.file("cities.csv", package = "futureheatwaves")
#  
#  gen_hw_set(out = "example_results",
#             dataFolder = projection_dir_location ,
#             dataDirectories = list("historical" = c(1980, 2004),
#                                          "rcp85" = c(2006, 2099)),
#             citycsv = city_file_location,
#             coordinateFilenames = "latitude_longitude_NorthAmerica_12mo.csv",
#             tasFilenames = "tas_NorthAmerica_12mo.csv",
#             timeFilenames = "time_NorthAmerica_12mo.csv",
#             lat_lon_colnames = c("lat", "long"))

## ------------------------------------------------------------------------
data(hw_datafr)

## ------------------------------------------------------------------------
hw_datafr[1:3, c("hw.number", "mean.temp", "length", "start.date",
                 "mean.temp.quantile", "city")]

## ---- eval = FALSE-------------------------------------------------------
#  # Define the function
#  average_length <- function(hw_datafr){
#          ave_length <- mean(hw_datafr$length)
#          return(ave_length)
#  }

## ---- eval = FALSE-------------------------------------------------------
#  # Apply across all heatwave dataframes from all ensemble members
#  apply_all_models(out = "results",
#                   FUN = average_length)

## ---- eval = FALSE-------------------------------------------------------
#  apply_all_models(out = "results",
#                   FUN = average_length,
#                   city_specific = TRUE)

## ---- eval = FALSE-------------------------------------------------------
#  average_mean_temp

## ----eval = FALSE--------------------------------------------------------
#  data(hw_datafr)

## ----fig.width = 5, message = FALSE, warning = FALSE---------------------
out <- system.file(package = "futureheatwaves")
map_grid(plot_model = "bcc1", out = out)

## ----fig.width = 5, fig.height = 7---------------------------------------
library(gridExtra)
a <- map_grid(plot_model = "bcc1", out = out)
b <- map_grid(plot_model = "ccsm", out = out)
grid.arrange(a, b, ncol = 1)

## ------------------------------------------------------------------------
data(datafr)

## ------------------------------------------------------------------------
head(datafr, 3)
id_of_hws <- IDHeatwavesR(datafr = datafr, threshold = 95)
head(id_of_hws, 3)

## ---- eval = FALSE-------------------------------------------------------
#  # Set up path names to example data
#  projection_dir_location <- system.file("cmip5", package = "futureheatwaves")
#  city_file_location <- system.file("cities.csv", package = "futureheatwaves")
#  
#  gen_hw_set(out = "example_results",
#             dataFolder = projection_dir_location ,
#             dataDirectories = list("historical" = c(1980, 2004),
#                                          "rcp85" = c(2006, 2099)),
#             citycsv = city_file_location,
#             coordinateFilenames = "latitude_longitude_NorthAmerica_12mo.csv",
#             tasFilenames = "tas_NorthAmerica_12mo.csv",
#             timeFilenames = "time_NorthAmerica_12mo.csv",
#             lat_lon_colnames = c("lat", "long"),
#             IDheatwavesFunction = "IDHeatwavesAlternative")

## ---- eval = FALSE-------------------------------------------------------
#  gen_hw_set(out = "example_results",
#             dataFolder = projection_dir_location ,
#             dataDirectories = list("historical" = c(1980, 2004),
#                                          "rcp85" = c(2006, 2099)),
#             citycsv = city_file_location,
#             coordinateFilenames = "latitude_longitude_NorthAmerica_12mo.csv",
#             tasFilenames = "tas_NorthAmerica_12mo.csv",
#             timeFilenames = "time_NorthAmerica_12mo.csv",
#             lat_lon_colnames = c("lat", "long"),
#             IDheatwavesFunction = "IDHeatwavesAlternative",
#             projectionBoundaries = c(2071, 2090))

## ---- eval = FALSE-------------------------------------------------------
#  gen_hw_set(out = "example_results",
#             dataFolder = projection_dir_location ,
#             dataDirectories = list("historical" = c(1980, 2004),
#                                          "rcp85" = c(2006, 2099)),
#             citycsv = city_file_location,
#             coordinateFilenames = "latitude_longitude_NorthAmerica_12mo.csv",
#             tasFilenames = "tas_NorthAmerica_12mo.csv",
#             timeFilenames = "time_NorthAmerica_12mo.csv",
#             lat_lon_colnames = c("lat", "long"),
#             IDheatwavesFunction = "IDHeatwavesAlternative",
#             thresholdBoundaries = c(2061, 2080))

## ---- eval = FALSE-------------------------------------------------------
#  gen_hw_set(out = "example_results",
#             dataFolder = projection_dir_location ,
#             dataDirectories = list("historical" = c(1980, 2004),
#                                          "rcp85" = c(2006, 2099)),
#             citycsv = city_file_location,
#             coordinateFilenames = "latitude_longitude_NorthAmerica_12mo.csv",
#             tasFilenames = "tas_NorthAmerica_12mo.csv",
#             timeFilenames = "time_NorthAmerica_12mo.csv",
#             lat_lon_colnames = c("lat", "long"),
#             IDheatwavesFunction = "IDHeatwavesAlternative",
#             referencdeBoundaries = c(2031, 2040))

