## ----echo = FALSE, message = FALSE---------------------------------------
library(futureheatwaves)

## ------------------------------------------------------------------------
system.file("cities.csv", package = "futureheatwaves")

## ------------------------------------------------------------------------
system.file("cmip5", package = "futureheatwaves")

## ----eval = FALSE--------------------------------------------------------
#  gen_hw_set(out = "example_results",
#             dataFolder = "cmip5" ,
#             citycsv = "cities.csv",
#             coordinateFilenames = "latitude_longitude_NorthAmerica_12mo.csv",
#             tasFilenames = "tas_NorthAmerica_12mo.csv",
#             timeFilenames = "time_NorthAmerica_12mo.csv")

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

## ---- eval = FALSE-------------------------------------------------------
#  gen_hw_set(out = "example_results",
#             dataFolder = "cmip5" ,
#             citycsv = "cities.csv",
#             coordinateFilenames = "latitude_longitude_NorthAmerica_12mo.csv",
#             tasFilenames = "tas_NorthAmerica_12mo.csv",
#             timeFilenames = "time_NorthAmerica_12mo.csv",
#             IDheatwavesFunction = "IDHeatwavesAlternative")

## ---- eval = FALSE-------------------------------------------------------
#  gen_hw_set(out = "example_results",
#             dataFolder = "cmip5" ,
#             citycsv = "cities.csv",
#             coordinateFilenames = "latitude_longitude_NorthAmerica_12mo.csv",
#             tasFilenames = "tas_NorthAmerica_12mo.csv",
#             timeFilenames = "time_NorthAmerica_12mo.csv",
#             projectionBoundaries = c(2071, 2090))

## ---- eval = FALSE-------------------------------------------------------
#  gen_hw_set(out = "example_results",
#             dataFolder = "cmip5" ,
#             citycsv = "cities.csv",
#             coordinateFilenames = "latitude_longitude_NorthAmerica_12mo.csv",
#             tasFilenames = "tas_NorthAmerica_12mo.csv",
#             timeFilenames = "time_NorthAmerica_12mo.csv",
#             thresholdBoundaries = c(2061, 2080))

## ---- eval = FALSE-------------------------------------------------------
#  gen_hw_set(out = "example_results",
#             dataFolder = "cmip5" ,
#             citycsv = "cities.csv",
#             coordinateFilenames = "latitude_longitude_NorthAmerica_12mo.csv",
#             tasFilenames = "tas_NorthAmerica_12mo.csv",
#             timeFilenames = "time_NorthAmerica_12mo.csv",
#             referenceBoundaries = c(2031, 2040))

