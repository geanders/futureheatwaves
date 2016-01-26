## ----echo = FALSE, message = FALSE---------------------------------------
library(futureheatwaves)

## ------------------------------------------------------------------------
dataFolder <- "inst/cmip5/" 
citycsv <- "inst/cities.csv"

## ------------------------------------------------------------------------
coordinateFilenames <- "latitude_longitude_NorthAmerica_12mo.csv"
tasFilenames <- "tas_NorthAmerica_12mo.csv"
timeFilenames <- "time_NorthAmerica_12mo.csv"

## ------------------------------------------------------------------------
out <- "~/tmp/example_results/"

## ----eval = FALSE--------------------------------------------------------
#  gen_hw_set(out = out,
#             dataFolder = dataFolder,
#             citycsv = citycsv,
#             coordinateFilenames = coordinateFilenames,
#             tasFilenames = tasFilenames,
#             timeFilenames = timeFilenames,
#             RorCPP = 0)

## ------------------------------------------------------------------------
dataFolder <- system.file("cmip5", package = "futureheatwaves")
finalList <- acquireDirectoryStructure(dataFolder = dataFolder,
   coordinateFilenames = coordinateFilenames,
   tasFilenames = tasFilenames,
   timeFilenames = timeFilenames)

str(finalList[[1]][[1]])
str(finalList[[1]][[2]][1])

