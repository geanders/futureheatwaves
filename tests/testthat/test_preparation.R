library(futureheatwaves)
context("Acquiring directory structure")

dataFolder <- system.file("cmip5", package = "futureheatwaves")
coordinateFilenames <- "latitude_longitude_NorthAmerica_12mo.csv"
tasFilenames <- "tas_NorthAmerica_12mo.csv"
timeFilenames <- "time_NorthAmerica_12mo.csv"

mods_1 <- "bcc1"
mods_2 <- "bcc1-a"
mods_3 <- c("bcc1", "bcc1-a")

finalList <- acquireDirectoryStructure(dataFolder = dataFolder,
                                       coordinateFilenames = coordinateFilenames,
                                       tasFilenames = tasFilenames,
                                       timeFilenames = timeFilenames,
                                       models_to_run = "all")

finalList_1 <- acquireDirectoryStructure(dataFolder = dataFolder,
                                       coordinateFilenames = coordinateFilenames,
                                       tasFilenames = tasFilenames,
                                       timeFilenames = timeFilenames,
                                       models_to_run = mods_1)

finalList_2 <- acquireDirectoryStructure(dataFolder = dataFolder,
                                         coordinateFilenames = coordinateFilenames,
                                         tasFilenames = tasFilenames,
                                         timeFilenames = timeFilenames,
                                         models_to_run = mods_2)

finalList_3 <- acquireDirectoryStructure(dataFolder = dataFolder,
                                         coordinateFilenames = coordinateFilenames,
                                         tasFilenames = tasFilenames,
                                         timeFilenames = timeFilenames,
                                         models_to_run = mods_3)

test_that("User can specify which models to run", {
        expect_equal(sapply(finalList, FUN = function(x) x[[1]]), c("bcc1", "bcc1-a"))
        # expect_equal(sapply(finalList_1, FUN = function(x) x[[1]]), "bcc1")
})
