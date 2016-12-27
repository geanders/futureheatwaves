#' Process city file
#'
#' This function takes the dataframe read in from the file specified with
#' \code{citycsv} in \code{\link{gen_hw_set}} and renames the columns for
#' latitude and longitude \code{lat} and \code{lon}, based on
#' the user's selections in \code{lat_lon_colnames} for
#' \code{\link{gen_hw_set}}. If there are extra columns besides those and the
#' \code{city} column, this function removes them.
#'
#' @param cities Dataframe with study cities and their latitudes
#'    and longitudes.The dataframe must have a column named \code{city} with a
#'    unique identifier for each city in the study, as well as columns for
#'    latitude and longitude. Other columns may be included in the dataset, but
#'    will not be passed through to later code.
#' @inheritParams gen_hw_set
#'
#' @return A processed version of the latitude and longitude dataframe.
#'
#' @importFrom dplyr %>%
process_cities_file <- function(cities, lat_lon_colnames){

        if(!all(lat_lon_colnames %in% colnames(cities))) {
                stop(paste("The `lat_lon_colnames` that you specified are not",
                           "column names in the `cities` dataframe."))
        }

        cities <- cities %>%
                dplyr::select_(~ city,
                              lat = ~ dplyr::matches(lat_lon_colnames[1]),
                              lon = ~ dplyr::matches(lat_lon_colnames[2]))

        return(cities)
}

#' Read latitude and longitude data
#'
#' This function reads in data on the longitudes and latitudes of a climate
#' model's grid pooints from a locally-stored comma-separated file located within
#' the directory specified by the \code{dataFolder} argument of
#' \code{\link{gen_hw_set}}.
#'
#' @param ensemble Character vector that includes
#'    the file paths to (1) the latitude and longitude file; (2) the
#'    climate projection file; and (3) the projection dates file
#'    for the selected climate model.
#' @inheritParams processModel
#'
#' @return A dataframe of the latitude and longitude data of the ensemble
#'    with columns for the latitude (first column) and longitude (second
#'    column) of each grid point in the climate model.
readLatLong <- function(ensemble, global){
        loc_file <- grep(global$coordinateFilenames, ensemble)
        locations <- utils::read.csv(ensemble[loc_file], header = FALSE)
        return(locations)
}

# readLatLong_ncdf <- function(ensemble, global){
#         loc_file <- "~/Downloads/tas_day_GFDL-ESM2G_historical_r1i1p1_19810101-19851231.nc"
#         ncdf_file <- ncdf4::nc_open(loc_file)
#         ncdf_lon <- ncdf4::ncvar_get(ncdf_file, "lon")
#         ncdf_lat <- ncdf4::ncvar_get(ncdf_file, "lat")
#         ncdf4::nc_close(ncdf_file)
#         locations <- expand.grid(ncdf_lat, ncdf_lon)
#         colnames(locations) <- c("V1", "V2")
#         return(locations)
# }

#' Read climate projection data
#'
#' This function reads climate projection data for a climate
#' model into R from a locally-stored comma-separated file located within
#' the directory specified by the \code{dataFolder} argument of
#' \code{\link{gen_hw_set}}.
#'
#' @param locations A numerical vector giving the column numbers that
#'    correspond to the closest grid point location for each study
#'    location in the temperature input data.
#' @inheritParams readLatLong
#' @inheritParams processModel
#'
#' @return A dataframe of climate projection data where each column
#'    corresponds to a climate model grid point and each row corresponds
#'    to a date of observation.
readtas <- function(ensemble, global, locations){
        loc_file <- grep(global$tasFilenames, ensemble)
        tas <- data.table::fread(ensemble[loc_file],
                                 select = unique(locations),
                                 header = FALSE)
        tas <- tas[ , paste0("V", locations)]
        return(tas)
}

#' Read projection dates data
#'
#' This function reads the dates corresponding to climate projection data for
#' a climate model into R from a locally-stored comma-separated file located
#' within the directory specified by the \code{dataFolder} argument of
#' \code{\link{gen_hw_set}}.
#'
#' @inheritParams readLatLong
#' @inheritParams processModel
#'
#' @return A dataframe of dates corresponding to climate projection data,
#'    where each row gives a date, split into columns of day in the
#'    dataframe, four-digit year, numeric month, and numeric day.
readTimes <- function(ensemble, global){
        loc_file <- grep(global$timeFilenames, ensemble)
        return(utils::read.csv(ensemble[loc_file], header = FALSE))
}

# readTimes_ncdf <- function(ensemble, global){
#         library(PCICt)
#         loc_file <- "~/Downloads/tas_day_GFDL-ESM2G_historical_r1i1p1_19810101-19851231.nc"
#         ncdf_file <- ncdf4::nc_open(loc_file)
#         ncdf_time <- ncdf4.helpers::nc.get.time.series(ncdf_file, v = "tas", time.dim.name = "time")
#         ncdf4::nc_close(ncdf_file)
#         times <- data.frame(V1 = 1:length(ncdf_time),
#                                 V2 = as.integer(format(ncdf_time, "%Y")),
#                                 V3 = as.integer(format(ncdf_time, "%m")),
#                                 V4 = as.integer(format(ncdf_time, "%d")))
#         return(times)
# }

#' Ensemble writer factory function
#'
#' This function creates a closure that writes a single heat wave list to a
#' comma-separated file in the directory specified by the user in the
#' \code{out} argument of \code{\link{gen_hw_set}}.
#'
#' The closure created by this function closes over an incrementer variable
#' for ensembles that advances each time the closure is called.
#'
#' @inheritParams buildStructureModels
#' @inheritParams processModel
#'
#' @return A closure that inputs \code{hwFrame}, a combined heat wave dataframe
#' with all heat wave information for all cities for the ensemble
#' and writes out a heat wave dataframe to the output directory specified by
#' the \code{out} argument in \code{\link{gen_hw_set}}.
#'
#' @importFrom dplyr %>%
createEnsembleWriter <- function(modelName, global, custom){
        # Incrementer
        i <- 1

        writePath <- paste0(global$output, "Heatwaves/Projections/")

        function(hwFrame){

                cat("Writing ", modelName, ": r", i, "i", i, "p", i,
                    "\n", sep = "")

                # Create the directory that the file will be written to
                dir.create(paste(writePath, modelName, sep = ""),
                           showWarnings = FALSE, recursive = TRUE)

                # Write the file
                utils::write.csv(hwFrame,
                          file = paste0(writePath, modelName, "/", i, ".csv"),
                          row.names = FALSE)

                # Increment the ensemble number
                i <<- i + 1
        }
}

#' Write model information to file
#'
#' This function writes out a dataframe that accumulates information on all
#' climate models included in the analysis, including the number of
#' ensemble members in each of the two experiments.
#'
#' @param modelInfoAccumulator A dataframe with information about the number of
#'    ensembles for each climate model.
#' @param locationList A dataframe with information about city locations
#'    and the closest grid point from each climate model to each city.
#' @inheritParams processModel
#'
#' @return This function writes out a dataframe with information on all
#' climate models included in the analysis, including the number of
#' ensemble members in each of the two experiments.
writeAccumulators <- function(modelInfoAccumulator,
                             locationList,
                             global){
        colnames(modelInfoAccumulator)[1] <- "Models"
        colnames(modelInfoAccumulator)[2] <- "# of historical ensembles"
        colnames(modelInfoAccumulator)[3] <- "# of future projection ensembles"

        cat("Writing accumulators", "\n")
        writePath <- global$output
        utils::write.csv(modelInfoAccumulator,
                  file = paste0(writePath, "hwModelInfo", ".csv"),
                  row.names = FALSE)
        utils::write.csv(locationList,
                  file = paste0(writePath, "locationList", ".csv"),
                  row.names = FALSE)
}
