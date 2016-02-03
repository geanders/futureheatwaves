#' Read latitude and longitude data
#'
#' This function reads data on the longitudes and latitudes of a climate
#' model into R from a locally-stored comma-separated file located within
#' the directory specified by the \code{dataFolder} argument of
#' \code{\link{gen_hw_set}}.
#'
#' @param ensemble Character vector that includes
#'    the file paths to (1) the latitude and longitude file; (2) the
#'    climate projection file; and (3) the projection dates file
#'    for the selected climate model.
#'
#' @return A dataframe of the latitude and longitude data of the ensemble
#'    with columns for the latitude (first column) and longitude (second
#'    column) of each grid point in the climate model. This dataframe
#'    uses a format where the location of New York City, for example, is
#'    (40.7127, 285.9941).
readLatLong <- function(ensemble, global){
        loc_file <- grep(global$coordinateFilenames, ensemble)
        return(read.csv(ensemble[loc_file], header = FALSE))
}

#' Read climate projection data
#'
#' This function reads climate projection data for a climate
#' model into R from a locally-stored comma-separated file located within
#' the directory specified by the \code{dataFolder} argument of
#' \code{\link{gen_hw_set}}.
#'
#' @inheritParams readLatLong
#'
#' @return A dataframe of climate projection data where each column
#'    corresponds to a climate model grid point and each row corresponds
#'    to a date of observation.
readtas <- function(ensemble, global){
        loc_file <- grep(global$tasFilenames, ensemble)
        return(read.csv(ensemble[loc_file], header = FALSE))
}

#' Read projection dates data
#'
#' This function reads the dates corresponding to climate projection data for
#' a climate model into R from a locally-stored comma-separated file located
#' within the directory specified by the \code{dataFolder} argument of
#' \code{\link{gen_hw_set}}.
#'
#' @inheritParams readLatLong
#'
#' @return A dataframe of dates corresponding to climate projection data,
#'    where each row gives a date, split into columns of day in the
#'    dataframe, four-digit year, numeric month, and numeric day.
readTimes <- function(ensemble, global){
        loc_file <- grep(global$timeFilenames, ensemble)
        return(read.csv(ensemble[loc_file], header = FALSE))
}

#' Ensemble writer factory function
#'
#' This function creates a closure that writes a single heatwave list to a
#' comma-separated file in the directory specified by the user in the
#' \code{out} argument of \code{\link{gen_hw_set}}.
#'
#' The closure created by this function closes over an incrementer variable
#' for ensembles that advances each time the closure is called.
#' Output the heatwave data of each model.
#'
#' @note The closure produced by this function must be
#' applied to a list of the heatwave dataframes corresponding to the
#' ensembles of the model
#'
#' @inheritParams buildStructureModels
#' @inheritParams processModel
#'
#' @return A closure that writes heatwave dataframes. This closure has
#' the argument \code{hwFrame}: A combined heatwave dataframe that contains
#' all heatwave information for the ensemble being processed.
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
                write.csv(hwFrame,
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
#' historical (1980--2004) and future (2006--2099) ensemble members.
#'
#' @param modelInfoAccumulator The dataframe that accumulates the number of
#'    ensembles for each model.
#' @inheritParams processModel
#'
#' @return ...
writeAccumulators <- function(modelInfoAccumulator,
                             locationList,
                             global){
        colnames(modelInfoAccumulator)[1] <- "Models"
        colnames(modelInfoAccumulator)[2] <- "# of historical ensembles"
        colnames(modelInfoAccumulator)[3] <- "# of future projection ensembles"

        cat("Writing accumulators", "\n")
        writePath <- global$output
        write.csv(modelInfoAccumulator,
                  file = paste0(writePath, "hwModelInfo", ".csv"),
                  row.names = FALSE)
        write.csv(locationList,
                  file = paste0(writePath, "locationList", ".csv"),
                  row.names = FALSE)
}
