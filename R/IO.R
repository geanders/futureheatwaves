#' Read the latitude and longitude data for a given ensemble
#'
#' @param ensemble Ensemble component of the directory structure that contains the path
#' to the latitude and longitude data to be read
#'
#' @return A dataframe of the latitude and longitude data of the ensemble
readLatLong <- function(ensemble){
        return(read.csv(ensemble[2], header = FALSE))
}

#' Read the time series data for a given ensemble
#'
#' @param ensemble Ensemble component of the directory structure that contains the path
#' to the time series data to be read
#'
#' @return A dataframe of the time series data of the ensemble
readtas <- function(ensemble){
        return(read.csv(ensemble[3], header = FALSE))
}

#' Read the date data for a given ensemble
#'
#' @param ensemble Ensemble component of the directory structure that contains the path
#' to the date data to be read
#'
#' @return A dataframe of the date data of the ensemble
readTimes <- function(ensemble){
        return(read.csv(ensemble[4], header = FALSE))
}

#' Write temperature thresholds to file
#'
#' This function creates a dataframe with a column with names of each of the
#' user-specified communities and a column with each community's threshold
#' temperature, as determine from the climate projections for the reference
#' period and the user-specified percentile threshold. These files are written
#' out to a directory called "Thresholds" within the user-specified output
#' directory (as specified in \code{\link{gen_hw_set}}). Within "Thresholds",
#' there will be a subdirectory for each of the climate models.
#'
#' @param threshOut Character string with the file name to use for the file
#'    the function writes out with threshold temperatures for each community
#'    for the model.
#' @inheritParams buildStructureModels
#' @inheritParams processProjections
#' @inheritParams processModel
writeThresholds <- function(modelName, threshOut, thresholds, global, custom){

        writePath <- paste0(global$output, "Thresholds/")

        # Create datastructure that will be written
        writeThis <- data.frame(global$cities[,1], thresholds)

        # Create the directory that the file will be written to
        dir.create(paste(writePath, modelName, sep = ""),
                   showWarnings = FALSE, recursive = TRUE, mode = "0777")

        # Write the file
        write.csv(writeThis, file = paste(writePath, modelName,
                                          "/", threshOut, ".csv", sep = ''))
}

#' Ensemble writer factory function
#'
#' Produces a function that writes a single heatwave list to a .csv
#'
#' Closes over an incrementer variable for ensembles that advances forward
#' each time the produced function is called.
#' Output the heatwave data of each model. Produces a closure that must be
#' applied to a list of the heatwave dataframes corresponding to the
#' ensembles of the model
#'
#' @inheritParams buildStructureModels
#' @inheritParams processModel
#'
#' @return A closure that writes heatwave dataframes
#' Argument 1: A combined heatwave dataframe that contains all heatwave
#' information for the ensemble being processed.
#'
#' @importFrom dplyr %>%
createEnsembleWriter <- function(modelName, global, custom){
        # Incrementer
        i <- 1

        writePath <- paste0(global$output, "Heatwaves/Projections/")

        function(hwFrame){

                cat("Writing ", modelName, ": r", i, "i", i, "p", i,
                    "\n", sep = "")

                # TODO: Contradiction detected. writeThresholds is called each time the closure is called
                # even though the documentation for writeThresholds states it is only supposed to be called
                # for the r1i1p1 ensemble. Make adjustments.
                # Write minimum threshold temperatures of each city to a file
                thresholds <- dplyr::group_by(hwFrame, city) %>%
                        dplyr::summarize(min = min(min.temp))
                writeThresholds(modelName, 'minimums',
                                thresholds, global, custom)

                # Create the directory that the file will be written to
                dir.create(paste(writePath, modelName, sep = ""),
                           showWarnings = FALSE, recursive = TRUE,
                           mode = "0777")

                # Write the file
                write.csv(hwFrame,
                          file = paste0(writePath, modelName, "/", i, ".csv"),
                          row.names = FALSE)

                # Increment the ensemble number
                i <<- i + 1
        }
}

#' Write out model information
#'
#' Output the variable that accumulates information on the models and
#' ensemble counts
#'
#' @param modelInfoAccumulator The dataframe that accumulates the number of
#'    ensembles for each model.
#' @inheritParams processModel
writeAccumulator <- function(modelInfoAccumulator, global){
        colnames(modelInfoAccumulator)[1] <- "Models"
        colnames(modelInfoAccumulator)[2] <- "# of historical ensembles"
        colnames(modelInfoAccumulator)[3] <- "# of future projection ensembles"
        cat("Writing accumulator", "\n")
        writePath <- global$output
        write.csv(modelInfoAccumulator,
                  file = paste0(writePath, "hwModelInfo", ".csv"),
                  row.names = FALSE)
}
