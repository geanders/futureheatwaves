# IO.R
#
# All functions that partake in I/O are here.

# Read the ensemble
readLatLong <- function(ensemble){
        return(read.csv(ensemble[2], header = FALSE))
}
readtas <- function(ensemble){
        return(read.csv(ensemble[3], header = FALSE))
}
readTimes <- function(ensemble){
        return(read.csv(ensemble[4], header = FALSE))
}

# Write the temperature cutoffs acquired from the r1i1p1 ensemble for each model to a .csv
writeThresholds <- function(modelName, threshLow, thresholds, global, custom){

        writePath <- paste0(global$output, "Thresholds/")

        # Create datastructure that will be written
        writeThis <- data.frame(global$cities[,1], thresholds)

        # Create the directory that the file will be written to
        dir.create(paste(writePath, modelName, sep = ""), showWarnings = FALSE, recursive = TRUE, mode = "0777")

        # Write the file
        write.csv(writeThis, file = paste(writePath, modelName, "/", threshLow, ".csv", sep = ''))
}

# ensembleWriterFactory Produces a function that writes a single heatwave list to a .csv
# Closes over an incrementer variable for ensembles that advances forward each time the produced function is called.
createEnsembleWriter <- function(modelName, global, custom){
        # Incrementer
        i <- 1

        writePath <- paste0(global$output, "Heatwaves/rcp 8.5/")

        function(hwFrame){

                cat("Writing ", modelName, " r", i, "i", i, "p", i, "\n", sep = "")

                # TODO: Check this!
                # Write minimum threshold temperatures of each city to a file
                writeThresholds(modelName, 'minimums', ddply(hwFrame, "city", summarize, min = min(min.temp)), global)

                # Create the directory that the file will be written to
                dir.create(paste(writePath, modelName, sep = ""), showWarnings = FALSE, recursive = TRUE, mode = "0777")

                # Write the file
                write.csv(hwFrame, file = paste0(writePath, modelName, "/", i, ".csv"), row.names = FALSE)

                # Increment the ensemble number
                i <<- i + 1
        }
}

# Output the variable that accumulates information on the models and ensemble counts
writeAccumulator <- function(modelInfoAccumulator, global){
        cat("Writing accumulator", "\n")
        writePath <- global("output")
        write.csv(modelInfoAccumulator, file = paste0(writePath, "hwModelInfo", ".csv"), row.names = FALSE)
}
