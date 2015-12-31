#' Create heatwave dataset for a directory of climate projections
#'
#' @param out Character string with pathway to directory to which
#'    heatwave files will be written.
#' @param dataFolder Character string with pathway to directory that
#'    contains climate projections. Must include the final backslash.
#' @param citycsv Character string giving the filepath to a .csv
#'    file with latitude and longitude values for each city.
#' @param RorCPP 0 /1 flag that indicates whether to use R (1) or
#'    CPP (0) functions
#' @param IDheatwavesReplacement Either FALSE, to use the default
#'    heatwave definition, or a user-specified custom function to
#'    use to identify heatwaves.
#' @param dataBoundaries Custom time boundaries for extracting data
#'    from the ensembles. Format: c(historical low bound, historical
#'    high bound, reference low bound, reference high bound).
#'    Restrictions: Bounds cannot span multiple experiments
#' @param referenceBoundaries Reference boundaries.
#'
#' @return [What does this function return?]
gen_hw_set <- function(out, dataFolder, citycsv,
                                    RorCPP = 1,
                                    IDheatwavesReplacement = FALSE,
                                    dataBoundaries = FALSE,
                                    referenceBoundaries = FALSE,
                                    printWarning = TRUE,
                                    coordinateFilenames, 
                                    tasFilenames, 
                                    timeFilenames){
  
  #~
  # TODO: Note for later. Will delete
  # "latitude_longitude_NorthAmerica_12mo.csv",  "tas_NorthAmerica_12mo.csv",  "time_NorthAmerica_12mo.csv"
  #
  # ~
        # Add warning for user that this will write new files
        if(printWarning){
                cat("\n", "Warning: This function will write new files",
                    "to your computer in the ", "\n", out,
                    "directory of your computer.", "\n",
                    "Do you want to continue (y / n):", "\n")
                user_prompt <- scan(n = 1, what = "character")
                user_prompt <- tolower(user_prompt)
                if(user_prompt %in% c("n", "no")){
                        stop("User chose to exit function run.")
                }
        }

        # TODO: Fix the program such that this block is not required
        # If `dataFolder` does not end in "/", add it.
        split_dataFolder <- unlist(strsplit(dataFolder, split = ""))
        last_char <- split_dataFolder[length(split_dataFolder)]
        if(last_char != "/"){
                dataFolder <- paste0(dataFolder, "/")
        }

        # Check the parameters for errors
        check_params(out, dataFolder, citycsv, RorCPP,
                               dataBoundaries, IDheatwavesReplacement,
                               referenceBoundaries)

        # Put the directories into nested list form
        models <- acquireDirectoryStructure(dataFolder, coordinateFilenames, tasFilenames, timeFilenames)

        # Read the cities data file
        cities <- read.csv(citycsv)

        # Create "global" list object that will hold variables that all
        # functions that need then will have access to
        global <- list("output" = out,
                       "data" = dataFolder,
                       "cities" = cities,
                       "RorCPP" = RorCPP)

        # Create the "custom" list object that will hold all of the user's
        # custom settings.
        custom <- list("IDheatwaves" = IDheatwavesReplacement,
                       "getBounds" = dataBoundaries,
                       "processModel" = referenceBoundaries,
                       "createHwDataframe" = referenceBoundaries != FALSE)

        # TODO Think of a better name for these variables.
        accumulators <- createAccumulators()

        # Process the entire dataset
        referenceEnsembles <- sapply(models, processModel, global, custom,
                                     accumulators)

        # Write the model information from the model information accumulator
        writeAccumulator(accumulators("return model information"))

        # Make the map
        makeMap(accumulators("return locations"), cities)

        cat("All operations completed. Exiting.", "\n")
}

#' Error check for parameters of gen_hw_set
#'
#' @param out Character string with pathway to directory to which
#'    heatwave files will be written.
#' @param dataFolder Character string with pathway to directory that
#'    contains climate projections. Must include the final backslash.
#' @param citycsv Character string giving the filepath to a .csv
#'    file with latitude and longitude values for each city.
#' @param RorCPP 0 /1 flag that indicates whether to use R (1) or
#'    CPP (0) functions
#' @param IDheatwavesReplacement Either FALSE, to use the default
#'    heatwave definition, or a user-specified custom function to
#'    use to identify heatwaves.
#' @param dataBoundaries Custom time boundaries for extracting data
#'    from the ensembles. Format: c(historical low bound, historical
#'    high bound, reference low bound, reference high bound).
#'    Restrictions: Bounds cannot span multiple experiments
#' @param referenceBoundaries Reference boundaries.
#'
#' @return Stops and returns an error if any parameters are incorrect.
#'
#' @note Does not check if the data exists in the proper structure or
#'    if any data exists within the directory at all.
#'
#' @examples
#' out <- "~/Downloads/sample/results"
#' dataFolder <- "~/Downloads/sample/cmip5/"
#' citycsv <- "inst/cities.csv"
#' referenceBoundaries <- FALSE
#' check_params(out, dataFolder, citycsv,
#'    referenceBoundaries = referenceBoundaries)
check_params <- function(out,
                                   dataFolder,
                                   citycsv,
                                   RorCPP,
                                   IDheatwavesReplacement,
                                   dataBoundaries,
                                   referenceBoundaries){

        # TODO: ERROR CHECKING!!!!!!!!
        # Check to see if the folder that holds the climate data exists.
        workingDirectory <- getwd()
        tryCatch(
                # TODO: Replace setwd with Dr. Anderson's suggestion
                setwd(dataFolder),
                error = function(){
                        stop("Pathway containing cmip5 data (`dataFolder`) invalid. Stopping")
                },
                finally = {
                        setwd(workingDirectory)
                }
        )


        # Check if the city information .csv can be opened.
        # Note: Does not check if the city information is valid.
        tryCatch(
                read.csv(citycsv, header = TRUE),
                error = function(x){
                        stop("Cannot read city information .csv (`citycsv`). Stopping")
                }
        )

        # Check if the user has specified 1 or 0 for the RorCPP flag
        if( RorCPP != 1 & RorCPP != 0){
                stop("Invalid RorCPP flag value. Must enter 1 or 0. Stopping")
        }

        # TODO CHECK FOR BOUNDARY STRADDLING
        # STILL UNFINISHED!
        # The boundary checking still contains logical errors.
        # Must make sure that both upper and lower bounds for a period are FALSE if unspecified
        checkCustomBounds(dataBoundaries)
        checkCustomBounds(referenceBoundaries)
}

checkCustomBounds <- function(boundList){
        length = length(boundList)
        if(boundList != FALSE & length(boundList) != length){
                stop("boundList length not equal to length of user-specified boundary list. Stopping.")
        } else if(boundList != FALSE){
                histLow <- boundList[1]
                histHigh <- boundList[2]
                rcpLow <- boundList[3]
                rcpHigh <- boundList[4]

                if(histLow != FALSE){
                        if(histLow < 1981){
                                stop("Custom boundaries for threshold calculation fall out of acceptable range. Stopping")
                        }
                }

                if(histHigh != FALSE){
                        if(histHigh > 2004){
                                stop("Custom boundaries for threshold calculation fall out of acceptable range. Stopping")
                        }
                }

                if(rcpLow != FALSE){
                        if(rcpLow < 2061){
                                stop("Custom boundaries for threshold calculation fall out of acceptable range. Stopping")
                        }
                }

                if(rcpHigh != FALSE){
                        if(rcpHigh > 2080){
                                stop("Custom boundaries for threshold calculation fall out of acceptable range. Stopping")
                        }
                }
        }
}

# TODO: It may be possible to make the accumulator system extremely robust. Consider this possibility
#       after all else is finished.
#' Create accumulator closure
#' This closure holds data structures that the user wishes to grow at various
#' points in the execution of the package. It exists to couple these structures together
#' in order to lower the number of parameters of this nature the user would have to pass down
#' the the program otherwise.
#'
#' @return A closure that accepts commands to access and append new data onto 
#' data structures as the program executes.
createAccumulators <- function(){
        modelInfoAccumulator <- data.frame(c(), c())
        locationList <- list()

        function(command, newElement = FALSE){

                # Commands for model information accumulator
                if(command == "return model information"){
                        return(modelInfoAccumulator)

                } else if(command == "append model information"){
                        modelInfoAccumulator <<- rbind(modelInfoAccumulator, newElement)

                # Commands for location list accumulator
                } else if(command == "return locations"){
                        return(locationList)

                } else if(command == "append location list"){
                        locationList <<- list(locationList, newElement)
                }
          
          
        }
}
