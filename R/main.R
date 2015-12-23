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
create.heatwave.dataset <- function(out, dataFolder, citycsv,
                                    RorCPP = 1,
                                    IDheatwavesReplacement = FALSE,
                                    dataBoundaries = FALSE,
                                    referenceBoundaries = FALSE){

        # Check the parameters for errors
        parameterErrorChecking(out, dataFolder, citycsv, RorCPP, dataBoundaries, IDheatwavesReplacement, referenceBoundaries)

        # Put the directories into nested list form
        models <- acquireDirectoryStructure(dataFolder)

        # Read the cities data file
        cities <- read.csv(citycsv)

        # Create "global" list object that will hold variables that all functions that need then will have access to
        global <- listGlobal(out, dataFolder, cities, RorCPP)

        # Create the "custom" list object that will hold all of the user's custom settings.
        custom <- listCustom(IDheatwavesReplacement, dataBoundaries, referenceBoundaries)

        # TODO Think of a better name for these variables.
        accumulators <- createAccumulators()

        # Process the entire dataset
        referenceEnsembles <- sapply(models, processModel, global, custom, accumulators)

        # Write the model information from the model information accumulator
        writeAccumulator(accumulators("return model information"))

        # Make the map
        makeMap(accumulators("return locations"), cities)

        cat("All operations completed. Exiting.", "\n")
}

#' Error check for parameters of create.heatwave.dataframe
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
#' parameterErrorChecking(out, dataFolder, citycsv,
#'    referenceBoundaries = referenceBoundaries)
parameterErrorChecking <- function(out,
                                   dataFolder,
                                   citycsv,
                                   RorCPP = 1,
                                   IDheatwavesReplacement = FALSE,
                                   dataBoundaries = FALSE,
                                   referenceBoundaries){

        # TODO: ERROR CHECKING!!!!!!!!
        # Check to see if the folder that holds the climate data exists.
        workingDirectory <- getwd()
        tryCatch(
                setwd(dataFolder),
                error = function(){
                        stop("Pathway containing cmip5 data invalid. Stopping")
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
                        stop("City information .csv invalid. Stopping")
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

#' Creates lists for 'global' variables and custom data respectively.
#'
#' @param out Character string with pathway to directory to which
#'    heatwave files will be written.
#' @param dataFolder Character string with pathway to directory that
#'    contains climate projections. Must include the final backslash.
#' @param citycsv Character string giving the filepath to a .csv
#'    file with latitude and longitude values for each city.
#' @param RorCPP 0 /1 flag that indicates whether to use R (1) or
#'    CPP (0) functions.
#'
#' @return A list with slots for the output directory pathname,
#'    the pathname for the directory with the climate projections,
#'    the pathname with the csv with city latitudes and longitudes,
#'    and the RorCPP choices.
#'
#' @note These functions exist to give the user a way to create the
#'    required "global" and "custom" parameters required by the rest
#'    of the program without running the entire program.
#'
#' @examples
#' out <- "~/Downloads/sample/results"
#' dataFolder <- "~/Downloads/sample/cmip5/"
#' citycsv <- "inst/cities.csv"
#' RorCPP <- FALSE
#' listGlobal(out, dataFolder, citycsv, RorCPP)
listGlobal <- function(out = FALSE,
                       dataFolder = FALSE,
                       citycsv = FALSE,
                       RorCPP = FALSE){

        return(list("output" = out,
                    "data" = dataFolder,
                    "cities" = citycsv,
                    "RorCPP" = RorCPP))
}

#' Create list of custom settings.
#'
#' @param IDheatwavesReplacement Either FALSE, to use the default
#'    heatwave definition, or a user-specified custom function to
#'    use to identify heatwaves.
#' @param dataBoundaries Custom time boundaries for extracting data
#'    from the ensembles. Format: c(historical low bound, historical
#'    high bound, reference low bound, reference high bound).
#'    Restrictions: Bounds cannot span multiple experiments
#' @param referenceBoundaries Reference boundaries.
#'
#' @return A list with slots for custom settings specified by the user.
listCustom <- function(IDheatwavesReplacement = FALSE,
                       dataBoundaries = FALSE,
                       referenceBoundaries = FALSE){

        # TODO: can't remember what boolflag is/does. Review.
        boolflag <- FALSE
        if(referenceBoundaries != FALSE){
                boolflag <- TRUE
        }

        return(list("IDheatwaves" = IDheatwavesReplacement,
                    "getBounds" = dataBoundaries,
                    "processModel" = referenceBoundaries,
                    "createHwDataframe" = boolflag))
}

#' Create accumulators
#'
#' @return A closure that maintains data structures that track
#' information about the models and the location vectors for each
#' ensemble respectively.
createAccumulators <- function(){
        modelInfoAccumulator <- data.frame(c(), c())
        locationList <- list()

        function(command, newElement = FALSE){

                if(command == "return model information"){
                        return(modelInfoAccumulator)

                } else if(command == "append model information"){
                        modelInfoAccumulator <<- rbind(modelInfoAccumulator, newElement)

                } else if(command == "return locations"){
                        return(locationList)

                } else if(command == "append location list"){
                        locationList <<- list(locationList, newElement)
                }
        }
}
