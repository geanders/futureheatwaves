#~
# TODO: Note for later. Will delete
# "latitude_longitude_NorthAmerica_12mo.csv",  "tas_NorthAmerica_12mo.csv",
# "time_NorthAmerica_12mo.csv"
#
# TODO: Analyze the dataBoundaries and referenceBoundaries variables to make
# sure they are consistent with program requirements.
# ~

#' Create and write heatwave projections
#'
#' This function creates heatwave projection datasets for all models and
#'    ensemble members in a directory of climate projections for a specified
#'    set of communities.The resulting heatwave projections are written out
#'    to a specified directory on the user's local computer.
#'
#' @param out Character string with pathway to directory to which
#'    heatwave files will be written.
#' @param dataFolder Character string with pathway to directory that
#'    contains climate projection data.
#' @param citycsv Character string giving the filepath to a .csv
#'    file with latitude and longitude values for each city.
#' @param coordinateFilenames Character string with name of the files
#'    containing the latitude and longitude coordinates
#'    corresponding to the columns of the time series data.
#' @param tasFilenames Character sting with name of files containing the time
#'    series data.
#' @param timeFilenames Character string with name of the files containing the
#'    date information corresponding to the rows of the time series data.
#' @param RorCPP 0 /1 flag that indicates whether to use R (0) or
#'    C++ (1) function to identify heatwaves in the projections
#' @param IDheatwavesReplacement Either FALSE, to use the default
#'    heatwave definition, or a user-specified custom function to
#'    use to identify heatwaves.
#' @param dataBoundaries Custom time boundaries for extracting data
#'    from the ensembles. Format: c(historical low bound, historical
#'    high bound, reference low bound, reference high bound).
#'    Restrictions: Bounds cannot span multiple experiments
#' @param referenceBoundaries Custom time boundaries for ... .
#'    The first two values give the year range of the dataset to use to
#'    determine the threshold temperatures of the heatwave definition.
#'    Format: c(historical low bound, historical
#'    high bound, reference low bound, reference high bound).
#'    Restrictions: Bounds cannot span multiple experiments
#' @param probThreshold Numerical value between 0 and 1 specifying the threshold
#'    of temperature to use when defining heatwaves. The default value is 0.98
#'    (i.e., a heatwave is two or more days above the community's 98th
#'    percentile temperature).
#' @param printWarning TRUE / FALSE specifying whether to print out the progress
#'    of the function as it runs. Default is TRUE.
#'
#' @return This function returns a dataframe listing the name of each climate
#'    model included in the directory of projection files inputted to the
#'    function as well as the number of historical and future projection
#'    ensembles included for each model. This output can be used as a check
#'    that the function processed through the directory of input files
#'    specified using the \code{dataFolder} argument.
#'
#' @export
gen_hw_set <- function(out,
                       dataFolder,
                       citycsv,
                       coordinateFilenames,
                       tasFilenames,
                       timeFilenames,
                       RorCPP = 1,
                       IDheatwavesReplacement = FALSE,
                       dataBoundaries = FALSE,
                       referenceBoundaries = FALSE,
                       probThreshold = 0.98,
                       printWarning = TRUE){

        # If `dataFolder` does not end in "/", add it.
        split_dataFolder <- unlist(strsplit(dataFolder, split = ""))
        last_char <- split_dataFolder[length(split_dataFolder)]
        if(last_char != "/"){
                dataFolder <- paste0(dataFolder, "/")
        }

        # If `out` does not end in "/", add it.
        split_out <- unlist(strsplit(out, split = ""))
        last_char <- split_out[length(split_out)]
        if(last_char != "/"){
                out <- paste0(out, "/")
        }

        # Check the parameters for errors
        check_params(out = out,
                     dataFolder = dataFolder,
                     citycsv = citycsv,
                     coordinateFilenames = coordinateFilenames,
                     tasFilenames = tasFilenames,
                     timeFilenames = timeFilenames,
                     RorCPP = RorCPP,
                     IDheatwavesReplacement = IDheatwavesReplacement,
                     dataBoundaries = dataBoundaries,
                     referenceBoundaries = referenceBoundaries)

  # Add warning for user that this will write new files
        if(printWarning){
                cat("\n", "Warning: This function will write new files",
                    "to your computer in the \n", out,
                    "directory of your computer.\n",
                    "Do you want to continue? (y / n): \n")
                user_prompt <- scan(n = 1, what = "character")
                user_prompt <- tolower(user_prompt)
                if(user_prompt %in% c("n", "no")){
                        stop("User chose to exit at prompt.")
                }
        }

        # Put the directories into nested list form
        models <- acquireDirectoryStructure(dataFolder, coordinateFilenames,
                                            tasFilenames, timeFilenames)

        # Read the cities data file
        cities <- read.csv(citycsv)

        # Create "global" list object that will hold variables that all
        # functions that need then will have access to
        global <- list("output" = out,
                       "data" = dataFolder,
                       "cities" = cities,
                       "coordinateFilenames" = coordinateFilenames,
                       "tasFilenames" = tasFilenames,
                       "timeFilenames" = timeFilenames,
                       "RorCPP" = RorCPP)

        # Create the "custom" list object that will hold all of the user's
        # custom settings.
        custom <- list("IDheatwaves" = IDheatwavesReplacement,
                       "getBounds" = dataBoundaries,
                       "processModel" = referenceBoundaries,
                       "createHwDataframe" = referenceBoundaries != FALSE,
                       "probThreshold" = probThreshold)

        # Create accumulator closure
        accumulators <- createAccumulators()

        # Process the entire dataset
        referenceEnsembles <- sapply(models, processModel, global, custom,
                                     accumulators)

        # Write the model information from the model information accumulator
        out <- accumulators("return model information")
        writeAccumulator(accumulators("return model information"), global)

        # Make the map
        # makeMap(accumulators("return locations"), cities)
        cat("All operations completed. Exiting.", "\n\n")
        return(out)
}

#' Check for input parameter errors
#'
#' This function goes through all parameter inputs for the main
#'    functions, \code{gen_hw_set}, and makes sure all parameter
#'    entries are in the appropriate format for following functions.
#'    If any parameters are in an incorrect format, the function stops
#'    and returns an error describing the problem.
#'
#' @param out Character string with pathway to directory to which
#'    heatwave files will be written.
#' @param dataFolder Character string with pathway to directory that
#'    contains climate projections. Must include the final backslash.
#' @param citycsv Character string giving the filepath to a .csv
#'    file with latitude and longitude values for each city.
#' @param coordinateFilenames Character string with name of the files
#'    containing the latitude and longitude coordinates
#'    corresponding to the columns of the time series data.
#' @param tasFilenames Character sting with name of files containing the time
#'    series data.
#' @param timeFilenames Character string with name of the files containing the
#'    date information corresponding to the rows of the time series data.
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
#' @note This function does not check if the data is organized in the proper
#'    structure or if any data exists within the directory at all, so a
#'    call to \code{gen_hw_set} could still pass through this check and
#'    make it further through the function code with those mistakes.
#' @note Does not check if the three ensemble final .csv data files exist,
#'    only if they have the .csv extension if they do exist. (Reminder:
#'    the final subdirectory should have the following three csv files:
#'    1. A file with the climate model projections, with grid points by
#'    column and times by row; 2. A file with the longitude and latitude
#'    of each grid point in the projection file and; 3. A file with the
#'    date of each of the rows in the projection file.)
#'
#' @examples
#' out <- "~/tmp/results"
#' dataFolder <- "inst/cmip5/"
#' citycsv <- "inst/cities.csv"
#' coordinateFilenames <- "latitude_longitude_NorthAmerica_12mo.csv"
#' tasFilenames <- "tas_NorthAmerica_12mo.csv"
#' timeFilenames <- "time_NorthAmerica_12mo.csv"
#' RorCPP <- 1
#' IDheatwavesReplacement <- FALSE
#' dataBoundaries <- FALSE
#' referenceBoundaries <- FALSE
#'
#' check_params(out, dataFolder, citycsv,
#'    coordinateFilenames, tasFilenames, timeFilenames,
#'    RorCPP, IDheatwavesReplacement,
#'    dataBoundaries, referenceBoundaries)
check_params <- function(out,
                         dataFolder,
                         citycsv,
                         coordinateFilenames,
                         tasFilenames,
                         timeFilenames,
                         RorCPP,
                         IDheatwavesReplacement,
                         dataBoundaries,
                         referenceBoundaries){

        # TODO: ERROR CHECKING!!!!!!!!
        # Check to see if the folder that holds the climate data exists.
        tryCatch(
                dir.exists(dataFolder),
                error = function(){
                        stop(paste("Pathway containing projection data",
                                   "(`dataFolder`) invalid. Stopping."))
                },
                finally = {}
        )

        # Check if the city information .csv can be opened.
        # Note: Does not check if the city information is valid.
        tryCatch(
                read.csv(citycsv, header = TRUE),
                error = function(x){
                        stop(paste("Cannot read city information .csv",
                                   "(`citycsv`). Stopping."))
                }
        )

        # Check if the user has specified 1 or 0 for the RorCPP flag
        if( RorCPP != 1 & RorCPP != 0){
                stop(paste("Invalid RorCPP flag value. Must be 1 or 0.",
                           "Stopping."))
        }

        # Check 'Filenames' parameters for .csv extension.
        if(!grepl(".csv", coordinateFilenames)){
                stop("Invalid format: coordinateFilenames. Stopping.")
        }
        if(!grepl(".csv", tasFilenames)){
                stop("Invalid format: tasFilenames. Stopping.")
        }
        if(!grepl(".csv", timeFilenames)){
                stop("Invalid format: timeFilenames. Stopping.")
        }

        # TODO CHECK FOR BOUNDARY STRADDLING
        # STILL UNFINISHED!
        # The boundary checking still contains logical errors.
        # Must make sure that both upper and lower bounds for a period are
        # FALSE if unspecified
        checkCustomBounds(dataBoundaries)
        checkCustomBounds(referenceBoundaries)
}

#' Check year boundaries for errors
#'
#' This function inputs the boundary lists specified in \code{gen_hw_set},
#'    \code{dataBoundaries} and \code{referenceBoundaries}, and checks them
#'    for errors in structure of the input or in the years selected.
#'
#' @param boundList Either FALSE, if user wishes to use the default
#'    boundaries, or a set of boundary years in the format
#'    c(historical period earliest year, historical period latest year,
#'    future projection period earliest year, future projection period
#'    latest year).
#' @param expected_length The expected length of \code{boundList}.
checkCustomBounds <- function(boundList, expected_length = 4){

  # Check to make sure the user entered a list of the correct length
  if(boundList != FALSE & length(boundList) != expected_length){
    stop(paste0("User-specified boundary list not expected length. ",
               "Expecting a vector of length ", expected_length,
               ". Stopping."))

  # All other bounds error checking
  } else if(boundList != FALSE){
    histLow <- boundList[1]
    histHigh <- boundList[2]
    rcpLow <- boundList[3]
    rcpHigh <- boundList[4]

    # Check to make sure both upper and lower boundaries exist for the
    # historical and rcp boundary sets
    if(typeof(histLow) != typeof(histHigh)){
      stop(paste("One of the required boundaries of boundaries variable",
                 "unspecified. Stopping."))
    }
    if(typeof(rcpLow) != typeof(rcpHigh)){
      stop(paste("One of the required boundaries of boundaries variable",
                 "unspecified. Stopping."))
    }

    # Check if bounds are the correct type
    if(typeof(histLow) != "double" | typeof(histHigh) != "double"){
      stop("Invalid type: histLow or histHigh. Stopping.")
    }
    if(typeof(rcpLow) != "double" | typeof(rcpHigh) != "double"){
      stop("Invalid type: rcpLow or rcpHigh. Stopping.")
    }

    # Check if bounds are in the correct range
    if(histLow != FALSE){
      if(histLow < 1981){
        stop(paste("Custom boundaries for threshold calculation fall out",
                   "of acceptable range. Stopping."))
      }
    }

    if(histHigh != FALSE){
      if(histHigh > 2004){
        stop(paste("Custom boundaries for threshold calculation fall out",
                   "of acceptable range. Stopping."))
      }
    }

    if(rcpLow != FALSE){
      if(rcpLow < 2061){
        stop(paste("Custom boundaries for threshold calculation fall out of",
                   "acceptable range. Stopping."))
      }
    }

    if(rcpHigh != FALSE){
      if(rcpHigh > 2080){
        stop(paste("Custom boundaries for threshold calculation fall out of",
                   "acceptable range. Stopping."))
      }
    }
  }
}

# TODO: It may be possible to make the accumulator system extremely robust.
# Consider this possibility after all else is finished.

#' Create accumulator closure
#'
#' This closure holds, adds to, and returns data structures that the user
#'    wishes to grow at various points in the execution of the package.
#'
#' As an example, when the generated
#'    closure is used with the command "append location list", it will add
#'    information on the communities and closest grid point locations based
#'    on the climate model it has just completed analyzing to a growing
#'    dataframe with this information for all climate models. After the function
#'    run to generate the heatwave projections is completed, this closure can
#'    be used with the command "return locations" to output the completed
#'    dataframe of this location information.The closure can be used in a
#'    similar manner to aggregate and then return meta-data on the models
#'    analyzed based on their inclusion in the user-specified projections
#'    directory.
#'
#' This function exists to couple these structures together in order to lower
#'    the number of parameters of this nature the user would have to pass down
#'    the the program otherwise. It is a closure instead of a list as a
#'    pre-emptive measure.
#'
#' @return A closure that accepts commands to access and append new data onto
#'    data structures as the program executes. The closure created by this
#'    function accepts two arguments: (1) the command and (2) an element to
#'    be appended to the end of the data structure of the command. These two
#'    arguments must be entered in this exact order. The first argument (the
#'    command) can be one of the following options: "return model information",
#'    "append model information", "return locations", and "append location
#'    list". The second argument for the created closure should only be used
#'    in conjunction with the two "append" commands for the closure.
#'
#' @note This function contains no error checking for the types of elements
#'    input into the data structures it contains.
createAccumulators <- function(){
        modelInfoAccumulator <- data.frame(c(), c(), c())
        locationList <- list()

        function(command, newElement = FALSE){

                # Commands for model information accumulator
                if(command == "return model information"){
                        return(modelInfoAccumulator)

                } else if(command == "append model information"){
                        modelInfoAccumulator <<- rbind(modelInfoAccumulator,
                                                       newElement)

                # Commands for location list accumulator
                } else if(command == "return locations"){
                        return(locationList)

                } else if(command == "append location list"){
                        locationList <<- list(locationList, newElement)
                }

                # If user passes an invalid command, halt the program.
                else{
                        stop("Accumulator closure: Bad command. Exiting.")
                }
        }
}
