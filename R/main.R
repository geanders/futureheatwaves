#' Create and write heatwave projections
#'
#' This function creates heatwave projection datasets for all models and
#' ensemble members for a user-specified set of communities.The resulting
#' heatwave projections are written out to a specified directory on the
#' user's local computer.
#'
#' @param out Character string with pathway to directory to which
#'    heatwave files will be written. This should be a pathname to a directory
#'    on the user's local computer. If the directory already exists, it will
#'    be overwritten by this function, so the user should either specify a
#'    pathname for a directory that does not yet exist.
#' @param dataFolder Character string with pathway to directory that
#'    contains climate projection data.
#' @param citycsv Character string giving the filepath to a
#'    comma-separated values (.csv) file with unique identifiers, latitudes, and
#'    longitudes for each community for which the user wants to generate
#'    heatwave projections.
#' @param coordinateFilenames Character string with filename of the file
#'    containing the latitude and longitude coordinates in the bottom
#'    directory of each ensemble member subdirectory. (See the package
#'    vignette for an example of the required structure for this file.)
#' @param tasFilenames Character string with filename of the file
#'    containing the climate projection data in the bottom
#'    directory of each ensemble member subdirectory. (See the package
#'    vignette for an example of the required structure for this file.)
#' @param timeFilenames Character string with filename of the file
#'    containing the date dataframe in the bottom
#'    directory of each ensemble member subdirectory. (See the package
#'    vignette for an example of the required structure for this file.)
#' @param IDheatwavesFunction A character string with the name of the R function
#'    to use to identify heatwaves. This function may be a user-specified custom
#'    function to, but it must be loaded into the current R session. The
#'    function name must be put in quotation marks.
#' @param thresholdBoundaries A numeric vector with the custom time boundaries
#'    to be used to determine the threshold temperatures for the heatwave
#'    definition. The required format for this vector is that the first element
#'    is the lower year bound and the second element is the upper year bound,
#'    with the restrictions that bounds must be between the years 1980 and 2099
#'    and cannot span 2005 (i.e., both must either be before 2005 or after 2005).
#' @param projectionBoundaries A numeric vector with the custom time boundaries
#'    for which the user wants to create heatwave projections. The required
#'    format for this vector is that the first element
#'    is the lower year bound and the second element is the upper year bound,
#'    with the restrictions that bounds must be between the years 1980 and 2099
#'    and cannot span 2005 (i.e., both must either be before 2005 or after 2005).
#' @param referenceBoundaries A numeric vector with the custom time boundaries
#'    to use in calculating relative characteristics for heatwaves (i.e., to use
#'    when exploring the role of adaptation in projections). The required format
#'    for this vector is that the first element is the lower year bound and the
#'    second element is the upper year bound, with the restrictions that bounds
#'    must be between the years 1980 and 2099 and cannot span 2005 (i.e., both
#'    must either be before 2005 or after 2005).
#' @param probThreshold Numerical value between 0 and 1 specifying the threshold
#'    of temperature to use when defining heatwaves. The default value is 0.98
#'    (i.e., a heatwave is two or more days above the community's 98th
#'    percentile temperature).
#' @param printWarning TRUE / FALSE, specifies whether to print out a warning
#'    informing the user that the function will write out results to the local
#'    directory specified by the user with \code{out}. This warning prints out
#'    by default; the user must opt-out of this warning by specifying FALSE
#'    for this argument.
#' @param lat_lon_colnames A character vector of length two with the column names
#'    in the \code{cities} dataframe for latitude (first vector element) and
#'    longitude (second vector element)
#' @param models_to_run A character vector with either "all" (the default),
#'    in which case the function runs through all models in the specified data
#'    directory, or the names of the models to run, using the names of each
#'    model's subdirectory within the data directory (e.g.,
#'    \code{c("bcc1", "ccsm")})
#' @param dataDirectories A list object, with two elements for each of the
#'    two subdirectories included in the main directory. Typically, these will
#'    be a historical directory and a climate projection directory. Each element
#'    of the list should be named with the names of the subdirectories and
#'    should provide a numeric vector with the starting year and ending year of
#'    the data within each of the two subdirectories (e.g.,
#'    \code{list("historical" = c(1980, 2004), "rcp85" = c(2006, 2099))})
#' @param threshold_ensemble A character vector giving the name of the ensemble
#'    member that should be used when determining the threshold temperature for
#'    the heatwave definition for each community for each climate model (e.g.,
#'    \code{"r1i1p1"})
#'
#' @return This function returns a dataframe listing the name of each climate
#'    model used, as well as the number of historical and future projection
#'    ensembles for each model. This output can be used as a check
#'    that the function processed through the directory of input files
#'    specified using the \code{dataFolder} argument.
#'
#' This function also creates, and writes to the user's computer, files with
#'    the heatwaves and their characteristics for the specified climate
#'    projections and dates.
#'
#' @export
#'
#' @importFrom dplyr %>%
gen_hw_set <- function(out,
                       dataFolder,
                       dataDirectories = list("historical" = c(1980, 2004),
                                              "rcp85" = c(2006, 2099)),
                       citycsv,
                       coordinateFilenames,
                       tasFilenames,
                       timeFilenames,
                       IDheatwavesFunction = "IDHeatwavesCPPwrapper",
                       thresholdBoundaries = c(1981, 2004),
                       projectionBoundaries = c(2061, 2080),
                       referenceBoundaries = c(2061, 2080),
                       models_to_run = "all",
                       probThreshold = 0.98,
                       printWarning = TRUE,
                       threshold_ensemble = "r1i1p1",
                       lat_lon_colnames = c("lat", "lon")){

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
                     dataDirectories = dataDirectories,
                     citycsv = citycsv,
                     coordinateFilenames = coordinateFilenames,
                     tasFilenames = tasFilenames,
                     timeFilenames = timeFilenames,
                     IDheatwavesFunction = IDheatwavesFunction,
                     thresholdBoundaries = thresholdBoundaries,
                     projectionBoundaries = projectionBoundaries,
                     referenceBoundaries = referenceBoundaries)

  # Add warning for user that this will write new files
        if(printWarning){
                cat("\n", "Warning: This function will write new files",
                    "to your computer in the \n", out,
                    "directory of your computer. If that directory already",
                    "exists,\nrunning this function will write over it. \n",
                    "Do you want to continue? (y / n): \n")
                user_prompt <- scan(n = 1, what = "character")
                user_prompt <- tolower(user_prompt)
                if(user_prompt %in% c("n", "no")){
                        stop("User chose to exit at prompt.")
                }
        }

        # Put the directories into nested list form
        models <- acquireDirectoryStructure(dataFolder = dataFolder,
                                            coordinateFilenames = coordinateFilenames,
                                            tasFilenames = tasFilenames,
                                            timeFilenames = timeFilenames,
                                            models_to_run = models_to_run,
                                            dataDirectories = dataDirectories,
                                            threshold_ensemble = threshold_ensemble)

        # Read the cities data file
        cities <- read.csv(citycsv) %>%
                process_cities_file(lat_lon_colnames = lat_lon_colnames)

        # Create "global" list object that will hold variables that all
        # functions that need then will have access to
        global <- list("output" = out,
                       "data" = dataFolder,
                       "dataDirectories" = dataDirectories,
                       "cities" = cities,
                       "coordinateFilenames" = coordinateFilenames,
                       "tasFilenames" = tasFilenames,
                       "timeFilenames" = timeFilenames,
                       "threshold_ensemble" = threshold_ensemble)

        # Create the "custom" list object that will hold all of the user's
        # custom settings.
        custom <- list("IDheatwaves" = IDheatwavesFunction,
                       "getBounds" = c(thresholdBoundaries,
                                       projectionBoundaries),
                       "processModel" = referenceBoundaries,
                       "createHwDataframe" = !identical(projectionBoundaries,
                                                       referenceBoundaries),
                       "probThreshold" = probThreshold)

        # Create accumulator closure
        accumulators <- createAccumulators()

        # Process the entire dataset
        referenceEnsembles <- sapply(models, processModel,
                                     global = global,
                                     custom = custom,
                                     accumulators = accumulators)

        # Write the model information from the model information accumulator
        out <- accumulators("return model information")
        writeAccumulators(modelInfoAccumulator = accumulators("return model information"),
                          locationList = accumulators("return locations"),
                          global = global)

        cat("All operations completed. Exiting.", "\n\n")
        return(out)
}

#' Check for input parameter errors
#'
#' This function goes through all parameter inputs for the main
#' functions, \code{\link{gen_hw_set}}, and makes sure all parameter
#' entries are in the appropriate format for following functions.
#' If any parameters are in an incorrect format, the function stops
#' and returns an error describing the problem.
#'
#' @inheritParams gen_hw_set
#'
#' @return Stops and returns an error if any parameters are incorrect.
#'
#' @note This function does not check if the data is organized in the proper
#'    structure or if any data exists within the directory at all, so a
#'    call to \code{\link{gen_hw_set}} could still pass through this check and
#'    make it further through the function code with those mistakes.
#'
#'  Does not check if the three ensemble final .csv data files exist,
#'    only if they have the .csv extension if they do exist. (Reminder:
#'    the final subdirectory should have the following three csv files:
#'    1. A file with the climate model projections, with grid points by
#'    column and times by row; 2. A file with the longitude and latitude
#'    of each grid point in the projection file and; 3. A file with the
#'    date of each of the rows in the projection file.)
check_params <- function(out,
                         dataFolder,
                         dataDirectories,
                         citycsv,
                         coordinateFilenames,
                         tasFilenames,
                         timeFilenames,
                         IDheatwavesFunction,
                         thresholdBoundaries,
                         projectionBoundaries,
                         referenceBoundaries){

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

        checkCustomBounds(boundList = thresholdBoundaries,
                          dataDirectories = dataDirectories)
        checkCustomBounds(boundList = projectionBoundaries,
                          dataDirectories = dataDirectories)
        checkCustomBounds(boundList = referenceBoundaries,
                          dataDirectories = dataDirectories)
}

#' Check year boundaries for errors
#'
#' This function inputs the boundary lists specified in
#' \code{\link{gen_hw_set}}, \code{dataBoundaries} and
#' \code{referenceBoundaries}, and checks them for errors in structure of the
#' input or in the years selected.
#'
#' @param boundList Either FALSE, if user wishes to use the default
#'    boundaries, or a set of boundary years in the format
#'    c(historical period earliest year, historical period latest year,
#'    future projection period earliest year, future projection period
#'    latest year).
#' @inheritParams gen_hw_set
checkCustomBounds <- function(boundList, dataDirectories){

        if(class(boundList) != "numeric"){
                stop("All date boundaries must have the class `numeric`.")
        }

        if(boundList[1] > boundList[2]){
                stop(paste("In date boundaries, the first value must equal",
                           "or be lower than the second value."))
        }

        if(boundList[1] < dataDirectories[[1]][1] |
           boundList[2] > dataDirectories[[2]][2]){
                stop(paste0("Date boundaries must be within the years ",
                           dataDirectories[[1]][1],
                           " and ",
                           dataDirectories[[2]][2], "."))
        }

        if(boundList[1] <= dataDirectories[[1]][2]){
                if(boundList[2] > dataDirectories[[1]][2]){
                        stop(paste0("Date boundaries cannot span between ",
                                   "the first (",
                                   dataDirectories[[1]][1],
                                   "-",
                                   dataDirectories[[1]][2],
                                   ") and second (",
                                   dataDirectories[[2]][1],
                                   "-",
                                   dataDirectories[[2]][2],
                                   ") directory time spans."))
                }
        }
}

#' Create accumulator closure
#'
#' This closure holds, adds to, and returns data structures that the user
#' wishes to grow at various points in the execution of the package.
#'
#' As an example, when the generated
#' closure is used with the command "append location list", it will add
#' information on the communities and closest grid point locations based
#' on the climate model it has just completed analyzing to a growing
#' dataframe with this information for all climate models. After the function
#' run to generate the heatwave projections is completed, this closure can
#' be used with the command "return locations" to output the completed
#' dataframe of this location information.The closure can be used in a
#' similar manner to aggregate and then return meta-data on the models
#' analyzed based on their inclusion in the user-specified projections
#' directory.
#'
#' This function exists to couple these structures together in order to lower
#' the number of parameters of this nature the user would have to pass down
#' the the program otherwise. It is a closure instead of a list as a
#' pre-emptive measure.
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
        locationList <- data.frame(c(), c(), c(), c(), c(), c())

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
                        locationList <<- rbind(locationList, newElement)
                }

                # If user passes an invalid command, halt the program.
                else{
                        stop("Accumulator closure: Bad command. Exiting.")
                }
        }
}
