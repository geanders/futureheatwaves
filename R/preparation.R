#' Acquire structure of input directory
#'
#' This function walks through the directory structure of the user-provided
#' directory of climate projection files and parses out a list of the
#' climate models and ensemble members included in that directory based
#' on subdirectory names.
#'
#' @inheritParams gen_hw_set
#'
#' @return A list object outlining the file structure of the directory
#'    containing the climate projections.This list has an element
#'    for each climate model (e.g.,"bcc1"). The first element within each
#'    of these elements is the name of the model. The second element within
#'    the first-level element gives the file paths for location grids,
#'    climate projections, and projection times for each ensemble run of the
#'    model.
#'
#' @note The files in the bottom directory must all have a .csv extension
#'    and must have names corresponding to the relevant "*Filenames" parameter
#'    of this function. All other files will be removed when creating the
#'    directory structure.
#'
#' @examples
#' dataFolder <- system.file("cmip5", package = "futureheatwaves")
#' coordinateFilenames <- "latitude_longitude_NorthAmerica_12mo.csv"
#' tasFilenames <- "tas_NorthAmerica_12mo.csv"
#' timeFilenames <- "time_NorthAmerica_12mo.csv"
#'
#' finalList <- acquireDirectoryStructure(dataFolder = dataFolder,
#'    coordinateFilenames = coordinateFilenames,
#'    tasFilenames = tasFilenames,
#'    timeFilenames = timeFilenames)
#'
#' finalList[[1]][[1]]
#' finalList[[1]][[2]][1]
#'
#' @export
#'
#' @importFrom dplyr %>%
acquireDirectoryStructure <- function(dataFolder, coordinateFilenames,
                                      tasFilenames, timeFilenames,
                                      models_to_run){

        # If `dataFolder` does not end in "/", add it.
        # (Only need to repeat here to use with in-package examples)
        split_dataFolder <- unlist(strsplit(dataFolder, split = ""))
        last_char <- split_dataFolder[length(split_dataFolder)]
        if(last_char != "/"){
                dataFolder <- paste0(dataFolder, "/")
        }

        # Acquire all pathnames to csv files rooted at dataPath
        all <- list.files(dataFolder, recursive = TRUE,
                          pattern = "\\.csv$")

        # Convert list to a dataframe
        split_all <- strsplit(all, "/")
        split_all <- split_all[sapply(split_all, length) == 4]
        df_all <- as.data.frame(matrix(unlist(split_all),
                                        ncol = 4, byrow = TRUE))
        colnames(df_all) <- c("exp", "model", "ens", "type")

        # Only get climate models with (a) both historical and
        # rcp85 results and (b) r1i1p1 ensemble historical results
        df_all <- dplyr::group_by(df_all, model) %>%
                dplyr::summarize(check_1 = "historical" %in% exp,
                          check_2 = "rcp85" %in% exp,
                          check_3 = "historical r1i1p1" %in%
                                  paste(exp, ens)) %>%
                dplyr::left_join(df_all, by = "model") %>%
                dplyr::filter(check_1 & check_2 & check_3 &
                               type %in% c(coordinateFilenames,
                                           tasFilenames,
                                           timeFilenames)) %>%
                dplyr::select(exp, model, ens, type)

        models <- as.character(unique(df_all$model))
        experiments <- as.character(unique(df_all$exp))

        # Generate the nested lists that will be used for the processing step
        # Structure: model -> experiment -> ensemble
        finalList <- lapply(models, buildStructureModels, experiments,
                            dataFolder, coordinateFilenames, tasFilenames,
                            timeFilenames)

        # Limit to only the models the user wants to run (if specified)
        if(models_to_run[1] != "all"){
                names(finalList) <- sapply(finalList, function(x) x[[1]])
                finalList <- finalList[models_to_run]
        }

        return(finalList)
}

#' Generate list of file structure
#'
#' This function takes input from \code{\link{acquireDirectoryStructure}} and
#' uses it to generate a list object with the projection directory
#' file structure. This parsed file structure is later used to lead other
#' code through all climate models and ensemble members in the input
#' projection directory.
#'
#' @param modelName Character string of climate model name (e.g., "bcc1"). This
#'    name is typically generated for use in this function from the subdirectory
#'    names for the climate model within the directory of projection data
#'    specified by the user in \code{\link{gen_hw_set}}.
#' @param experiments Character vector of the experiment(s) of interest.
#'    Possible variables are "historical", "rcp85", or both.
#' @param dataPath Character string of the file path to the directory
#'    containing the climate projections. Must include the final `/`.
#' @inheritParams gen_hw_set
#'
#' @return A list of length 3. The first element is the name of the model
#'    whose structure was being built. The second element is the historical
#'    experiment hierarchy. The third element is the hierarchy of the future
#'    projection directory for the model. The second and third elements are
#'    return values of \code{\link{buildStructureExperiments}}.
#'
#' @examples
#' modelName <- "bcc1"
#' experiments <- c("historical", "rcp85")
#'
#' dataFolder <- system.file("cmip5", package = "futureheatwaves")
#' dataFolder <- paste0(dataFolder, "/")
#'
#' coordinateFilenames <- "latitude_longitude_NorthAmerica_12mo.csv"
#' tasFilenames <- "tas_NorthAmerica_12mo.csv"
#' timeFilenames <- "time_NorthAmerica_12mo.csv"
#'
#' buildStructureModels(model = modelName,
#'                      experiments = experiments,
#'                      dataFolder,
#'                      coordinateFilenames = coordinateFilenames,
#'                      tasFilenames = tasFilenames,
#'                      timeFilenames = timeFilenames)
buildStructureModels <- function(modelName, experiments,
                                 dataFolder,
                                 coordinateFilenames, tasFilenames,
                                 timeFilenames){
        return(list(modelName,
                    buildStructureExperiments(modelName, experiments[1],
                                              dataFolder,
                                              coordinateFilenames,
                                              tasFilenames, timeFilenames),
                    buildStructureExperiments(modelName, experiments[2],
                                              dataFolder,
                                              coordinateFilenames,
                                              tasFilenames, timeFilenames)))
}

#' Generate file structure for an experiment
#'
#' This function generates a list object with the file structure of files
#' within the user-specified projection directory for a single experiment
#' (i.e., "historical" or "rcp85").
#'
#' @param experiment Character string of the experiment of interest.
#'    Possible variables are "historical" or "rcp85".
#' @inheritParams buildStructureModels
#' @inheritParams gen_hw_set
#'
#' @return A list that is the length of the number of ensembles. Each element
#'    is a return value of the \code{\link{buildStructureEnsembles}} function for
#'    one of the ensemble members in that experiment of that climate model
#'    within the user-specified projections directory.
#'
#' @examples
#' modelName <- "bcc1"
#' experiment <- "rcp85"
#'
#' dataPath <- system.file("cmip5", package = "futureheatwaves")
#' dataPath <- paste0(dataFolder, "/")
#'
#' coordinateFilenames <- "latitude_longitude_NorthAmerica_12mo.csv"
#' tasFilenames <- "tas_NorthAmerica_12mo.csv"
#' timeFilenames <- "time_NorthAmerica_12mo.csv"
#'
#' buildStructureExperiments(modelName = modelName,
#'                           experiment = experiment,
#'                           dataPath = dataPath,
#'                           coordinateFilenames = coordinateFilenames,
#'                           tasFilenames = tasFilenames,
#'                           timeFilenames = timeFilenames)
#'
buildStructureExperiments <- function(modelName, experiment,
                                      dataPath,
                                      coordinateFilenames, tasFilenames,
                                      timeFilenames){

        # List all ensembles in the given experiment
        ensembles <- list.dirs(paste0(dataPath, experiment, "/", modelName))

        # Trim off the first element of 'ensembles' list, since it does
        # not contain information about an ensemble's data.
        ensembles <- ensembles[-1]

        # Build the directory structure of each ensemble
        ret <- lapply(ensembles, buildStructureEnsembles, coordinateFilenames,
                      tasFilenames, timeFilenames)
        return(ret)
}

#' List files for a single ensemble member
#'
#' This function reads through the user-specified projections directory and
#' creates a list with all files in the subdirectory of a single
#' ensemble member subdirectory within a specific climate model
#' subdirectory.
#'
#' @param ensemblePath A character string that gives the absolute file path
#'    for the subdirectory for a particular ensemble member of a climate
#'    model within the user-specified projection directory.
#' @inheritParams gen_hw_set
#'
#' @return A list of length 2. The first element is the name of the ensemble
#' that was processed. The second element is a list containing the coordinate
#' comma-separated file, the projection data comma-separated file,
#' and the time data comma-separated file, respectively.
#'
#' @examples
#'
#' experiment <- "rcp85"
#' modelName <- "bcc1"
#' ensembleName <- "r1i1p1"
#' dataFolder <- system.file("cmip5", package = "futureheatwaves")
#'
#' ensemblePath <- paste(dataFolder, experiment,
#'                       modelName, ensembleName, sep = "/")
#'
#' coordinateFilenames <- "latitude_longitude_NorthAmerica_12mo.csv"
#' tasFilenames <- "tas_NorthAmerica_12mo.csv"
#' timeFilenames <- "time_NorthAmerica_12mo.csv"
#'
#' buildStructureEnsembles(ensembleName = ensembleName,
#'                         coordinateFilenames = coordinateFilenames,
#'                         tasFilenames = tasFilenames,
#'                         timeFilenames = timeFilenames)
buildStructureEnsembles <- function(ensemblePath, coordinateFilenames,
                                    tasFilenames, timeFilenames){

        # Extract name of the ensemble (two directories below the
        # one named for the experiment, "historical" or "rcp85").
        splist = strsplit(ensemblePath, "/")
        ensemble_name_index <- which(sapply(splist,
                                         function(x) x %in%
                                                 c("historical", "rcp85")))
        ensemble_name_index <- ensemble_name_index + 2
        ensembleName <- splist[[1]][ensemble_name_index]

        # remove any irrelevant files from the file structure
        files <- list.files(ensemblePath)
        coor <- files[grep(coordinateFilenames, files)]
        tas <- files[grep(tasFilenames, files)]
        time <- files[grep(timeFilenames, files)]
        files <- c(coor, tas, time)
        directories <- unlist(lapply(files, function(x){
                return(paste(ensemblePath, x, sep = "/"))
        }))
        return(c(ensembleName, directories))
}
