#' Determine climate projection directory structure
#'
#' @param dataPath A character string giving the file path to the
#'    directory with the climate projection data.
#' @param Names of the files containing the latitude and longitude coordinates
#'    corresponding to the columns of the time series data.
#' @param Names of files containing the time series data.
#' @param Names of the files containing the date information corresponding,
#'    to the rows of the time series data.
#'
#' @return A list object outlining the file structure of the directory
#'    containing the climate projections.This list has an element
#'    for each climate model (e.g. ,"bcc1"). The first element within each
#'    of these elements is the name of the model. The second element with
#'    the first level element gives the file paths for location grids,
#'    `tas` and `time` for each ensemble run of the model.
#'
#' @note The files in the bottom directory must all have a .csv extension
#'    and must names corresponding to the "Filenames" parameters of this function.
#'    All other files will be removed when creating the directory structure.
#'
#' @importFrom dplyr %>%
#' dataFolder <- "~/Downloads/sample/cmip5/"
#' finalList <- acquireDirectoryStructure(dataFolder)
#' str(finalList[[1]][[1]])
#' str(finalList[[1]][[2]][1])
acquireDirectoryStructure <- function(dataPath, coordinateFilenames,
                                      tasFilenames, timeFilenames){

        # Acquire all pathnames to csv files rooted at dataPath
        all <- list.files(dataPath, recursive = TRUE,
                          pattern = "\\.csv$")

        # Convert list to a dataframe
        split_all <- strsplit(all, "/")
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

        all <- apply(df_all, 1, paste, collapse = "/")
        models <- as.character(unique(df_all$model))
        experiments <- as.character(unique(df_all$exp))

        # Generate the nested lists that will be used for the processing step
        # Structure: model -> experiment -> ensemble
        finalList <- lapply(models, buildStructureModels, experiments, all, dataPath, coordinateFilenames, tasFilenames, timeFilenames)

        return(finalList)
}

#' Generate list of file structure from model, experiment, and path
#'
#' @param model Character string of climate model name
#' @param experiments Character vector of the experiment(s) of interest.
#'    Possible variables are "historical", "rcp85", or both.
#' @param all Character vectors with the relative pathnames of all
#'    climate projection files of interest from the directory
#'    specified in dataPath
#' @param dataPath Character string of the file path to the directory
#'    containing the climate projections. Must include the final `/`.
#'
#' @return A list of length 3. The first element is the name of the model whose structure was being built.
#' The second element is the historical experiment hierarchy. The third element is the rcp85 experiment hierarchy. The
#' second and third elements are return values of buildStructureExperiments.
#'
#' @examples
#' model <- "bcc1"
#' experiments <- c("rcp85")
#' all <- c("rcp85/bcc1/r1i1p1/latitude_longitude_NorthAmerica_12mo.csv",
#'          "rcp85/bcc1/r1i1p1/tas_NorthAmerica_12mo.csv",
#'          "rcp85/bcc1/r1i1p1/time_NorthAmerica_12mo.csv")
#' dataPath <- "~/Downloads/sample/cmip5/"
#' buildStructureModels(model, experiments, all, dataPath)
buildStructureModels <- function(model, experiments, all, dataPath, coordinateFilenames, tasFilenames, timeFilenames){
        return(list(model,
                    buildStructureExperiments(model, experiments[1], all, dataPath, coordinateFilenames, tasFilenames, timeFilenames),
                    buildStructureExperiments(model, experiments[2], all, dataPath, coordinateFilenames, tasFilenames, timeFilenames)))
}

#' Generate list of file structure for an experiment
#'
#' @param model Character string of climate model name
#' @param experiment Character string of the experiment of interest.
#'    Possible variables are "historical" or "rcp85".
#' @param all Character vectors with the relative pathnames of all
#'    climate projection files of interest from the directory
#'    specified in dataPath
#' @param dataPath Character string of the file path to the directory
#'    containing the climate projections. Must include the final `/`.
#'
#' @return A list that is the length of the number of ensembles. Each element is a return value
#' of the buildStructureEnsembles function.
#'
#' @examples
#' model <- "bcc1"
#' experiment <- "rcp85"
#' all <- c("rcp85/bcc1/r1i1p1/latitude_longitude_NorthAmerica_12mo.csv",
#'          "rcp85/bcc1/r1i1p1/tas_NorthAmerica_12mo.csv",
#'          "rcp85/bcc1/r1i1p1/time_NorthAmerica_12mo.csv")
#' dataPath <- "~/Downloads/sample/cmip5/"
#' buildStructureExperiments(model, experiments, all, dataPath)
#'
buildStructureExperiments <- function(model, experiment, all, dataPath, coordinateFilenames, tasFilenames, timeFilenames){

        # List all ensembles in the given experiment
        ensembles <- list.dirs(paste0(dataPath, experiment, "/", model))

        # Trim off the first element of 'ensembles' list, since it does not contain information about an ensemble's data.
        ensembles <- ensembles[-1]

        # Build the directory structure of each ensemble
        ret <- lapply(ensembles, buildStructureEnsembles, coordinateFilenames, tasFilenames, timeFilenames)
        return(ret)
}

#' List all files for a single ensemble member
#'
#' @param Ensemble character string that gives the absolute file path
#'    for the directory with a particular ensemble member of a climate
#'    model projection.
#'
#' @return A list of length 2. First element is the name of the ensemble that was processed. Second element is
#' a list containing the coordinate .csv, the time series data .csv, and the time data .csv respectively.
#'
#' @examples
#' ensemble <- "/Users/brookeanderson/Downloads/sample/cmip5/rcp85/bcc1/r1i1p1"
#' buildStructureEnsembles(ensemble)
buildStructureEnsembles <- function(ensemble, coordinateFilenames, tasFilenames, timeFilenames){

        # Extract name of the ensemble
        splist = strsplit(ensemble, "/")
        ensemble_name_index <- which(sapply(splist,
                                         function(x) x %in%
                                                 c("historical", "rcp85"))) + 2
        ensembleName <- splist[[1]][model_name_index]

        # remove any irrelevant files from the file structure
        files <- list.files(ensemble)
        coor <- files[grep(coordinateFilenames, files)]
        tas <- files[grep(tasFilenames, files)]
        time <- files[grep(timeFilenames, files)]
        files <- c(coor, tas, time)
        directories <- unlist(lapply(files, function(x){
                return(paste(ensemble, x, sep = "/"))
        }))
        return(c(ensembleName, directories))
}
