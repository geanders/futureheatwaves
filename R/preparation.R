#'
#'
parseModel <- function(model){
        modelName <- model[1]

        # Get ensemble directories for one model.
        hDirs <- toString(unlist(model[2]))
        hDirs <- unlist(strsplit(hDirs, "@"))
        hDirs <- strsplit(hDirs, "#")
        rcpDirs <- toString(unlist(model[3]))
        rcpDirs <- unlist(strsplit(rcpDirs, "@"))
        rcpDirs <- strsplit(rcpDirs, "#")

        return(list(modelName, hDirs, rcpDirs))
}

#' Determine climate projection directory structure
#'
#' @param dataPath A character string giving the file path to the
#'    directory with the climate projection data.

#' @return A list object outlining the file structure of the directory
#'    containing the climate projections.This list has an element
#'    for each climate model (e.g. ,"bcc1"). The first element within each
#'    of these elements is the name of the model. The second element with
#'    the first level element gives the file paths for location grids,
#'    `tas` and `time` for each ensemble run of the model.
#'
#' @examples
#' dataFolder <- "~/Downloads/sample/cmip5/"
#' finalList <- acquireDirectoryStructure(dataFolder)
#' str(finalList[[1]][[1]])
#' str(finalList[[1]][[2]][1])
acquireDirectoryStructure <- function(dataPath){

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
        df_all <- group_by(df_all, model) %>%
                summarize(check_1 = "historical" %in% exp,
                          check_2 = "rcp85" %in% exp,
                          check_3 = "historical r1i1p1" %in%
                                  paste(exp, ens)) %>%
                left_join(df_all, by = "model") %>%
                filter(check_1 & check_2 & check_3 &
                               type %in% c("latitude_longitude_NorthAmerica_12mo.csv",
                                           "tas_NorthAmerica_12mo.csv",
                                           "time_NorthAmerica_12mo.csv")) %>%
                select(-starts_with("check"))

        all <- apply(df_all, 1, paste, collapse = "/")
        models <- as.character(unique(df_all$model))
        experiments <- as.character(unique(df_all$exp))

        # Generate the nested lists that will be used for the processing step
        # Structure: model -> experiment -> ensemble
        # See Flow.Rmd for more details.
        finalList <- lapply(models, buildStructureModels, experiments, all, dataPath)

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
#'    containing the climate projections. Must include the final `\`.
#'
#' @examples
#' model <- "bcc1"
#' experiments <- c("rcp85")
#' all <- c("rcp85/bcc1/r1i1p1/latitude_longitude_NorthAmerica_12mo.csv",
#'          "rcp85/bcc1/r1i1p1/tas_NorthAmerica_12mo.csv",
#'          "rcp85/bcc1/r1i1p1/time_NorthAmerica_12mo.csv")
#' dataPath <- "~/Downloads/sample/cmip5/"
#' buildStructureModels(model, experiments, all, dataPath)
buildStructureModels <- function(model, experiments, all, dataPath){
        return(list(model,
                    buildStructureExperiments(model, experiments[1], all, dataPath),
                    buildStructureExperiments(model, experiments[2], all, dataPath)))
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
#'    containing the climate projections. Must include the final `\`.
#'
#' @examples
#' model <- "bcc1"
#' experiment <- "rcp85"
#' all <- c("rcp85/bcc1/r1i1p1/latitude_longitude_NorthAmerica_12mo.csv",
#'          "rcp85/bcc1/r1i1p1/tas_NorthAmerica_12mo.csv",
#'          "rcp85/bcc1/r1i1p1/time_NorthAmerica_12mo.csv")
#' dataPath <- "~/Downloads/sample/cmip5/"
#' buildStructureExperiments(model, experiments, all, dataPath)
buildStructureExperiments <- function(model, experiment, all, dataPath){
        ensembles <- list.dirs(paste0(dataPath, experiment, "/", model))
        ensembles <- ensembles[-1]
        ret <- lapply(ensembles, buildStructureEnsembles)
        return(ret)
}

#' List all files for a single ensemble member
#'
#' @param ensemble Character string that gives the absolute file path
#'    for the directory with a particular ensemble member of a climate
#'    model projection.
#'
#' @examples
#' ensemble <- "/Users/brookeanderson/Downloads/sample/cmip5/rcp85/bcc1/r1i1p1"
#' buildStructureEnsembles(ensemble)
buildStructureEnsembles <- function(ensemble){
        hold <<- ensemble
        ensembleName <- strsplit(ensemble, "/")[[1]][8]
        files <- list.files(ensemble)
        files <- files[!grepl("Icon", files)]
        files <- files[!grepl(".mat", files)]
        files <- unlist(lapply(files, swapPaste, ensemble))
        return(c(ensembleName, files))
}

#' Paste second element before first, separated by a backslash
#'
#' @param second Character string with element to paste second in output.
#' @param first Character string with element to paste first in output.
#'
#' @examples
#' second <- "latitude_longitude_NorthAmerica_12mo.csv"
#' first <- "/Users/brookeanderson/Downloads/sample/cmip5/rcp85/bcc1/r1i1p1"
#' swapPaste(second, first)
swapPaste <- function(second, first){
        return(paste0(first, "/", second))
}
