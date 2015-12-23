#` This title
#`
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

#` Determine climate projection directory structure
#`
#` @param dataPath A character string giving the file path to the
#`    directory with the climate projection data.

#` @return A list object outlining the file structure of the directory
#`    containing the climate projections.This list has an element
#`    for each climate model (e.g. ,"bcc1"). The first element within each
#`    of these elements is the name of the model. The second element with
#`    the first level element gives the file paths for location grids,
#`    `tas` and `time` for each ensemble run of the model.
#`
#` @examples
#` dataFolder <- "~/Downloads/sample/cmip5/"
#` finalList <- acquireDirectoryStructure(dataFolder)
#` str(finalList[[1]][[1]])
#` str(finalList[[1]][[2]][1])
acquireDirectoryStructure <- function(dataPath){

        # Acquire all directories to all files rooted at dataPath
        all <- list.files(dataPath, recursive = TRUE)

        # Filter out the "Icon" files.
        # Note: I don't know what the "Icon" files within the data folders actually are, except that they have no use.
        all <- all[!grepl("Icon", all)]
        all <- all[!grepl(".mat", all)]

        latlong <- all[grepl("latitude_longitude_NorthAmerica_12mo.csv", all)]
        tas <- all[grepl("tas_NorthAmerica_12mo.csv", all)]
        time <- all[grepl("time_NorthAmerica_12mo.csv", all)]

        all <- c(latlong, tas, time)

        # Separate the directories for each experiment
        histDirs <- subset(all, grepl("historical", all))
        rcpDirs <- subset(all, grepl("rcp85", all))

        # Acquire the models for each experiment
        histModels <- c()
        splitted <- strsplit(histDirs, "/")
        for(i in 1:length(splitted)){
                histModels <- c(histModels, splitted[[i]][2])
        }

        rcpModels <- c()
        splitted <- strsplit(rcpDirs, "/")
        for(i in 1:length(splitted)){
                rcpModels <- c(rcpModels, splitted[[i]][2])
        }

        # Filter out models that are not common to both experiments
        models <- intersect(histModels, rcpModels)

        # Filter out models within the historical eperiment that do not have an ensemble named r1i1p1
        mask <- lapply(paste0("historical/", models, "/r1i1p1"), grepl, histDirs)
        models <- models[unlist(lapply(mask, any))]

        # Generate the list structure
        experiments <- c("historical", "rcp85")

        # Generate the nested lists that will be used for the processing step
        # Structure: model -> experiment -> ensemble
        # See Flow.Rmd for more details.
        finalList <- lapply(models, buildStructureModels, experiments, all, dataPath)

        return(finalList)
}

#` Generate list of file structure from model, experiment, and path
#`
#` @param model Character string of climate model name
#` @param experiments Character vector of the experiment(s) of interest.
#`    Possible variables are "historical", "rcp85", or both.
#` @param all
#` @param dataPath Character string of the file path to the directory
#`    containing the climate projections. Must include the final `\`.
buildStructureModels <- function(model, experiments, all, dataPath){
        return(list(model,
                    buildStructureExperiments(model, experiments[1], all, dataPath),
                    buildStructureExperiments(model, experiments[2], all, dataPath)))
}

buildStructureExperiments <- function(model, experiment, all, dataPath){
        ensembles <- list.dirs(paste0(dataPath, experiment, "/", model))
        ensembles <- ensembles[-1]
        ret <- lapply(ensembles, buildStructureEnsembles)
        return(ret)
}

buildStructureEnsembles <- function(ensemble){
        hold <<- ensemble
        ensembleName <- strsplit(ensemble, "/")[[1]][8]
        files <- list.files(ensemble)
        files <- files[!grepl("Icon", files)]
        files <- files[!grepl(".mat", files)]
        files <- unlist(lapply(files, swapPaste, ensemble))
        return(c(ensembleName, files))
}

swapPaste <- function(second, first){
        return(paste0(first, "/", second))
}
