#' Identify all heatwaves within a given city's time series
#'
#' @param threshold Numeric string with threshold percentile used in
#'    heatwave definition.
#' @param days Numeric string specifying minimum number of days required
#'    in heatwave definition.
#' @param datafr A dataframe with daily temperature projections in the
#'    the community being processed. This dataframe must have two columns:
#'    (1) the first column must have the date of each observation, with
#'    class "Date" and; (2) the second column must have temperatures, in
#'    Fahrenheit. In the normal running of this package, this dataframe
#'    will be generate by the closure created by
#'    \code{\link{createCityProcessor}}.
#' @inheritParams processModel
#' @inheritParams closest_point
#'
#' @return Returns the dataframe entered as \code{datafr}, but with new
#'    columns providing heatwave identifiers. The returned dataframe will
#'    have new columns for whether a day was part of a heatwave (\code{hw},
#'    0 / 1), if it was part of a heatwave, the number of the heatwave
#'    (\code{hw.number}), and whether the day was the first day in a heatwave
#'    (\code{first.hw.day}, 0 /1).
#'
IDheatwaves <- function(city, threshold, days = 2, datafr, global, custom){
        # Initialize return value
        hwdata <- c()
        RorCPP <- global["RorCPP"]

        # Check if user has specified their own replacement function for identifying heatwaves
        if(!is.logical(custom["IDheatwaves"]) &
           is.function(custom["IDheatwaves"][[1]]) == TRUE){
                customidhw <- custom["IDheatwaves"][[1]]
                tryCatch(
                        hwdata <<- customidhw(city, threshold, days, datafr),
                        error = function(){
                                stop("Invalid function for identifying heatwaves specified. Exiting")
                        },
                        finally = {}
                )
        }

        # Acquire heatwave dataframe using the R or C++ functions
        # R
        else if(RorCPP == 0){
                hwdata <- IDHeatwavesR(city, threshold, days, datafr)

        # C++
        } else if (RorCPP == 1){
                # Add names to the dataframe
                colnames(datafr) <- c("date", "tmpd")

                # Find temperatures that exceed the threshold. One means the measurement equals or exceeds threshold.
                tempsExceedingthreshold <- ifelse(datafr[,2] >= threshold, 1, 0)

                # Add zero onto the end of the vector. The CPP routine needs this to work properly.
                tempsExceedingthreshold <- c(tempsExceedingthreshold, 0)

                # Identify heatwaves using the C++ functions.
                heatwaves <- IDHeatwavesCPP(days, tempsExceedingthreshold)

                # Attach heatwaves columns onto the data in the datafr variable
                # Note that the final row, which contains zeroes as placeholders, is excluded.
                hwdata <- data.frame(datafr, heatwaves[-(dim(heatwaves)[1]),])
        }
        return(hwdata)
}

#' Identify heatwaves in a time series
#'
#' @param city ...
#' @param threshold Numeric string with threshold percentile used in
#'    heatwave definition.
#' @param days Numeric string specifying minimum number of days required
#'    in heatwave definition.
#' @param datafr Dataframe with a time series of the daily temperatures in
#'    the community being analyzed.
#'
#' @return Returns the dataframe entered as \code{datafr}, but with new
#'    columns providing heatwave identifiers.
IDHeatwavesR <- function(city = stop("Error: unspecified city"),
                         threshold = stop("Error: unspecified threshold"),
                         days = 2,
                         datafr =  stop("Error: 'datafr' unspecified")){

        # Add names to the dataframe
        colnames(datafr) <- c("date", "tmpd")

        # Find temperatures that exceed the threshold. One means the measurement equals or exceeds threshold.
        tempsExceedingthreshold <- ifelse(datafr[,2] >= threshold, 1, 0)

        # Add zero to end of vector so that the match function below
        tempsExceedingthreshold <- c(tempsExceedingthreshold, 0)

        # What a heatwave looks like in a vector
        heatwaveForm <- rep(1, days)

        # Counter for heatwave number
        counter <- 1

        # hwBound is used to extract a vector of data from the series to compare against heatwaveForm
        hwBound <- days - 1

        # Initialize dataframe containing the columns that will be added to datafr
        hwInfo <- data.frame(hw = c(9), hw.number = c(9), first.hw.day = c(9))

        # Current Index
        i <- 1

        # Identify all heatwaves for the city
        while (i <= length(datafr[,2])) {
                if(identical( tempsExceedingthreshold[i: (i + hwBound)], heatwaveForm)){ # Check if there is a heatwave starting at i
                        size <- match(0, tempsExceedingthreshold[-(1:i)]) # Acquire size of heatwave

                        # Store all desired information about this heatwave
                        hwInfo <- data.frame(hw = c(hwInfo[,1], rep(1, size)),
                                             hw.number = c(hwInfo[,2], rep(counter, size)),
                                             first.hw.day = c(hwInfo[,3], 1, rep(0, size - 1)))

                        counter <- counter + 1 # Increment counter for heatwave number
                        i <- i + size # Advance i to next position after heatwave
                } else {
                        # If no heatwave at i, then add zeros to the end of the dataframe
                        hwInfo <- data.frame(hw = c(hwInfo[,1], 0),
                                             hw.number = c(hwInfo[,2], 0),
                                             first.hw.day = c(hwInfo[,3], 0))
                        i <- i + 1
                }
        }

        return(data.frame(datafr, hwInfo[-1,])) # Combine the original dataframe with the heatwave characteristics matrix. Notice the placeholder row is excluded.
}

#' Identify heatwaves with alternative definition
#'
#' @inheritParams IDHeatwavesR
#'
#' @return Returns the dataframe entered as \code{datafr}, but with new
#'    columns providing heatwave identifiers.
IDHeatwavesAlternative <- function(city = stop("Error: unspecified city"),
                           threshold = stop("Error: unspecified threshold"),
                           days = 1,
                           datafr =  stop("Error: 'datafr' unspecified")){

        # Add names to the dataframe
        colnames(datafr) <- c("date", "tmpd")

        # Find temperatures that exceed the threshold. One means the measurement equals or exceeds threshold.
        tempsExceedingthreshold <- ifelse(datafr[,2] >= threshold, 1, 0)

        # Add zero to end of vector so that the match function below
        tempsExceedingthreshold <- c(tempsExceedingthreshold, 0)

        # What a heatwave looks like in a vector
        heatwaveForm <- rep(1, days)

        # Counter for heatwave number
        counter <- 1

        # hwBound is used to extract a vector of data from the series to compare against heatwaveForm
        hwBound <- days - 1

        # Initialize dataframe containing the columns that will be added to datafr
        hwInfo <- data.frame(hw = c(9), hw.number = c(9), first.hw.day = c(9))

        # Current Index
        i <- 1

        # Identify all heatwaves for the city
        while (i <= length(datafr[,2])) {
                if(identical( tempsExceedingthreshold[i: (i + hwBound)], heatwaveForm)){ # Check if there is a heatwave starting at i
                        size <- match(0, tempsExceedingthreshold[-(1:i)]) # Acquire size of heatwave

                        # Store all desired information about this heatwave
                        hwInfo <- data.frame(hw = c(hwInfo[,1], rep(1, size)),
                                             hw.number = c(hwInfo[,2], rep(counter, size)),
                                             first.hw.day = c(hwInfo[,3], 1, rep(0, size - 1)))

                        counter <- counter + 1 # Increment counter for heatwave number
                        i <- i + size # Advance i to next position after heatwave
                } else {
                        # If no heatwave at i, then add zeros to the end of the dataframe
                        hwInfo <- data.frame(hw = c(hwInfo[,1], 0),
                                             hw.number = c(hwInfo[,2], 0),
                                             first.hw.day = c(hwInfo[,3], 0))
                        i <- i + 1
                }
        }

        return(data.frame(datafr, hwInfo[-1,])) # Combine the original dataframe with the heatwave characteristics matrix. Notice the placeholder row is excluded.
}

