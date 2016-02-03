#' Identify all heatwaves in a time series
#'
#' This function takes a dataframe with columns for date and projected
#' temperature and adds columns identifying which days belong to a
#' heatwave, giving separate numbers to identify each discrete heatwave,
#' and identifying which days are the first day in a heatwave.
#'
#' @param threshold Numeric string with threshold temperature used in
#'    the heatwave definition, in the same units as the temperature time
#'    series in \code{datafr}.
#' @param datafr A dataframe with daily temperature projections in the
#'    the community being processed. This dataframe must have two columns:
#'    (1) the first column must have the date of each observation, with
#'    class "Date" and; (2) the second column must have temperatures
#'    (in typical runs of this function, this will be in degrees
#'    Fahrenheit). In the normal running of this package, this dataframe
#'    will be generate by the closure created by
#'    \code{\link{createCityProcessor}}.
#' @inheritParams processModel
#' @inheritParams closest_point
#'
#' @return Returns the dataframe entered as \code{datafr}, but with new
#'    columns providing heatwave identifiers. The returned dataframe will
#'    have new columns for: \code{hw}: whether a day was part of a heatwave
#'    (0 : not part of a heatwave / 1: part of a heatwave); \code{hw.number}:
#'    if it was part of a heatwave, the number of the heatwave (1, 2, etc.);
#'    and \code{first.hw.day} whether the day was the first day in a heatwave
#'    (0: not the first day in a heatwave / 1: first day in a heatwave).
#'
#' @note The function actually used to identify heatwaves in the time series
#'    is specified in the `IDheatwaves` slot of the `custom` object passed
#'    into this function. The default is the function
#'    \code{\link{IDHeatwavesR}}. The user can specify a different function
#'    using the argument `IDheatwavesFunction` in \code{\link{gen_hw_set}}.
#'
#' @export
IDheatwaves <- function(threshold, datafr, global, custom){

        hwdata <- do.call(custom["IDheatwaves"][[1]],
                          list(threshold = threshold,
                               datafr = datafr))

        return(hwdata)
}

#' Identify heatwaves in a time series
#'
#' This function identifies heatwaves in a time series of temperature
#' data using a heatwave definition that a heatwave must be two or more
#' days with temperatures equal to or above some threshold temperature.
#'
#' This function is the default function used to identify heatwaves in
#' \code{\link{gen_hw_set}}.
#'
#' @inheritParams closest_point
#' @inheritParams IDheatwaves
#'
#' @return Returns the dataframe entered as \code{datafr}, but with new
#'    columns providing heatwave identifiers. The returned dataframe will
#'    have new columns for whether a day was part of a heatwave (\code{hw},
#'    0 / 1), if it was part of a heatwave, the number of the heatwave
#'    (\code{hw.number}), and whether the day was the first day in a heatwave
#'    (\code{first.hw.day}, 0 /1).
#'
#' @note There are a few cases near the edges of data frames when this function
#'    would return that a day was not a heatwave when it was.First, if the first
#'    day of the dataset is a heatwave because preceeding days exceeded the
#'    threshold, but the second day in the dataframe is not above the threshold,
#'    this function would not capture that the first day was a heatwaves.
#'    Similar caveats apply to the last day in the dataframe. In northern
#'    hemisphere communities, this should not be a concern when studying
#'    heatwaves, as it is unlikely that Jan. 1 or Dec. 31 would qualify as
#'    a heatwave in this part of the world. However, care should be taken
#'    when using this function either with Southern Hemisphere communities
#'    or when exploring exposures that, unlike heatwaves, may occur very
#'    early or late in the calendar year.
#' @export
IDHeatwavesR <- function(threshold, datafr){

        days <- 2

        # Add names to the dataframe
        colnames(datafr) <- c("date", "tmpd")

        # Find temperatures that exceed the threshold. One means the
        # measurement equals or exceeds threshold.
        tempsExceedingthreshold <- as.numeric(datafr[,2] >= threshold)

        # Add zero to end of vector so that the match function below
        tempsExceedingthreshold <- c(tempsExceedingthreshold, 0)

        # What a heatwave looks like in a vector
        heatwaveForm <- rep(1, days)

        # Counter for heatwave number
        counter <- 1

        # hwBound is used to extract a vector of data from the series to
        # compare against heatwaveForm
        hwBound <- days - 1

        # Initialize dataframe containing the columns that will be added to datafr
        # Initialize dataframe containing the columns that will be added to
        # datafr
        hwInfo <- data.frame(hw = c(9),
                            hw.number = c(9))

        # Current Index
        i <- 1

        # Identify all heatwaves for the city
        while (i <= nrow(datafr)) {
                # Check if there is a heatwave starting at i
                # If so, acquire size of heatwave
                if(identical(tempsExceedingthreshold[i: (i + hwBound)],
                              heatwaveForm)){
                        size <- match(0, tempsExceedingthreshold[-(1:i)])

                        # Store all desired information about this heatwave
                        hwInfo <- data.frame(hw = c(hwInfo[,1], rep(1, size)),
                                             hw.number = c(hwInfo[,2],
                                                           rep(counter, size)))

                        # Increment and advance
                        counter <- counter + 1
                        i <- i + size
                } else {
                        # If no heatwave at i, then add zeros to the end of
                        # the dataframe
                        hwInfo <- rbind(hwInfo, c(0, 0))
                        i <- i + 1
                }
        }

        # Combine the original dataframe with the heatwave characteristics
        # matrix. Notice the placeholder row is excluded.
        return(data.frame(datafr, hwInfo[-1,]))
}

#' Identify heatwaves in a time series
#'
#' This function identifies heatwaves in a time series of temperature
#' data using a heatwave definition that a heatwave must be five or more
#' days with temperatures equal to or above some threshold temperature.
#'
#' @inheritParams closest_point
#' @inheritParams IDheatwaves
#'
#' @return Returns the dataframe entered as \code{datafr}, but with new
#'    columns providing heatwave identifiers. The returned dataframe will
#'    have new columns for whether a day was part of a heatwave (\code{hw},
#'    0 / 1), if it was part of a heatwave, the number of the heatwave
#'    (\code{hw.number}), and whether the day was the first day in a heatwave
#'    (\code{first.hw.day}, 0 /1).
#'
#' @note There are a few cases near the edges of data frames when this function
#'    would return that a day was not a heatwave when it was.First, if the first
#'    day of the dataset is a heatwave because preceeding days exceeded the
#'    threshold, but the second day in the dataframe is not above the threshold,
#'    this function would not capture that the first day was a heatwaves.
#'    Similar caveats apply to the last day in the dataframe. In northern
#'    hemisphere communities, this should not be a concern when studying
#'    heatwaves, as it is unlikely that Jan. 1 or Dec. 31 would qualify as
#'    a heatwave in this part of the world. However, care should be taken
#'    when using this function either with Southern Hemisphere communities
#'    or when exploring exposures that, unlike heatwaves, may occur very
#'    early or late in the calendar year.
#' @export
IDHeatwavesAlternative <- function(threshold, datafr){

        days <- 5

        # Add names to the dataframe
        colnames(datafr) <- c("date", "tmpd")

        # Find temperatures that exceed the threshold. One means the
        # measurement equals or exceeds threshold.
        tempsExceedingthreshold <- as.numeric(datafr[,2] >= threshold)

        # Add zero to end of vector so that the match function below
        tempsExceedingthreshold <- c(tempsExceedingthreshold, 0)

        # What a heatwave looks like in a vector
        heatwaveForm <- rep(1, days)

        # Counter for heatwave number
        counter <- 1

        # hwBound is used to extract a vector of data from the series to
        # compare against heatwaveForm
        hwBound <- days - 1

        # Initialize dataframe containing the columns that will be added to
        # datafr
        hwInfo <- data.frame(hw = c(9),
                             hw.number = c(9))

        # Current Index
        i <- 1

        # Identify all heatwaves for the city
        while (i <= length(datafr[,2])) {
                # Check if there is a heatwave starting at i
                if(identical(tempsExceedingthreshold[i: (i + hwBound)],
                              heatwaveForm)){
                        # Acquire size of heatwave
                        size <- match(0, tempsExceedingthreshold[-(1:i)])

                        # Store all desired information about this heatwave
                        hwInfo <- data.frame(hw = c(hwInfo[,1], rep(1, size)),
                                             hw.number = c(hwInfo[,2],
                                                           rep(counter, size)))

                        # Increment counter for heatwave number
                        # Advance i to next position after heatwave
                        counter <- counter + 1
                        i <- i + size
                } else {
                        # If no heatwave at i, then add zeros to the end of the
                        # dataframe
                        hwInfo <- rbind(hwInfo, c(0, 0))
                        i <- i + 1
                }
        }

        # Combine the original dataframe with the heatwave characteristics
        # matrix. Notice the placeholder row is excluded.
        return(data.frame(datafr, hwInfo[-1,]))
}

#' Identify heatwaves in a time series
#'
#' This function identifies heatwaves in a time series of temperature
#' data using a heatwave definition that a heatwave must be two or more
#' days with temperatures equal to or above some threshold temperature.
#'
#' @inheritParams closest_point
#' @inheritParams IDheatwaves
#'
#' @return Returns the dataframe entered as \code{datafr}, but with new
#'    columns providing heatwave identifiers. The returned dataframe will
#'    have new columns for whether a day was part of a heatwave (\code{hw},
#'    0 / 1), if it was part of a heatwave, the number of the heatwave
#'    (\code{hw.number}), and whether the day was the first day in a heatwave
#'    (\code{first.hw.day}, 0 /1).
#'
#' @note This function gives identical results to the \link{\code{IDHeatwavesR}}
#'    function, but should take less time to run.
#'
#' @export
IDHeatwavesCPPwrapper <- function(datafr, threshold){
        colnames(datafr) <- c("date", "tmpd")

        # Find temperatures that exceed the threshold. One means the
        # measurement equals or exceeds threshold.
        tempsExceedingthreshold <- as.numeric(datafr[ , 2] >= threshold)

        # Add zero onto the end of the vector. The CPP routine needs
        # this to work properly.
        tempsExceedingthreshold <- c(tempsExceedingthreshold, 0)

        # Identify heatwaves using the C++ functions.
        heatwaves <- IDHeatwavesCPP(heatwaveLength = 2,
                                    tempsExceedingCutoff = tempsExceedingthreshold)

        # Attach heatwaves columns onto the data in the datafr
        # variable. Note that the final row, which contains zeroes as
        # placeholders, is excluded.
        hwdata <- data.frame(datafr, heatwaves[-(dim(heatwaves)[1]),])
        return(hwdata)
}
