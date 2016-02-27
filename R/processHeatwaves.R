#' Create heatwave dataframe for an ensemble
#'
#' This function takes inputs, from \code{\link{processProjections}}, on the
#' projection data for an ensemble member, the thresholds for communities for
#' the ensemble, and \code{global} and \code{custom} objects with user
#' specifications. Using these inputs, the function creates a dataframe with
#' heatwaves identified and characterized for the ensemble member.
#'
#' @param ensembleSeries A list object giving the projection time series as
#'    well as a variety of other information for a single ensemble member.
#'    This is the output of \code{\link{processEnsemble}}.
#' @inheritParams processProjections
#' @inheritParams processModel
#'
#' @return The combined dataframe of identified and characterized heatwaves for
#'    selected projection date range for all communities specified by the user.
#'    This dataframe includes the following columns:
#' \itemize{
#'    \item hw.number: A sequential number identifying each heatwave in a city;
#'    \item mean.temp: Average daily temperature across all days in the
#'       heatwave, in degrees Fahrenheit;
#'    \item max.temp: Highest daily temperature across days in the
#'       heatwave, in degrees Fahrenheit;
#'    \item min.temp: Lowest daily temperature across days in the
#'       heatwave, in degrees Fahrenheit
#'    \item length: Number of days in the heatwave;
#'    \item start.date: Date of the first day of the heatwave;
#'    \item end.date: Date of the last day of the heatwave;
#'    \item start.doy: Day of the year of the first day of the heatwave
#'       (1 = Jan. 1, etc.);
#'    \item start.month: Month in which the heatwave started (1 = January,
#'       etc.);
#'    \item days.above.80: Number of days in the heatwave above 80 degrees
#'        Fahrenheit;
#'    \item days.above.85: Number of days in the heatwave above 85 degrees
#'        Fahrenheit;
#'    \item days.above.90: Number of days in the heatwave above 90 degrees
#'        Fahrenheit;
#'    \item days.above.95: Number of days in the heatwave above 90 degrees
#'        Fahrenheit;
#'    \item days.above.99th: Number of days in the heatwave above the 99th
#'        percentile temperature for the city, using the period specified
#'        by the user with the \code{referenceBoundaries} argument in
#'        \code{\link{gen_hw_set}} as a reference for determining these
#'        percentiles;
#'    \item days.above.99.5th: Number of days in the heatwave above the 99.5th
#'        percentile temperature for the city, using the period specified
#'        by the user with the \code{referenceBoundaries} argument in
#'        \code{\link{gen_hw_set}} as a reference for determining these
#'        percentiles;
#'    \item first.in.season: Whether the heatwave was the first to occur in its
#'        calendar year (Note: this characteristic is likely not useful in
#'        southern hemisphere studies.);
#'    \item threshold.temp: The temperature used as the threshold for the
#'        heatwave definition in the city;
#'    \item mean.temp.quantile: The percentile of the average daily mean
#'        temperature during the heatwave compared to the city's year-round
#'        temperature distribution, based on the temperatures for the city
#'        during the period specified by the \code{referenceBoundaries}
#'        argument in \code{\link{gen_hw_set}};
#'    \item max.temp.quantile: The percentile of the highest daily mean
#'        temperature during the heatwave compared to the city's year-round
#'        temperature distribution;
#'    \item min.temp.quantile: The percentile of the lowest daily mean
#'        temperature during the heatwave compared to the city's year-round
#'        temperature distribution;
#'    \item mean.temp.1: The city's average year-round temperature, based
#'        on the temperatures for the city during the period specified by
#'        the \code{referenceBoundaries} argument in
#'        \code{\link{gen_hw_set}};
#'    \item mean.summer.temp: The city's average May--September
#'        temperature, based on the temperatures for the community during the
#'        period specified by the \code{referenceBoundaries} argument
#'        in \code{\link{gen_hw_set}}; and
#'    \item city: The identifier for the community, as given in the file
#'        specified in the \code{citycsv} argument of
#'        \code{\link{gen_hw_set}}.
#' }
#' An example of the output of this function is available as the
#' \code{\link{hw_datafr}} dataset and can be accessed using
#' \code{data(hw_datafr)}.
formHwFrame <- function(ensembleSeries, thresholds, global, custom){
        # Acquire list of heatwave dataframes for each city
        hwDataframeList <- apply(data.frame(thresholds), 1,
                                 createCityProcessor(global = global),
                                 ensembleSeries = ensembleSeries,
                                 custom = custom)

        # Combine the heatwave dataframes contained in hwDataframeList into
        # a single dataframe
        hwFrame <- consolidate(hwDataframeList)

        return(hwFrame)
}

#' Create closure to identify and aggregate heatwaves
#'
#' This function creates a closure that returns a
#'    dataframe with the identified heatwaves and heatwave characteristics for
#'    a given city for the specified projection period, as generated by the
#'    \code{\link{createHwDataframe}} function.
#'
#' @inheritParams processModel
#'
#' @return This function creates a closure that takes inputs of \code{threshold},
#'    \code{ensembleSeries}, and \code{custom} and will
#'    find and characterize all heatwaves in all communities for a given
#'    ensemble. See the help file for \code{\link{formHwFrame}} for more
#'    information on the format of the dataframe created by this closure.

#' @note The closure encapsulates an incrementer varaible and advances it
#'    with every call. This variable is used to index into the \code{cities}
#'    vector from the \code{global} object passed into this function.
createCityProcessor <- function(global){
        # incrementer
        i <- 1

        function(threshold, ensembleSeries, custom){
                city <- as.character(global$cities[i,1])
                cat("Creating heatwave dataframe ~~ City: ", city,
                    " ~~ City Number: ", i, " ~~ Cutoff: ", threshold, "\n")

                datafr <- data.frame(ensembleSeries$dates,
                                     ensembleSeries$series[,i])

                # Identify all heatwaves for the given city
                heatwaves <- IDheatwaves(threshold = threshold,
                                         datafr = datafr,
                                         global = global,
                                         custom = custom)

                # Aggregate heatwaves for the given city
                hwDataframeList <- createHwDataframe(city = city,
                                                     threshold = threshold,
                                                     heatwaves = heatwaves,
                                                     ensembleSeries = ensembleSeries,
                                                     i = i,
                                                     global = global,
                                                     custom = custom)

                i <<- i + 1
                return(hwDataframeList)
        }
}

#' Consolidate heatwave dataframes
#'
#' This function combines all identified city-specific heatwave dataframes
#' together into a single dataframe. This function is used to create a single
#' dataframe with all heatwaves from all study cities for an ensemble
#' member.
#'
#' @param hwDataframeList A list object where each element is the dataframe
#'    of heatwaves, created by the closure created by
#'    \code{\link{createCityProcessor}}, for a single city.
#'
#' @return A combined dataframe version of the list object that was passed as an
#'    argument.
consolidate <- function(hwDataframeList){
        all <- hwDataframeList[[1]]
        for(i in 2:length(hwDataframeList)){
                all <- rbind(all, hwDataframeList[[i]])
        }
        return(all)
}

#' Characterize heatwaves
#'
#' This function takes a dataframe with identified heatwaves and returns
#' a dataframe that lists and characterizes all of the heatwaves.
#'
#' @param city A character vector with the identification of the city
#'    being processed.
#' @param heatwaves A dataframe with the following columns:
#'    \itemize{
#'    \item \code{date}: Date of each observation, in class "Date";
#'    \item \code{tmpd}: Temperature in degrees Fahrenheit;
#'    \item \code{hw}: A binary variable designating whether a day is in a
#'    heatwave (0: not in a heatwave; 1: in a heatwave); and
#'    \item \code{hw.number}: A numeric value, 0 if the day was not part of a
#'    heatwave, otherwise the number of the heatwave to which the day belonged.
#'    }
#'    This is the format of the output of \code{\link{IDheatwaves}}.
#' @param i An index specifying which city is being processed. This corresponds
#'    to the order of the cities in the \code{citycsv} file specified in
#'    \code{\link{gen_hw_set}}.
#' @inheritParams processModel
#' @inheritParams formHwFrame
#' @inheritParams IDheatwaves
#'
#' @return A dataframe of identified and characterized heat waves for a single
#'    city and single ensemble member. Each row of this dataframe represents a
#'    heatwave, with the following columns:
#' \itemize{
#'    \item hw.number: A sequential number identifying each heatwave in a city;
#'    \item mean.temp: Average daily temperature across all days in the
#'       heatwave, in degrees Fahrenheit;
#'    \item max.temp: Highest daily temperature across days in the
#'       heatwave, in degrees Fahrenheit;
#'    \item min.temp: Lowest daily temperature across days in the
#'       heatwave, in degrees Fahrenheit
#'    \item length: Number of days in the heatwave;
#'    \item start.date: Date of the first day of the heatwave;
#'    \item end.date: Date of the last day of the heatwave;
#'    \item start.doy: Day of the year of the first day of the heatwave
#'       (1 = Jan. 1, etc.);
#'    \item start.month: Month in which the heatwave started (1 = January,
#'       etc.);
#'    \item days.above.80: Number of days in the heatwave above 80 degrees
#'        Fahrenheit;
#'    \item days.above.85: Number of days in the heatwave above 85 degrees
#'        Fahrenheit;
#'    \item days.above.90: Number of days in the heatwave above 90 degrees
#'        Fahrenheit;
#'    \item days.above.95: Number of days in the heatwave above 90 degrees
#'        Fahrenheit;
#'    \item days.above.99th: Number of days in the heatwave above the 99th
#'        percentile temperature for the city, using the period specified
#'        by the user with the \code{referenceBoundaries} argument in
#'        \code{\link{gen_hw_set}} as a reference for determining these
#'        percentiles;
#'    \item days.above.99.5th: Number of days in the heatwave above the 99.5th
#'        percentile temperature for the city, using the period specified
#'        by the user with the \code{referenceBoundaries} argument in
#'        \code{\link{gen_hw_set}} as a reference for determining these
#'        percentiles;
#'    \item first.in.season: Whether the heatwave was the first to occur in its
#'        calendar year (Note: this characteristic is likely not useful in
#'        southern hemisphere studies.);
#'    \item threshold.temp: The temperature used as the threshold for the
#'        heatwave definition in the city;
#'    \item mean.temp.quantile: The percentile of the average daily mean
#'        temperature during the heatwave compared to the city's year-round
#'        temperature distribution, based on the temperatures for the city
#'        during the period specified by the \code{referenceBoundaries}
#'        argument in \code{\link{gen_hw_set}};
#'    \item max.temp.quantile: The percentile of the highest daily mean
#'        temperature during the heatwave compared to the city's year-round
#'        temperature distribution;
#'    \item min.temp.quantile: The percentile of the lowest daily mean
#'        temperature during the heatwave compared to the city's year-round
#'        temperature distribution;
#'    \item mean.temp.1: The city's average year-round temperature, based
#'        on the temperatures for the city during the period specified by
#'        the \code{referenceBoundaries} argument in
#'        \code{\link{gen_hw_set}};
#'    \item mean.summer.temp: The city's average May--September
#'        temperature, based on the temperatures for the community during the
#'        period specified by the \code{referenceBoundaries} argument
#'        in \code{\link{gen_hw_set}}; and
#'    \item city: The identifier for the community, as given in the file
#'        specified in the \code{citycsv} argument of
#'        \code{\link{gen_hw_set}}.
#' }
#'
#' @note When calculating relative characteristics of heatwaves, like the
#' relative value of the heatwave's mean temperature, this function uses a
#' time series from the date ranges specified by the user using the
#' \code{referenceBoundaries} option in \code{\link{gen_hw_set}}. By
#' default, these references are based on projection data from 2070 to
#' 2079.
#'
#' @importFrom dplyr %>%
createHwDataframe <- function(city, threshold, heatwaves,
                              ensembleSeries, i, global, custom){

        heatwaves2 <- dplyr::filter_(heatwaves, ~ hw == 1)

        if(custom["createHwDataframe"][[1]]){
                ref_temps <- ensembleSeries$reference[ , i]
                ref_dates <- ensembleSeries$reference_dates
        } else {
                ref_temps <- ensembleSeries$series[ , i]
                ref_dates <- ensembleSeries$dates
        }

        hw.frame <- dplyr::group_by_(heatwaves2, ~ hw.number) %>%
                dplyr::summarize_(mean.temp = ~ mean(tmpd),
                                 max.temp = ~ max(tmpd),
                                 min.temp = ~ min(tmpd),
                                 length = ~ length(unique(date)),
                                 start.date = ~ date[1],
                                 end.date = ~ date[length(date)],
                                 start.doy = ~ as.POSIXlt(date[1])$yday,
                                 start.month = ~ as.POSIXlt(date[1])$mon + 1,
                                 days.above.80 = ~ length(date[tmpd > 80]),
                                 days.above.85 = ~ length(date[tmpd > 85]),
                                 days.above.90 = ~ length(date[tmpd > 90]),
                                 days.above.95 = ~ length(date[tmpd > 95]),
                                 days.above.99th = ~ length(date[tmpd >
                                                stats::quantile(ref_temps, .99,
                                                         na.rm = TRUE)]),
                                 days.above.99.5th = ~ length(date[tmpd >
                                                stats::quantile(ref_temps, .995,
                                                         na.rm = TRUE)]))

        hw.frame$first.in.season <- c(1, rep(NA, nrow(hw.frame) - 1))
        for(i in 2:nrow(hw.frame)){
                if(as.POSIXlt(hw.frame$start.date)$year[i] !=
                   as.POSIXlt(hw.frame$start.date)$year[i - 1]){
                        hw.frame$first.in.season[i] <- 1
                } else {
                        hw.frame$first.in.season[i] <- 0
                }
        }

        hw.frame$threshold <- threshold

        dist.tmpd <- stats::ecdf(ref_temps)
        hw.frame$mean.temp.quantile <- dist.tmpd(hw.frame$mean.temp)
        hw.frame$max.temp.quantile <- dist.tmpd(hw.frame$max.temp)
        hw.frame$min.temp.quantile <- dist.tmpd(hw.frame$min.temp)

        hw.frame$mean.temp.1 <- mean(ref_temps)
        # Summertime is months May through September
        summertime <- as.POSIXlt(ref_dates)$mon %in% c(4:8)
        hw.frame$mean.summer.temp <- mean(ref_temps[summertime])

        hw.frame$city <- city

        return(hw.frame)
}

