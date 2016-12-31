#' Example data to input to extreme event identifier functions
#'
#' This dataset provides an example of the dataframe to be input into a
#' custom extreme event identification function. This dataset can be used to test a
#' custom extreme event identification function before using the function
#' in \code{\link{gen_hw_set}}.
#'
#' @format A dataframe with 7,300 rows and 2 columns. The variables are:
#' \itemize{
#'   \item date: A Date vector with the date of the projection
#'   \item tmpd: A numeric vector with the temperature of the projection, in
#'               degrees Fahrenheit
#' }
"datafr"

#' Example of extreme event characteristics dataset
#'
#' This dataset provides an example of the structure of the dataframe of
#' extreme events and their characteristics, as created by \code{\link{gen_hw_set}}.
#' This example dataset can be used in developing custom functions to use with
#' \code{\link{apply_all_models}}.
#'
#' @format A dataframe with 258 rows and 23 columns. The variables are:
#' \itemize{
#'    \item hw.number: A sequential number identifying each heat wave in a city;
#'    \item mean.var: Average daily temperature across all days in the
#'       heat wave, in degrees Fahrenheit;
#'    \item max.var: Highest daily temperature across days in the
#'       heat wave, in degrees Fahrenheit;
#'    \item min.var: Lowest daily temperature across days in the
#'       heat wave, in degrees Fahrenheit
#'    \item length: Number of days in the heat wave;
#'    \item start.date: Date of the first day of the heat wave;
#'    \item end.date: Date of the last day of the heat wave;
#'    \item start.doy: Day of the year of the first day of the heat wave
#'       (1 = Jan. 1, etc.);
#'    \item start.month: Month in which the heat wave started (1 = January,
#'       etc.);
#'    \item days.above.abs.threshold.1: Number of days in the heat wave above 80 degrees
#'        Fahrenheit;
#'    \item days.above.abs.threshold.2: Number of days in the heat wave above 85 degrees
#'        Fahrenheit;
#'    \item days.above.abs.threshold.3: Number of days in the heat wave above 90 degrees
#'        Fahrenheit;
#'    \item days.above.abs.threshold.4: Number of days in the heat wave above 90 degrees
#'        Fahrenheit;
#'    \item days.above.99th: Number of days in the heat wave above the 99th
#'        percentile temperature for the city, using the period specified
#'        by the user with the \code{referenceBoundaries} argument in
#'        \code{\link{gen_hw_set}} as a reference for determining these
#'        percentiles;
#'    \item days.above.99.5th: Number of days in the heat wave above the 99.5th
#'        percentile temperature for the city, using the period specified
#'        by the user with the \code{referenceBoundaries} argument in
#'        \code{\link{gen_hw_set}} as a reference for determining these
#'        percentiles;
#'    \item first.in.year: Whether the heat wave was the first to occur in its
#'        calendar year (Note: this characteristic is likely not useful in
#'        southern hemisphere studies.);
#'    \item threshold: The temperature used as the threshold for the
#'        heat wave definition in the city;
#'    \item mean.var.quantile: The percentile of the average daily mean
#'        temperature during the heat wave compared to the city's year-round
#'        temperature distribution, based on the temperatures for the city
#'        during the period specified by the \code{referenceBoundaries}
#'        argument in \code{\link{gen_hw_set}};
#'    \item max.var.quantile: The percentile of the highest daily mean
#'        temperature during the heat wave compared to the city's year-round
#'        temperature distribution;
#'    \item min.var.quantile: The percentile of the lowest daily mean
#'        temperature during the heat wave compared to the city's year-round
#'        temperature distribution;
#'    \item mean.yearround.var: The city's average year-round temperature, based
#'        on the temperatures for the city during the period specified by
#'        the \code{referenceBoundaries} argument in
#'        \code{\link{gen_hw_set}};
#'    \item mean.seasonal.var: The city's average May--September
#'        temperature, based on the temperatures for the city during the
#'        period specified by the \code{referenceBoundaries} argument
#'        in \code{\link{gen_hw_set}}; and
#'    \item city: The identifier for the city, as given in the file
#'        specified in the \code{citycsv} argument of
#'        \code{\link{gen_hw_set}}.
#' }
"hw_datafr"
