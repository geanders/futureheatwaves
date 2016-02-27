#' Example data to input to heatwave identifier functions
#'
#' This dataset provides an example of the dataframe to be input into a
#' custom heatwave identification function. This dataset can be used to test a
#' custom heatwave identification function before using the function
#' in \code{\link{gen_hw_set}}.
#'
#' @format A dataframe with 7,300 rows and 2 columns. The variables are:
#' \itemize{
#'   \item date: A Date vector with the date of the projection
#'   \item tmpd: A numeric vector with the temperature of the projection, in
#'               degrees Fahrenheit
#' }
"datafr"

#' Example of heatwave characteristics dataset
#'
#' This dataset provides an example of the structure of the dataframe of
#' heatwaves and their characteristics, as created by \code{\link{gen_hw_set}}.
#' This example dataset can be used in developing custom functions to use with
#' \code{\link{apply_all_models}}.
#'
#' @format A dataframe with 45 rows and 23 columns. The variables are:
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
"hw_datafr"
