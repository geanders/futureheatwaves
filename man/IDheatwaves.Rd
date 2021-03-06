% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/IDHeatwaves.R
\name{IDheatwaves}
\alias{IDheatwaves}
\title{Identify all heat waves in a time series}
\usage{
IDheatwaves(threshold, datafr, global, custom)
}
\arguments{
\item{threshold}{Numeric string with threshold temperature used in
the heat wave definition, in degrees Fahrenheit.}

\item{datafr}{A dataframe with daily temperature projections in the
the city being processed. This dataframe must have two columns:
(1) the first column must have the date of each observation, with
class "Date" and; (2) the second column must have temperatures
in degrees Fahrenheit. In the normal running of this package, this
dataframe will be generated by the closure created by
\code{\link{createCityProcessor}}.}

\item{global}{An list object created by \code{\link{gen_hw_set}} that
includes user specifications (e.g., the path to the output directory, the
path to the input climate projections, the dataframe with city
locations).}

\item{custom}{An list object created by \code{\link{gen_hw_set}} that
includes user specifications (e.g., the name of the R function to
use to identify heat waves, alternative upper and lower year boundaries
for the data used to determine threshold temperatures for the heat wave
definition, alternative upper and lower year boundaries
for the projection period of the heat wave datasets being generated).}
}
\value{
Returns the dataframe entered as \code{datafr}, but with new
   columns providing heat wave identifiers. The returned dataframe will
   have new columns for: \code{hw}: whether a day was part of a heat wave
   (0 : not part of a heat wave / 1: part of a heat wave); and
   \code{hw.number}: if it was part of a heat wave, the number of the
   heat wave (1, 2, etc.).
}
\description{
This function takes a dataframe with columns for date and projected
temperature and adds columns identifying which days belong to a
heat wave and giving separate numbers to identify each discrete heat wave.
}
\note{
The function actually used to identify heat waves in the time series
   is specified in the `IDheatwaves` slot of the `custom` object passed
   into this function. The default is the function
   \code{\link{IDHeatwavesCPPwrapper}}. The user can specify a different
   function using the argument \code{IDheatwavesFunction} in
   \code{\link{gen_hw_set}}.
}
