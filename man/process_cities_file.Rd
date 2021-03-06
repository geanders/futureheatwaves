% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/IO.R
\name{process_cities_file}
\alias{process_cities_file}
\title{Process city file}
\usage{
process_cities_file(cities, lat_lon_colnames)
}
\arguments{
\item{cities}{Dataframe with study cities and their latitudes
and longitudes.The dataframe must have a column named \code{city} with a
unique identifier for each city in the study, as well as columns for
latitude and longitude. Other columns may be included in the dataset, but
will not be passed through to later code.}

\item{lat_lon_colnames}{A character vector of length two with the column names
in the \code{citycsv} dataframe for latitude (first vector element) and
longitude (second vector element)}
}
\value{
A processed version of the latitude and longitude dataframe.
}
\description{
This function takes the dataframe read in from the file specified with
\code{citycsv} in \code{\link{gen_hw_set}} and renames the columns for
latitude and longitude \code{lat} and \code{lon}, based on
the user's selections in \code{lat_lon_colnames} for
\code{\link{gen_hw_set}}. If there are extra columns besides those and the
\code{city} column, this function removes them.
}
