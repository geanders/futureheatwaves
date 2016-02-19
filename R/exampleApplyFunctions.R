#' Calculate number of heatwaves
#'
#' This function takes a dataframe of heatwaves, as created by
#' \code{\link{gen_hw_set}}, and calculates the number of heatwaves in
#' the dataframe.
#'
#' To calculate the number of heatwaves, this function determines the number
#' of rows in the dataframe, since \code{\link{gen_hw_set}} outputs a
#' dataframe with one and only one row per heatwave.
#'
#' @param hw_datafr A dataframe of heatwaves and their characteristics, as
#'    created by \code{\link{gen_hw_set}}
#'
#' @return A numeric value giving the number of heatwaves in the dataframe
#'
#' @export
number_of_heatwaves <- function(hw_datafr){
        out <- nrow(hw_datafr)
        return(out)
}

#' Calculate total heatwave days
#'
#' This function takes a dataframe of heatwaves, as created by
#' \code{\link{gen_hw_set}}, and calculates the total number of heatwave days in
#' the dataframe (sum of the number of days in each heatwave for all heatwaves
#' in the dataset).
#'
#' @param hw_datafr A dataframe of heatwaves and their characteristics, as
#'    created by \code{\link{gen_hw_set}}
#'
#' @return A numeric value giving the number of heatwave days in the dataframe
#'
#' @export
heatwave_days <- function(hw_datafr){
        out <- sum(hw_datafr$length)
        return(out)
}

#' Calculate average length of heatwaves
#'
#' This function takes a dataframe of heatwaves, as created by
#' \code{\link{gen_hw_set}}, and calculates the average length (in days) of
#' heatwaves in the dataframe.
#'
#' @param hw_datafr A dataframe of heatwaves and their characteristics, as
#'    created by \code{\link{gen_hw_set}}
#'
#' @return A numeric value giving the average length of heatwaves in the
#'    dataframe
#'
#' @export
average_length <- function(hw_datafr){
        out <- mean(hw_datafr$length)
        return(out)
}

#' Calculate average temperature of heatwaves
#'
#' This function takes a dataframe of heatwaves, as created by
#' \code{\link{gen_hw_set}}, and calculates the average of the mean daily
#' temperature during each heatwave across all heatwaves in the dataframe.
#'
#' @param hw_datafr A dataframe of heatwaves and their characteristics, as
#'    created by \code{\link{gen_hw_set}}
#'
#' @return A numeric value giving the average of the mean daily
#'    temperature during each heatwave across all heatwaves in the dataframe,
#'    in degrees Fahrenheit
#'
#' @export
average_mean_temp <- function(hw_datafr){
        out <- mean(hw_datafr$mean.temp)
        return(out)
}
