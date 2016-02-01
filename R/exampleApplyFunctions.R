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
#'    created by \code{\link{gen_hw_set}}.
#'
#' @return A numeric value giving the number of heatwaves in the dataframe.
#'
#' @export
number_of_heatwaves <- function(hw_datafr){
        out <- nrow(hw_datafr)
        return(out)
}

heatwave_days <- function(hw_datafr){
        out <- sum(hw_datafr$length)
        return(out)
}

average_length <- function(hw_datafr){
        out <- mean(hw_datafr$length)
        return(out)
}

average_mean_temp <- function(hw_datafr){
        out <- mean(hw_datafr$mean.temp)
        return(out)
}
