IDHeatwavesR <- function(city = stop("Error: unspecified city"),
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
