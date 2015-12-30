#' Aggregate by heatwave
#'
#' @param city Index of cities global
#' @param threshold Threshold temperature for this city
#' @param heatwaves data.frame(dates, thresholds)
#' @param percentile Percentile for threshold)
#'
#' @return Result is a dataframe where each row represents a heatwave.
createHwDataframe <- function(city = stop("Unspecified city"),
                              threshold = stop("Unspecified threshold"),
                              heatwaves = stop("'heatwaves' unspecified"),
                              percentile = .98,
                              ensemble,
                              i,
                              global, custom){
        hold <<- heatwaves

        intermediate <- heatwaves
        heatwaves2 <- subset(intermediate, hw == 1)

        if(custom["createHwDataframe"] != FALSE){
                datafr <- data.frame(ensemble$dates, ensemble$reference[,i])
                heatwaves <- IDheatwaves(city, threshold, 2, datafr,
                                         global, custom)
        }

        bloodhound <<- heatwaves2
        bark <<- heatwaves

        hw.frame <- dplyr::group_by(heatwaves2, hw.number) %>%
                dplyr::summarize(mean.temp = mean(tmpd),
                          max.temp = max(tmpd),
                          min.temp = min(tmpd),
                          length = length(unique(date)),
                          start.date = date[1],
                          end.date = date[length(date)],
                          start.doy = as.POSIXlt(date[1])$yday,
                          start.month = as.POSIXlt(date[1])$mon + 1,
                          days.above.80 = length(date[tmpd > 80]),
                          days.above.85 = length(date[tmpd > 85]),
                          days.above.90 = length(date[tmpd > 90]),
                          days.above.95 = length(date[tmpd > 95]),
                          days.above.99th = length(date[tmpd > quantile(heatwaves$tmpd, .99, na.rm = TRUE)]),
                          days.above.99.5th = length(date[tmpd > quantile(heatwaves$tmpd, .995, na.rm = TRUE)]))

        hw.frame$first.in.season <- c(1, rep(NA, nrow(hw.frame) - 1))
        for(i in 2:nrow(hw.frame)){
                if(as.POSIXlt(hw.frame$start.date)$year[i] !=
                   as.POSIXlt(hw.frame$start.date)$year[i - 1]){
                        hw.frame$first.in.season[i] <- 1
                } else {
                        hw.frame$first.in.season[i] <- 0
                }
        }

        hw.frame$"98th.temp" <- threshold

        dist.tmpd <- ecdf(heatwaves$tmpd)
        hw.frame$mean.temp.quantile <- dist.tmpd(hw.frame$mean.temp)
        hw.frame$max.temp.quantile <- dist.tmpd(hw.frame$max.temp)
        hw.frame$min.temp.quantile <- dist.tmpd(hw.frame$min.temp)

        hw.frame$mean.temp.1 <- mean(heatwaves$tmpd)
        summertime <- as.POSIXlt(heatwaves$date)$mon %in% c(4:8)
        hw.frame$mean.summer.temp <- mean(heatwaves$tmpd[summertime])

        hw.frame$city <- city

        return(hw.frame)
}
