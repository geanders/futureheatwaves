makeMap <- function(locationsList, cities, sampleEnsemble){
        identical <- checkLocations(locationsList)
        if(identical){
                mapCities(locationsList[1], cities, sampleEnsemble)
        }
}

checkLocations <- function(locationsList){
        identical <- sapply(locationsList, function(locations, locationsList){
                identical2 <- sapply(locationsList, function(locations2, locations){
                        if(identical(locations, locations2)){
                                return(TRUE)
                        }
                },
                locations)
                return(all(identical2))
        },
        locationsList)
        return(all(identical))
}

#TODO: SWAP LONGITUDE AND LATITUDE
mapCities <- function(locations, cities, sampleEnsemble){
        latlong <- readLatLong(sampleEnsemble)
        dataCoordinates <- latlong[locations]
        cities <- cities[,1:3]
        cities <- -cities$long
        states <- map_data("state")
        map <- ggplot()
        map <- map + geom_polygon( data = states, aes(x = long, y = lat, group = group), colour = "white", fill = "grey10")
        map <- map + geom_text( data = cities, hjust = .5, vjust = -.5, aes( x = long, y = lat, label = cities$city), colour = "gold2", size = 4)



        map("state")
        intermediate <- gcIntermediate(cities[,2:3], locations)
        lines(intermediate, col = 'red')
        text(cities[,2], cities[,3], cities[,1], pos = 1)
}
