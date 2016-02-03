# Set up libraries you'll need (you'll need to install with `install.packages`
# before you can use for the first time)
library(devtools)
library(dplyr)
library(tree)
library(randomForest)
library(gbm)

# Load predictive models
load("predictivemodels/unpr.tree.rose.RData")
load("predictivemodels/bag.tree.rose.RData")
load("predictivemodels/boost.tree.rose.RData")

# Load data on population projections and land area
proj_pops <- read.csv("predictivemodels/projected_populations.csv",
                      header = TRUE)
land_area <- read.csv("predictivemodels/land_area.csv",
                      header = TRUE)
proj_pops <- group_by(proj_pops, city) %>%
        summarise(pop100 = sum(SSP5)) %>%
        left_join(land_area) %>%
        mutate(pop.density = pop100 / arealand) %>%
        dplyr::select(-arealand)

## ------------------------------------------------------------------------

# Code to run to get results

out <- "~/tmp/results"  ## Replace with the path to where you have heatwave
                        ## dataframes stored

# Predict frequency of very dangerous heatwaves using the custom tree model
apply_all_models(out = out, FUN = "bag_frequency")
apply_all_models(out = out, FUN = "bag_frequency",
                 city_specific = TRUE)

# Example of saving model results to file
to_save <- apply_all_models(out = out, FUN = "bag_frequency",
                            city_specific = TRUE)
write.csv(to_save, file = "~/tmp/To_Save.csv", ## Replace with filename you want
          row.names = FALSE)

## ------------------------------------------------------------------------

# Functions you'll apply across all the heatwaves

custom_tree_frequency <- function(hw_datafr){
        predictions <- ifelse(hw_datafr$max.temp.quantile >= 0.9989,
                              "very", "less" )
        adj_very <-adj_for_precision(predictions = predictions,
                                     precision = 0.012,
                                     false_omission = 0.0)
        return(adj_very)
}

tree_frequency <- function(hw_datafr){
        hw_datafr <- add_pop_area(hw_datafr)

        predictions <- predict(unpr.tree.rose,
                               newdata = hw_datafr,
                               type = "class")

        adj_very <-adj_for_precision(predictions = predictions,
                                     precision = 0.026,
                                     false_omission = 0.0)
        return(adj_very)
}

bag_frequency <- function(hw_datafr){
        hw_datafr <- add_pop_area(hw_datafr)

        predictions <- predict(bag.tree.rose,
                               newdata = hw_datafr)

        adj_very <-adj_for_precision(predictions = predictions,
                                     precision = 0.026,
                                     false_omission = 0.0)
        return(adj_very)
}

boost_frequency <- function(hw_datafr){
        hw_datafr <- add_pop_area(hw_datafr)

        predictions <- predict(boost.tree.rose,
                               newdata = hw_datafr,
                               n.trees = 500)
        predictions <- ifelse(predictions > 0, "very", "other")

        adj_very <-adj_for_precision(predictions = predictions,
                                     precision = 0.023,
                                     false_omission = 0.0)
        return(adj_very)
}

# Helper functions for those functions (don't use directory in
# `apply_all_models`)

adj_for_precision <- function(predictions, precision, false_omission){
        tot_very <- sum(predictions == "very")
        tot_less <- sum(predictions == "less")
        out <- tot_very * precision + tot_less * false_omission
        return(out)
}

add_pop_area <- function(hw_datafr){
        hw_datafr$city <- as.character(hw_datafr$city)
        proj_pops$city <- as.character(proj_pops$city)

        hw_datafr <- left_join(hw_datafr, proj_pops, by = "city")
        return(hw_datafr)
}
