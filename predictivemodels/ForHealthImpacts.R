# Set up libraries you'll need (you'll need to install with `install.packages`
# before you can use for the first time)
library(devtools)
library(dplyr)
library(tree)
library(randomForest)
library(gbm)
library(readxl)

# Load predictive models
load("predictivemodels/unpr.tree.rose.RData")
load("predictivemodels/bag.tree.rose.RData")
load("predictivemodels/boost.tree.rose.RData")
load("predictivemodels/rf_mod.RData")

# Load data on population projections and land area
proj_pops <- read_excel("predictivemodels/CountyPop_Brooke.xlsx")
colnames(proj_pops) <- gsub(" ", "", colnames(proj_pops))
land_area <- read.csv("predictivemodels/land_area.csv",
                      header = TRUE, as.is = TRUE)
load("predictivemodels/base_deaths.RData")

# Function to pull population data based on a start year
pull_proj_pops <- function(proj_pops, start_year){
        end_year <- start_year + 19
        year_range <- paste(c(start_year, end_year), collapse = "-")
        pop_data <- proj_pops
        colnames(pop_data)[colnames(pop_data) == year_range] <- "pop"

        pop_data <- pop_data %>%
                dplyr::select(city, pop) %>%
                dplyr::group_by(city) %>%
                dplyr::summarize(pop100 = sum(pop)) %>%
                dplyr::left_join(land_area, by = "city") %>%
                dplyr::mutate(pop.density = pop100 / arealand) %>%
                dplyr::select(-arealand) %>%
                dplyr::left_join(base_deaths, by = "city")

        return(pop_data)
}

## ------------------------------------------------------------------------

# Code to run to get results

out <- "~/tmp/results"  ## Replace with the path to where you have heatwave
                        ## dataframes stored

# Predict frequency of very dangerous heatwaves using the bagging model
apply_all_models(out = out, FUN = "tree_frequency", start_year = 2076)
apply_all_models(out = out, FUN = "tree_frequency",
                 city_specific = TRUE, start_year = 1985)

# Predict exposure (person-days) to very dangerous heatwaves using the bagging
# model
apply_all_models(out = out, FUN = "tree_exposure", start_year = 1985)
apply_all_models(out = out, FUN = "bag_exposure",
                 city_specific = TRUE, start_year = 1985)

# Predict exposure (days) to very dangerous heatwaves using the bagging model
apply_all_models(out = out, FUN = "bag_days", start_year = 1985)
apply_all_models(out = out, FUN = "bag_days",
                 city_specific = TRUE, start_year = 1985)

# Predict excess deaths
apply_all_models(out = out, FUN = "rf_excess_deaths", start_year = 1985)
apply_all_models(out = out, FUN = "rf_excess_deaths",
                 city_specific = TRUE, start_year = 1985)

# Example of saving model results to file
to_save <- apply_all_models(out = out, FUN = "bag_frequency",
                            city_specific = TRUE, start_year = 1985)
write.csv(to_save, file = "~/tmp/To_Save.csv", ## Replace with filename you want
          row.names = FALSE)

## ------------------------------------------------------------------------

# Functions you'll apply across all the heatwaves

custom_tree_frequency <- function(hw_datafr){
        predictions <- ifelse(hw_datafr$max.temp.quantile >= 0.9989,
                              "very", "other" )
        adj_very <-adj_for_precision(predictions = predictions,
                                     precision = 0.012,
                                     false_omission = 0.0)
        return(adj_very)
}

custom_tree_exposure <- function(hw_datafr, start_year){
        hw_datafr <- add_pop_area(hw_datafr, start_year = start_year)

        predictions <- ifelse(hw_datafr$max.temp.quantile >= 0.9989,
                              "very", "other" )

        adj_exp <- process_exposure(hw_datafr = hw_datafr,
                                    prediction = predictions,
                                    precision = 0.012,
                                    false_omission = 0.0)
        return(adj_exp)
}

custom_tree_days <- function(hw_datafr, start_year){
        hw_datafr <- add_pop_area(hw_datafr, start_year = start_year)

        predictions <- ifelse(hw_datafr$max.temp.quantile >= 0.9989,
                              "very", "other" )

        adj_days <- process_days(hw_datafr = hw_datafr,
                                 prediction = predictions,
                                 precision = 0.012,
                                 false_omission = 0.0)
        return(adj_days)
}

tree_frequency <- function(hw_datafr, start_year){
        hw_datafr <- add_pop_area(hw_datafr, start_year = start_year)

        predictions <- predict(unpr.tree.rose,
                               newdata = hw_datafr,
                               type = "class")

        adj_very <-adj_for_precision(predictions = predictions,
                                     precision = 0.026,
                                     false_omission = 0.0)
        return(adj_very)
}

tree_exposure <- function(hw_datafr, start_year){
        hw_datafr <- add_pop_area(hw_datafr, start_year = start_year)

        predictions <- predict(unpr.tree.rose,
                               newdata = hw_datafr,
                               type = "class")

        adj_exp <- process_exposure(hw_datafr = hw_datafr,
                                    prediction = predictions,
                                    precision = 0.026,
                                    false_omission = 0.0)
        return(adj_exp)
}

tree_days <- function(hw_datafr, start_year){
        hw_datafr <- add_pop_area(hw_datafr, start_year = start_year)

        predictions <- predict(unpr.tree.rose,
                               newdata = hw_datafr,
                               type = "class")

        adj_days <- process_days(hw_datafr = hw_datafr,
                                 prediction = predictions,
                                 precision = 0.026,
                                 false_omission = 0.0)
        return(adj_days)
}

bag_frequency <- function(hw_datafr, start_year){
        hw_datafr <- add_pop_area(hw_datafr, start_year = start_year)

        predictions <- predict(bag.tree.rose,
                               newdata = hw_datafr)

        adj_very <-adj_for_precision(predictions = predictions,
                                     precision = 0.026,
                                     false_omission = 0.0)
        return(adj_very)
}

bag_exposure <- function(hw_datafr, start_year){
        hw_datafr <- add_pop_area(hw_datafr, start_year = start_year)

        predictions <- predict(bag.tree.rose,
                               newdata = hw_datafr)

        adj_exp <- process_exposure(hw_datafr = hw_datafr,
                                    prediction = predictions,
                                    precision = 0.026,
                                    false_omission = 0.0)
        return(adj_exp)
}

bag_days <- function(hw_datafr, start_year){
        hw_datafr <- add_pop_area(hw_datafr, start_year = start_year)

        predictions <- predict(bag.tree.rose,
                               newdata = hw_datafr)

        adj_days <- process_days(hw_datafr = hw_datafr,
                                 prediction = predictions,
                                 precision = 0.026,
                                 false_omission = 0.0)
        return(adj_days)
}

boost_frequency <- function(hw_datafr, start_year){
        hw_datafr <- add_pop_area(hw_datafr, start_year = start_year)

        predictions <- predict(boost.tree.rose,
                               newdata = hw_datafr,
                               n.trees = 500)
        predictions <- ifelse(predictions > 0, "very", "other")

        adj_very <-adj_for_precision(predictions = predictions,
                                     precision = 0.023,
                                     false_omission = 0.0)
        return(adj_very)
}

boost_exposure <- function(hw_datafr, start_year){
        hw_datafr <- add_pop_area(hw_datafr, start_year = start_year)

        predictions <- predict(boost.tree.rose,
                               newdata = hw_datafr,
                               n.trees = 500)
        predictions <- ifelse(predictions > 0, "very", "other")

        adj_exp <- process_exposure(hw_datafr = hw_datafr,
                                    prediction = predictions,
                                    precision = 0.023,
                                    false_omission = 0.0)
        return(adj_exp)
}

boost_days <- function(hw_datafr, start_year){
        hw_datafr <- add_pop_area(hw_datafr, start_year = start_year)

        predictions <- predict(boost.tree.rose,
                               newdata = hw_datafr,
                               n.trees = 500)
        predictions <- ifelse(predictions > 0, "very", "other")

        adj_days <- process_days(hw_datafr = hw_datafr,
                                 prediction = predictions,
                                 precision = 0.023,
                                 false_omission = 0.0)
        return(adj_days)
}

rf_excess_deaths <- function(hw_datafr, start_year){
        hw_datafr <- add_pop_area(hw_datafr, start_year = start_year) %>%
                dplyr::select(mean.temp, max.temp, min.temp, length,
                              start.doy, start.month, days.above.80,
                              days.above.85, days.above.90,
                              days.above.95, days.above.99th,
                              days.above.99.5th, first.in.season,
                              mean.temp.quantile, max.temp.quantile,
                              min.temp.quantile, mean.temp.1,
                              mean.summer.temp, pop100, pop.density,
                              mort_rate)

        pred_log_rr <- predict(rf_mod, newdata = hw_datafr)

        hw_length <- hw_datafr$length
        base_mort <- hw_datafr$pop100 * hw_datafr$mort_rate

        exp_excess <- hw_length * base_mort * (exp(pred_log_rr) - 1)

        return(sum(exp_excess))
}

# Helper functions for those functions (don't use directory in
# `apply_all_models`)
process_exposure <- function(hw_datafr, predictions, precision, false_omission){

        exp_projs <- data.frame(predictions = predictions,
                                length = hw_datafr$length,
                                pop = hw_datafr$pop100) %>%
                mutate(exposure = length * pop) %>%
                group_by(predictions) %>%
                summarise(exposure = sum(exposure))

        # Add "very" with no exposure if necessary
        if(!("very" %in% exp_projs$predictions)){
                exp_projs <- rbind(exp_projs,
                                   data.frame(predictions = "very",
                                              exposure = 0))
        }

        adj_exp <- exp_projs$exposure[exp_projs$predictions == "other"] *
                               false_omission +
                exp_projs$exposure[exp_projs$predictions == "very"] * precision

        return(adj_exp)
}

process_days <- function(hw_datafr, predictions, precision, false_omission){

        exp_projs <- data.frame(predictions = predictions,
                                length = hw_datafr$length) %>%
                rename(exposure = length) %>%
                group_by(predictions) %>%
                summarise(exposure = sum(exposure))

        # Add "very" with no exposure if necessary
        if(!("very" %in% exp_projs$predictions)){
                exp_projs <- rbind(exp_projs,
                                   data.frame(predictions = "very",
                                              exposure = 0))
        }

        adj_exp <- exp_projs$exposure[exp_projs$predictions == "other"] *
                false_omission +
                exp_projs$exposure[exp_projs$predictions == "very"] * precision

        return(adj_exp)
}

adj_for_precision <- function(predictions, precision, false_omission){
        tot_very <- sum(predictions == "very")
        tot_less <- sum(predictions == "less")
        out <- tot_very * precision + tot_less * false_omission
        return(out)
}

add_pop_area <- function(hw_datafr, start_year){
        hw_datafr$city <- as.character(hw_datafr$city)
        pop_data <- pull_proj_pops(proj_pops, start_year)

        hw_datafr <- left_join(hw_datafr, pop_data, by = "city")
        return(hw_datafr)
}
