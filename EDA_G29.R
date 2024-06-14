               #### Exploratory Data Analysis - Team 29 ####

# This code will be to conduct EDA as part of the major group assignment for COMM3501 #
               
# Installing and loading packages
package_list <- c("ggplot2", "dplyr", "tidyr", "janitor", "skimr")

install.packages(package_list)

library(ggplot2)
library(dplyr)
library(tidyr)
library(janitor)
library(skimr)
# Uploading the dataset
original_data <- read.csv("A3_Dataset_2023.csv")

# General data cleaning
original_data <- original_data %>%
  clean_names() %>%
  remove_empty(c("rows", "cols"))

ggplot(data = original_data, aes(age_next)) + geom_bar()
ggplot(data = original_data, aes(gender)) + geom_bar()
ggplot(data = original_data, aes(smoker_status)) + geom_bar()
ggplot(data = original_data, aes(home_state)) + geom_bar()


# understanding the data
skim(original_data)


# creating subset of NA's to see what we are missing and if it would be appropriate to remove NAs
na_subset <- original_data[apply(
  original_data, 
  1, 
  function(x) any(is.na(x))
), ]

ggplot(data = na_subset, aes(underwriter)) + geom_bar()
ggplot(data = na_subset, aes(age_next)) + geom_bar()
ggplot(data = na_subset, aes(gender)) + geom_bar()
ggplot(data = na_subset, aes(smoker_status)) + geom_bar()
ggplot(data = na_subset, aes(home_state)) + geom_bar()



# Removing all missing data since they look like obvious data errors, with 0 premiums,
# missing adviser IDs, missing age, zero income with no information on employment.
# Additionally, it constitutes approx 1.1% of the data, thus making no significant
# impact to data quality and validity.

original_data <- na.omit(original_data)

