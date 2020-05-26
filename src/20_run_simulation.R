
# The aim of this code is to run the simulation of alcohol consumption

library(data.table)
library(stapmr)
library(tobalcepi)

# Load data ----------------

# Load the prepared data on alcohol consumption
survey_data <- readRDS("intermediate_data/HSE_2001_to_2017_alcohol.rds")

# Alcohol consumption trends
participation_proportions <- readRDS("intermediate_data/participation_proportions.rds")
consumption_params <- readRDS("intermediate_data/consumption_params.rds")

# Mortality data
mort_data <- readRDS("intermediate_data/alc_mort_data_cause.rds")

# Run simulation ----------------








