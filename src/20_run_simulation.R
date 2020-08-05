
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

AlcSim_forecast(
  survey_data = survey_data,
  stationary = FALSE,
  burnin = 1,
  participation_proportions = participation_proportions,
  consumption_params = consumption_params,
  dist_name = "gamma",
  mort_data = mort_data,
  baseline_year = 2011,
  baseline_sample_years = 2011:2013,
  time_horizon = 2025,
  pop_size = 1e5, # 100,000 is about the min sample size needed to simulate the fine scale changes in alcohol consumption
  pop_data = stapmr::pop_counts,
  seed_sim = NULL,
  pop_seed = 5,
  iter = NULL,
  write_outputs = "output", # write outputs to the output folder within the project folder
  label = NULL
)








