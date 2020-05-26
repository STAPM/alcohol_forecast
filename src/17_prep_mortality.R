
# The aim of this code is to prepare the mortality data 
# for use in the simulation model

library(data.table)
#library(mort.tools)
library(readxl)
library(ggplot2)
library(magrittr)

# Load the mortality data
# This is stored on the university's X drive 
# after having been processed into an aggregated form on the secure heta_study virtual machine

# Point to the location of the X drive
#root_dir <- "X:/"
root_dir <- "/Volumes/Shared/"

# Load the processed mortality data
alc_mort_data_cause <- fread(paste0(root_dir,
      "ScHARR/PR_Mortality_data_TA/Code/model_inputs/Output/alc_death_rates_national_2019-03-29_mort.tools_1.0.0.csv"))

# Filter data
alc_mort_data_cause <- alc_mort_data_cause[age %in% 13:89 & !is.na(cause) , c("age",
                                                                  "sex",
                                                                  "imd_quintile",
                                                                  "year",
                                                                  "cause",
                                                                  "mx_cause"), with = F]

# Rather than conduct a forecast of cause-specific mortality rates,
# assume that mortality rates for the latest year available (2016)
# remain constant into the future

# Duplicate these data for years up to a 2030 time horizon

data2016 <- alc_mort_data_cause[year == 2016]

for(y in 2017:2030) {
  
  alc_mort_data_cause <- rbindlist(list(alc_mort_data_cause, copy(data2016)[, year := y]), use.names = T)
  
}

# Change variable names
setnames(alc_mort_data_cause, c("cause", "mx_cause"), c("condition", "mix"))

# Save the data for use in the simulation model
saveRDS(alc_mort_data_cause, "intermediate_data/alc_mort_data_cause.rds")

rm(alc_mort_data_cause)
gc()











