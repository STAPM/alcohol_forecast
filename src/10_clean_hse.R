
# The aim of this code is to process the Health Survey for England data
# into the form required to estimate drinking trends

# Load the required packages
library(hseclean)
library(magrittr)
library(data.table)

# Set the file path to point to the University of Sheffield X drive
root_dir <- "/Volumes/Shared/"

# apply functions to create the variables for analysis and to retain only the required variables

# The variables to retain
keep_vars = c(
  # Survey design variables
  "wt_int",
  "psu",
  "cluster",
  "year",
  
  # Social / economic / demographic variables
  "age",
  "age_cat",
  "sex",
  "imd_quintile",
  
  # Drinking
  "drinks_now", "drink_freq_7d", "weekmean", "total_units7_ch"
)

# The variables that must have complete cases
complete_vars <- c("age", "sex", "imd_quintile", "year", "psu", "cluster")


#-----------------------------------------------------
# Read and clean the data

cleandata <- function(data) {
  
  data %<>%
    clean_age %>%
    clean_demographic %>% 
    clean_education %>%
    clean_economic_status %>%
    clean_family %>%
    clean_income %>%
    clean_health_and_bio %>%
    alc_drink_now_allages %>%
    alc_weekmean_adult %>%
    alc_sevenday_adult %>%
    alc_sevenday_child %>%
    
    select_data(
      ages = 13:89,
      years = 2001:2017,
      
      # variables to retain
      keep_vars = keep_vars,
      
      # The variables that must have complete cases
      complete_vars = complete_vars
    )
  
  return(data)
}

# Read and clean each year of data and bind them together in one big dataset
data <- combine_years(list(
  cleandata(read_2001(root = root_dir)),
  cleandata(read_2002(root = root_dir)),
  cleandata(read_2003(root = root_dir)),
  cleandata(read_2004(root = root_dir)),
  cleandata(read_2005(root = root_dir)),
  cleandata(read_2006(root = root_dir)),
  cleandata(read_2007(root = root_dir)),
  cleandata(read_2008(root = root_dir)),
  cleandata(read_2009(root = root_dir)),
  cleandata(read_2010(root = root_dir)),
  cleandata(read_2011(root = root_dir)),
  cleandata(read_2012(root = root_dir)),
  cleandata(read_2013(root = root_dir)),
  cleandata(read_2014(root = root_dir)),
  cleandata(read_2015(root = root_dir)),
  cleandata(read_2016(root = root_dir)),
  cleandata(read_2017(root = root_dir))
))

# clean the survey weights
data <- clean_surveyweights(data, pop_data = stapmr::pop_counts)

# remake age categories
data[, age_cat := c("8-12",
                    "13-15",
                    "16-17",
                    "18-24",
                    "25-34",
                    "35-44",
                    "45-54",
                    "55-64",
                    "65-74",
                    "75-89")[findInterval(age, c(-1, 13, 16, 18, 25, 35, 45, 55, 65, 75, 1000))]]

data <- data[!is.na(drinks_now)]

# Impute missing values of weekmean
data <- alc_impute(data)

# still an issue to check with alc_impute - leaving a few NAs for drinkers in years >= 2011
# remove them for now
data <- data[!(year >= 2011 & drinks_now == "drinker" & is.na(weekmean))]


#data <- readRDS("intermediate_data/HSE_2001_to_2017_alcohol.rds")

# clean the survey weights
data <- clean_surveyweights(data, pop_data = stapmr::pop_counts)

# Save data
saveRDS(data, "intermediate_data/HSE_2001_to_2017_alcohol.rds")


