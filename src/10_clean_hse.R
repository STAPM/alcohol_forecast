
# The aim of this code is to process the Health Survey for England data
# into the form required to estimate smoking transition probabilities

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
  "drinks_now", "weekmean"
)

# The variables that must have complete cases
complete_vars <- c("age", "sex", "imd_quintile", "year", "psu", "cluster", "drinks_now", "weekmean")


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
      ages = 11:89,
      years = 2011:2016,
      
      # variables to retain
      keep_vars = keep_vars,
      
      # The variables that must have complete cases
      complete_vars = complete_vars
    )
  
  return(data)
}

# Read and clean each year of data and bind them together in one big dataset
data <- combine_years(list(
  cleandata(read_2011(root = root_dir)),
  cleandata(read_2012(root = root_dir)),
  cleandata(read_2013(root = root_dir)),
  cleandata(read_2014(root = root_dir)),
  cleandata(read_2015(root = root_dir)),
  cleandata(read_2016(root = root_dir))
))

# clean the survey weights
data <- clean_surveyweights(data)

# remake age categories
data[, age_cat := c("11-15",
                    "16-17",
                    "18-24",
                    "25-34",
                    "35-44",
                    "45-54",
                    "55-64",
                    "65-74",
                    "75-89")[findInterval(age, c(-1, 16, 18, 25, 35, 45, 55, 65, 75, 1000))]]

write.table(data, "intermediate_data/HSE_2011_to_2016_alcohol.csv", row.names = FALSE, sep = ",")

