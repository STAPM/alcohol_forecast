
# The aim of this code is to do some basic checks on the simulation results

# Load packages
library(data.table)
library(ggplot2)

# Load data
data <- readRDS("intermediate_data/HSE_2001_to_2017_alcohol.rds")

# Summarise data to give proportion who drink
data[drinks_now == "non_drinker", drink_bin := 0]
data[drinks_now == "drinker", drink_bin := 1]


# Load the simulation output
simdata <- fread("output/alc_data_control_.txt")

# Calculate the period trends in alcohol consumption
# by sex and IMD quintile

simdata[weekmean == 0, drink_bin := 0]
simdata[weekmean > 0, drink_bin := 1]

######
# Summarise and plot trends in the proportion of people who drink by age category
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

# Average the smoothed and forecast data on proportion of people who drink within age categories
data_c <- data[ , .(drink_prop = mean(drink_bin)), 
                  by = c("age_cat", "year", "sex", "imd_quintile")]

simdata[, age_cat := c("8-12",
                      "13-15",
                      "16-17",
                      "18-24",
                      "25-34",
                      "35-44",
                      "45-54",
                      "55-64",
                      "65-74",
                      "75-89")[findInterval(age, c(-1, 13, 16, 18, 25, 35, 45, 55, 65, 75, 1000))]]

participation_proportions <- readRDS("intermediate_data/participation_proportions.rds")

participation_proportions[, age_cat := c("8-12",
                       "13-15",
                       "16-17",
                       "18-24",
                       "25-34",
                       "35-44",
                       "45-54",
                       "55-64",
                       "65-74",
                       "75-89")[findInterval(age, c(-1, 13, 16, 18, 25, 35, 45, 55, 65, 75, 1000))]]

data_i <- participation_proportions[ , .(drink_prop = mean(drink_prop)), 
                   by = c("age_cat", "year", "sex", "imd_quintile")]


# Average the observed data on proportion of people who drink within age categories
data_r <- simdata[ , .(drink_prop = mean(drink_bin)), 
                  by = c("age_cat", "year", "sex", "imd_quintile")]

# Order age categories
data_c[ , age_cat := factor(age_cat, c("8-12", "13-15", "16-17", "18-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75-89"))]
data_r[ , age_cat := factor(age_cat, c("8-12", "13-15", "16-17", "18-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75-89"))]
data_i[ , age_cat := factor(age_cat, c("8-12", "13-15", "16-17", "18-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75-89"))]



# Plot
png("output/prop_drinkers_simcheck.png", units="in", width=14, height=7, res=300)
ggplot() +
  geom_point(data = data_i, aes(x = year, y = drink_prop, colour = imd_quintile), size = .2, alpha = .7) +
  geom_line(data = data_i, aes(x = year, y = drink_prop, colour = imd_quintile), size = .2, alpha = .7) +
  geom_point(data = data_r, aes(x = year, y = drink_prop, colour = imd_quintile), size = .2, alpha = .3) +
  geom_line(data = data_r, aes(x = year, y = drink_prop, colour = imd_quintile), size = .2, alpha = .25) +
  #geom_line(data = data_c[year <= 2017], aes(x = year, y = drink_prop, colour = imd_quintile), linetype = 1, size = .3) +
  #geom_line(data = data_c[year > 2017], aes(x = year, y = drink_prop, colour = imd_quintile), linetype = 2, size = .3) +
  facet_wrap(~ sex + age_cat, nrow = 2) +
  geom_vline(xintercept = 2017, linetype = 1, col = "grey75") +
  theme_minimal() + 
  ylab("proportion drinkers") +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_colour_manual(name = "IMD quintile", values = c("#fcc5c0", "#fa9fb5", "#f768a1", "#c51b8a", "#7a0177")) +
  labs(title = "Proportion of people who drank in the last 12 months",
       subtitle = "Data from the Health Survey for England 2001-2017", 
       caption = "Trends smoothed with a 3 year moving average and forecast by single years of age and period before being summarised by age category")
dev.off()















