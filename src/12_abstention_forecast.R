
# The aim of this code is to summarise and forecast the 
# trends in the proportion of people who drink
# based on HSE data 2001-2017

# Load packages
library(data.table)
library(ggplot2)
library(alc.tools)

# Load data
data <- readRDS("intermediate_data/HSE_2001_to_2017_alcohol.rds")

# Summarise data to give proportion who drink
data[drinks_now == "non_drinker", drink_bin := 0]
data[drinks_now == "drinker", drink_bin := 1]

#######
# Check youth drinking trends

test <- data[ , .(drink_prop = sum(drink_bin * wt_int) / sum(wt_int)), 
              by = c("year", "age")]

ggplot() +
  geom_line(data = test[age < 16], aes(x = year, y = drink_prop), size = .2) +
  facet_wrap(~ age, nrow = 1) +
  theme_minimal() + 
  ylab("proportion drinkers") +
  theme(axis.text.x = element_text(angle = 90))
#######

# Calculate proportions of people who drink
check <- data[ , .(drink_prop = sum(drink_bin * wt_int) / sum(wt_int)), by = c("year")]
check[ , abstainers := 1 - drink_prop]

data_a <- data[ , .(drink_prop = sum(drink_bin * wt_int) / sum(wt_int)), 
                by = c("year", "age", "sex", "imd_quintile")]

# Smooth and forecast
data_f <- alc.tools::flexforecastlc(
  data = data_a,
  forecast_var = "drink_prop",
  time_horizon = 2025,
  smooth = TRUE,
  smooth_n_age = 7,
  smooth_n_year = 3,
  trans = "logit"
)

par(mfrow = c(1, 2))
hist(1 - data_a[year == 2011, drink_prop], nclass = 50, col = 2, main = "observed", xlab = "proportion", xlim = c(0,1))
hist(1 - data_f[year == 2011, drink_prop], nclass = 50, main = "predicted", xlab = "proportion", xlim = c(0,1))

1 - mean(data_a[year == 2011, drink_prop])
1 - mean(data_f[year == 2011, drink_prop])

1 - median(data_a[year == 2011, drink_prop])
1 - median(data_f[year == 2011, drink_prop])

# Save proportion estimates for use in simulation
saveRDS(data_f, "intermediate_data/participation_proportions.rds")


######
# Summarise and plot trends in the proportion of people who drink by age category
data_f[, age_cat := c("8-12",
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
data_c <- data_f[ , .(drink_prop = mean(drink_prop)), 
                  by = c("age_cat", "year", "sex", "imd_quintile")]

data_a[, age_cat := c("8-12",
                      "13-15",
                      "16-17",
                      "18-24",
                      "25-34",
                      "35-44",
                      "45-54",
                      "55-64",
                      "65-74",
                      "75-89")[findInterval(age, c(-1, 13, 16, 18, 25, 35, 45, 55, 65, 75, 1000))]]

# Average the observed data on proportion of people who drink within age categories
data_r <- data_a[ , .(drink_prop = mean(drink_prop)), 
                  by = c("age_cat", "year", "sex", "imd_quintile")]

# Order age categories
data_c[ , age_cat := factor(age_cat, c("8-12", "13-15", "16-17", "18-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75-89"))]
data_r[ , age_cat := factor(age_cat, c("8-12", "13-15", "16-17", "18-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75-89"))]

# Plot
png("output/prop_drinkers.png", units="in", width=14, height=7, res=300)
ggplot() +
  geom_point(data = data_r, aes(x = year, y = drink_prop, colour = imd_quintile), size = .2, alpha = .3) +
  geom_line(data = data_r, aes(x = year, y = drink_prop, colour = imd_quintile), size = .2, alpha = .25) +
  geom_line(data = data_c[year <= 2017], aes(x = year, y = drink_prop, colour = imd_quintile), linetype = 1, size = .3) +
  geom_line(data = data_c[year > 2017], aes(x = year, y = drink_prop, colour = imd_quintile), linetype = 2, size = .3) +
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

######
# Summarise cohort age trends

# period
png("output/prop_drinkers_period.png", units="in", width=14, height=7, res=300)
ggplot() +
  geom_line(data = data_f, aes(x = age, y = drink_prop, colour = year, group = year), alpha = .4, size = .2) +
  geom_line(data = data_f[year == 2017], aes(x = age, y = drink_prop), col = "red", size = .2) +
  facet_wrap(~ sex + imd_quintile, nrow = 2) +
  theme_minimal() + 
  ylab("proportion drinkers") +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Proportion of people who drank in the last 12 months",
       subtitle = "Data from the Health Survey for England 2001-2017", 
       caption = "Trends smoothed with a 3 year moving average and forecast by single years of age and period before being plotted by period")
dev.off()

# cohort

data_f[ , cohort := year - age]

png("output/prop_drinkers_cohort.png", units="in", width=14, height=7, res=300)
ggplot() +
  geom_line(data = data_f[cohort %in% seq(1950, 2020, 10)], aes(x = age, y = drink_prop, colour = cohort, group = cohort), size = .2) +
  facet_wrap(~ sex + imd_quintile, nrow = 2) +
  theme_minimal() + 
  ylab("proportion drinkers") +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Proportion of people who drank in the last 12 months",
       subtitle = "Data from the Health Survey for England 2001-2017", 
       caption = "Trends smoothed with a 3 year moving average and forecast by single years of age and period before being plotted by birth cohort")
dev.off()

















