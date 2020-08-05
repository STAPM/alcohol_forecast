
# The aim of this code is to do some basic checks on the results of the simulation of the amount that drinkers drink

# Load packages
library(data.table)
library(ggplot2)

# Load data
data <- readRDS("intermediate_data/HSE_2001_to_2017_alcohol.rds")

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

# Select only years 2011 and later because these are the years with consistent data on amount drunk
data <- data[year >= 2011]

par(mfrow = c(1, 2))

hist(data[weekmean > 0 & weekmean < 100 & year == 2017, weekmean], nclass = 100, main = "observed", xlab = "units")


# Summarise data to give the proportion of drinkers in each drinker category

# Assign drinker categories
data[ , drinker_cat := NA_character_]
data[drinks_now == "non_drinker", drinker_cat := "abstainer"]
data[drinks_now == "drinker" & weekmean < 14, drinker_cat := "lower_risk"]
data[drinks_now == "drinker" & weekmean >= 14 & weekmean < 35 & sex == "Female", drinker_cat := "increasing_risk"]
data[drinks_now == "drinker" & weekmean >= 14 & weekmean < 50 & sex == "Male", drinker_cat := "increasing_risk"]
data[drinks_now == "drinker" & weekmean >= 35 & sex == "Female", drinker_cat := "higher_risk"]
data[drinks_now == "drinker" & weekmean >= 50 & sex == "Male", drinker_cat := "higher_risk"]

# Calculate proportion in each category by age, sex and IMD quintile
data_d <- data[ , .(prop_drnk = sum(wt_int, na.rm = T), .N), by = c("year", "sex", "age_cat", "imd_quintile", "drinker_cat")]
data_d[ , prop_drnk := prop_drnk / sum(prop_drnk), by = c("year", "sex", "age_cat", "imd_quintile")]
data_d[ , se_prop_drnk := sqrt(prop_drnk * (1 - prop_drnk) / N)]

data_d[ , drinker_cat := factor(drinker_cat, c("abstainer", "lower_risk", "increasing_risk", "higher_risk"))]


# Load the simulation output
simdata <- fread("output/alc_data_control_.txt")

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


hist(simdata[weekmean > 0 & weekmean < 100 & year == 2017, weekmean], nclass = 100, col = 2, main = "predicted", xlab = "units")

# Assign drinker categories
simdata[ , drinker_cat := NA_character_]
simdata[weekmean == 0, drinker_cat := "abstainer"]
simdata[weekmean > 0 & weekmean < 14, drinker_cat := "lower_risk"]
simdata[weekmean > 0 & weekmean >= 14 & weekmean < 35 & sex == "Female", drinker_cat := "increasing_risk"]
simdata[weekmean > 0 & weekmean >= 14 & weekmean < 50 & sex == "Male", drinker_cat := "increasing_risk"]
simdata[weekmean > 0 & weekmean >= 35 & sex == "Female", drinker_cat := "higher_risk"]
simdata[weekmean > 0 & weekmean >= 50 & sex == "Male", drinker_cat := "higher_risk"]

# Calculate proportion in each category by age, sex and IMD quintile
data_s <- simdata[ , .(prop_drnk = as.double(.N)), by = c("year", "sex", "age_cat", "drinker_cat", "imd_quintile")]
data_s[ , prop_drnk := prop_drnk / sum(prop_drnk), by = c("year", "sex", "age_cat", "imd_quintile")]

data_s[ , drinker_cat := factor(drinker_cat, c("abstainer", "lower_risk", "increasing_risk", "higher_risk"))]

# plot

png("output/drinker_cat_dist_simcheck_abstainers.png", units="in", width=20, height=10, res=300)
ggplot() +
  geom_point(data = data_d[drinker_cat == "abstainer"], aes(x = year, y = prop_drnk, colour = sex), size = .2, alpha = .5) +
  geom_errorbar(data = data_d[drinker_cat == "abstainer"], aes(x = year, ymin = prop_drnk - 1.96 * se_prop_drnk, ymax = prop_drnk + 1.96 * se_prop_drnk, colour = sex), size = .2, alpha = .5, width = .1) +
  geom_line(data = data_s[drinker_cat == "abstainer"], aes(x = year, y = prop_drnk, colour = sex), size = .2, alpha = .8) +
  facet_wrap(~ imd_quintile + age_cat, nrow = 5) +
  theme_minimal() + 
  ylab("proportion in drinker category") +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_colour_manual(name = "Sex", values = c("#6600cc", "#00cc99")) +
  geom_hline(yintercept = 0)
dev.off()

png("output/drinker_cat_dist_simcheck_lower_risk.png", units="in", width=20, height=10, res=300)
ggplot() +
  geom_point(data = data_d[drinker_cat == "lower_risk"], aes(x = year, y = prop_drnk, colour = sex), size = .2, alpha = .5) +
  geom_errorbar(data = data_d[drinker_cat == "lower_risk"], aes(x = year, ymin = prop_drnk - 1.96 * se_prop_drnk, ymax = prop_drnk + 1.96 * se_prop_drnk, colour = sex), size = .2, alpha = .5, width = .1) +
  geom_line(data = data_s[drinker_cat == "lower_risk"], aes(x = year, y = prop_drnk, colour = sex), size = .2, alpha = .8) +
  facet_wrap(~ imd_quintile + age_cat, nrow = 5) +
  theme_minimal() + 
  ylab("proportion in drinker category") +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_colour_manual(name = "Sex", values = c("#6600cc", "#00cc99")) +
  geom_hline(yintercept = 0)
dev.off()

png("output/drinker_cat_dist_simcheck_increasing_risk.png", units="in", width=20, height=10, res=300)
ggplot() +
  geom_point(data = data_d[drinker_cat == "increasing_risk"], aes(x = year, y = prop_drnk, colour = sex), size = .2, alpha = .5) +
  geom_errorbar(data = data_d[drinker_cat == "increasing_risk"], aes(x = year, ymin = prop_drnk - 1.96 * se_prop_drnk, ymax = prop_drnk + 1.96 * se_prop_drnk, colour = sex), size = .2, alpha = .5, width = .1) +
  geom_line(data = data_s[drinker_cat == "increasing_risk"], aes(x = year, y = prop_drnk, colour = sex), size = .2, alpha = .8) +
  facet_wrap(~ imd_quintile + age_cat, nrow = 5) +
  theme_minimal() + 
  ylab("proportion in drinker category") +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_colour_manual(name = "Sex", values = c("#6600cc", "#00cc99")) +
  geom_hline(yintercept = 0, size = .1)
dev.off()

png("output/drinker_cat_dist_simcheck_higher_risk.png", units="in", width=20, height=10, res=300)
ggplot() +
  geom_point(data = data_d[drinker_cat == "higher_risk"], aes(x = year, y = prop_drnk, colour = sex), size = .2, alpha = .5) +
  geom_errorbar(data = data_d[drinker_cat == "higher_risk"], aes(x = year, ymin = prop_drnk - 1.96 * se_prop_drnk, ymax = prop_drnk + 1.96 * se_prop_drnk, colour = sex), size = .2, alpha = .5, width = .1) +
  geom_line(data = data_s[drinker_cat == "higher_risk"], aes(x = year, y = prop_drnk, colour = sex), size = .2, alpha = .8) +
  facet_wrap(~ imd_quintile + age_cat, nrow = 5) +
  theme_minimal() + 
  ylab("proportion in drinker category") +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_colour_manual(name = "Sex", values = c("#6600cc", "#00cc99")) +
  geom_hline(yintercept = 0, size = .1)
dev.off()













