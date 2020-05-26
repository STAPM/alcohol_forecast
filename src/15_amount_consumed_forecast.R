
# The aim of this code is to forecast the distribution of 
# the amount of alcohol consumed by people who drink
# using HSE data from 2011-2017

# Load packages
library(data.table)
library(ggplot2)
library(alc.tools)

# Load data
data <- readRDS("intermediate_data/HSE_2001_to_2017_alcohol.rds")

# Filter data to retain people with non-zero average weekly amount drunk
data_drinkers <- data[weekmean > 0 & year >= 2011]

#table(data_drinkers$age)
# youngest age for which amount drunk is recorded is 13 years

######
# Exploring fit of curves to data

#fit.gamma <- fitdist(data_drinkers$weekmean, distr = "gamma", method = "mle")
#summary(fit.gamma)
#plot(fit.gamma)

#fit.weibull <- fitdist(data_drinkers$weekmean, distr = "weibull", method = "mle")
#summary(fit.weibull)
#plot(fit.weibull)

#hist(data_drinkers$weekmean)
#hist(rweibull(nrow(data_drinkers), shape = 0.6749525, scale = 10.5582057), add = T, col = 2)


#test <- data_drinkers[imd_quintile == "2" & sex == "Male" & year == 2011 & age >= 16 & age < 18, weekmean]
#fit.weibull <- fitdist(test, distr = "weibull", method = "mle")
#summary(fit.weibull)
#plot(fit.weibull)

#hist(rweibull(nrow(data_drinkers), shape = 0.7540469, scale = 19.0939469), add = T, col = 3)

######
# Estimate the shape and scale parameters of the distribution

param_data <- alc.tools::distfit(
  data = data_drinkers,
  var_name = "weekmean",
  width_age = 5,
  width_year = 3,
  dist_name = "weibull"
)

# plot each parameter

# Scale
ggplot(param_data) + 
  geom_line(aes(x = age, y = scale, colour = year, group = year), alpha = .4, size = .3) + 
  facet_wrap(~ imd_quintile + sex, ncol = 2)

# Shape
ggplot(param_data) + 
  geom_line(aes(x = age, y = shape, colour = year, group = year), alpha = .4, size = .3) + 
  facet_wrap(~ imd_quintile + sex, ncol = 2)

######
# Forecast each parameter

# Check parameter distributions
#hist(param_data$shape, nclass = 100)
#hist(param_data$scale, nclass = 100)

# Smooth and forecast

# Scale
data_f_scale <- alc.tools::flexforecastlc(
  data = param_data,
  forecast_var = "scale",
  time_horizon = 2025,
  smooth = TRUE,
  smooth_n_age = 5,
  smooth_n_year = 3,
  family = "gaussian"
)

ggplot(data_f_scale) + 
  geom_line(aes(x = age, y = scale, colour = year, group = year), alpha = .4, size = .4) + 
  facet_wrap(~ imd_quintile + sex, ncol = 2)

# Shape
data_f_shape <- alc.tools::flexforecastlc(
  data = param_data,
  forecast_var = "shape",
  time_horizon = 2025,
  smooth = TRUE,
  smooth_n_age = 5,
  smooth_n_year = 3,
  family = "gaussian"
)

ggplot(data_f_shape) + 
  geom_line(aes(x = age, y = shape, colour = year, group = year), alpha = .4, size = .4) + 
  facet_wrap(~ imd_quintile + sex, ncol = 2)

# Merge data
data_f <- merge(data_f_scale, data_f_shape, all = T, by = c("age", "sex", "year", "imd_quintile"))

# Save parameter estimates for use in simulation
saveRDS(data_f, "intermediate_data/consumption_params.rds")


######
# Regenerate distributions and plot

# For 1 to 300 units consumed per week
# generate the corresponding probability density from the shape and scale parameters
mat <- t(sapply(1:nrow(data_f), function(x) {
    dweibull(1:300, data_f[x, shape], data_f[x, scale])
  }))

# Tidy
df <- as.data.frame(mat)
setDT(df)
data_d <- data.table(data_f, df)
data_d[ , `:=`(shape = NULL, scale = NULL)]

# Reshape to long form and tidy
data_a <- melt(data_d, id.vars = c("year", "age", "sex", "imd_quintile"))
data_a[ , variable := as.numeric(as.factor(variable))]

# Calculate the cumulative distribution - easier to see patterns
data_a[ , cum_dist := cumsum(value), by = c("year", "age", "sex", "imd_quintile")]

# Plot the change to the distribution for a certain age
ggplot(data_a[age == 18]) +
  geom_line(aes(x = variable, y = cum_dist, colour = year, group = year), alpha = .4, size = .4) +
  facet_wrap(~ imd_quintile + sex, ncol = 2)

# Calculate the average amount consumed from the distributions
data_b <- data_a[ , .(av = sum(variable * value, na.rm = T) / sum(value, na.rm = T)), by = c("year", "age", "sex", "imd_quintile")]

png("output/amount_consumed.png", units="in", width=7, height=14, res=300)
ggplot(data_b) + 
  geom_line(aes(x = age, y = av, colour = year, group = year), alpha = .4, size = .4) + 
  geom_line(data = data_b[year == 2017], aes(x = age, y = av), colour = "red", size = .3) +
  facet_wrap(~ imd_quintile + sex, ncol = 2) +
  ylab("units / week") +
  theme_minimal() +
  labs(title = "Average alcohol consumption of drinkers",
       subtitle = "Data from the Health Survey for England 2011-2017", 
       caption = "Distribution of consumption summarised to the shape and scale parameters 
       of a Weilbull distribution each parameter was forecast and then the distribution of 
       consumption was reconsituted and summarised.")
dev.off()









