
# The aim of this code is to forecast the distribution of 
# the amount of alcohol consumed by people who drink
# using HSE data from 2011-2017

# Load packages
library(data.table)
library(ggplot2)
library(alc.tools)
library(fitdistrplus)

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

vec <- data_drinkers[ , weekmean]

fit <- fitdistrplus::fitdist(vec, distr = "gamma", method = "mge", gof = "CvM")

fit <- fitdistrplus::fitdist(vec, distr = "gamma", method = "mge", gof = "CvM", start = list(shape = 0.53754229, rate = 0.04191911))

# Check prediction of proportion of higher risk drinkers

# observed
length(vec[which(vec >= 50)]) / length(vec)

# predicted
1 - pgamma(50, shape = fit$estimate[1], rate = fit$estimate[2])

# Check prediction of proportion of increasing risk drinkers

# observed
length(vec[which(vec >= 14 & vec < 50)]) / length(vec)

# predicted
pgamma(50, shape = fit$estimate[1], rate = fit$estimate[2]) - pgamma(14, shape = fit$estimate[1], rate = fit$estimate[2])

# Check prediction of proportion of increasing risk drinkers

# observed
length(vec[which(vec < 14)]) / length(vec)

# predicted
pgamma(14, shape = fit$estimate[1], rate = fit$estimate[2])



######
# Estimate the shape and scale parameters of the distribution

param_data <- alc.tools::distfit(
  data = data_drinkers,
  var_name = "weekmean",
  width_age = 7,
  width_year = 3,
  min_points = 10,
  dist_name = "gamma",
  start = list(shape = 0.53754229, rate = 0.04191911)
)

# plot each parameter

# Shape
ggplot(param_data) + 
  geom_line(aes(x = age, y = p1, colour = year, group = year), alpha = .4, size = .3) + 
  facet_wrap(~ imd_quintile + sex, ncol = 2)

# Scale or rate
ggplot(param_data) + 
  geom_line(aes(x = age, y = p2, colour = year, group = year), alpha = .4, size = .3) + 
  facet_wrap(~ imd_quintile + sex, ncol = 2)


###########
# Predict the proportions of higher risk drinkers

param_data[sex == "Male", higher_risk_threshold := 50]
param_data[sex == "Female", higher_risk_threshold := 35]

#param_data[ , p_higher_risk := 1 - pweibull(higher_risk_threshold, shape = p1, scale = p2)]
param_data[ , p_higher_risk := 1 - pgamma(higher_risk_threshold, shape = p1, rate = p2)]

param_data[ , age_cat := c("13-15", "16-17", "18-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75-89")[findInterval(age, c(-1, 16, 18, 25, 35, 45, 55, 65, 75))]]       

data_p <- param_data[ , .(p_higher_risk = mean(p_higher_risk)), by = c("year", "sex", "imd_quintile", "age_cat")]

# Compare against the observed proportions
data_drinkers[ , higher_risk := 0]
data_drinkers[sex == "Male" & weekmean >= 50, higher_risk := 1]
data_drinkers[sex == "Female" & weekmean >= 35, higher_risk := 1]

data_hr <- data_drinkers[ , .(p_higher_risk_obs = mean(higher_risk, na.rm = T)), by = c("age_cat", "sex", "imd_quintile", "year")]

data_p <- merge(data_p, data_hr, all.x = T, all.y = F, by = c("age_cat", "sex", "imd_quintile", "year"))

ggplot(data_p) +
  geom_point(aes(y = p_higher_risk, x = p_higher_risk_obs)) + 
  xlim(0,.5) + ylim(0,.5) +
  geom_abline(slope = 1, intercept = 0)

###########
# Predict the proportions of increasing risk drinkers

param_data[ , increasing_risk_threshold := 14]

#param_data[ , p_increasing_risk := pweibull(higher_risk_threshold, shape = p1, scale = p2) - pweibull(increasing_risk_threshold, shape = p1, scale = p2)]
param_data[ , p_increasing_risk := pgamma(higher_risk_threshold, shape = p1, rate = p2) - pgamma(increasing_risk_threshold, shape = p1, rate = p2)]

data_p <- param_data[ , .(p_increasing_risk = mean(p_increasing_risk)), by = c("year", "sex", "imd_quintile", "age_cat")]

# Compare against the observed proportions
data_drinkers[ , increasing_risk := 0]
data_drinkers[sex == "Male" & weekmean >= 14 & weekmean < 50, increasing_risk := 1]
data_drinkers[sex == "Female" & weekmean >= 14 & weekmean < 35, increasing_risk := 1]

data_ir <- data_drinkers[ , .(p_increasing_risk_obs = mean(increasing_risk, na.rm = T)), by = c("age_cat", "sex", "imd_quintile", "year")]

data_p <- merge(data_p, data_ir, all.x = T, all.y = F, by = c("age_cat", "sex", "imd_quintile", "year"))

ggplot(data_p) +
  geom_point(aes(y = p_increasing_risk, x = p_increasing_risk_obs)) + 
  xlim(0,1) + ylim(0,1) +
  geom_abline(slope = 1, intercept = 0)

###########
# Predict the proportions of lower risk drinkers

param_data[ , p_lower_risk := pgamma(increasing_risk_threshold, shape = p1, rate = p2)]

data_p <- param_data[ , .(p_lower_risk = mean(p_lower_risk)), by = c("year", "sex", "imd_quintile", "age_cat")]

# Compare against the observed proportions
data_drinkers[ , lower_risk := 0]
data_drinkers[weekmean < 14, lower_risk := 1]

data_ir <- data_drinkers[ , .(p_lower_risk_obs = mean(lower_risk, na.rm = T)), by = c("age_cat", "sex", "imd_quintile", "year")]

data_p <- merge(data_p, data_ir, all.x = T, all.y = F, by = c("age_cat", "sex", "imd_quintile", "year"))

ggplot(data_p) +
  geom_point(aes(y = p_lower_risk, x = p_lower_risk_obs)) + 
  xlim(0,1) + ylim(0,1) +
  geom_abline(slope = 1, intercept = 0)

######
# Forecast each parameter

# Check parameter distributions
#hist(param_data$shape, nclass = 100)
#hist(param_data$scale, nclass = 100)

# Smooth and forecast

# Shape

# Look at the distribution
hist(param_data$p1, nclass = 100)

# Truncate the extreme values
p1_q99 <- quantile(param_data$p1, .99, na.rm = T)
param_data[p1 > p1_q99, p1 := p1_q99]

# Smooth and forecast the estimates
data_f_p1 <- alc.tools::flexforecastlc(
  data = param_data,
  forecast_var = "p1",
  time_horizon = 2025,
  smooth = TRUE,
  smooth_n_age = 7,
  smooth_n_year = 3,
  trans = "gaussian"
)

par(mfrow = c(1, 2))
hist(param_data$p1, nclass = 100, main = "observed")
hist(data_f_p1$p1, nclass = 100, main = "predicted")

ggplot(data_f_p1) + 
  geom_line(aes(x = age, y = p1, colour = year, group = year), alpha = .4, size = .4) + 
  facet_wrap(~ imd_quintile + sex, ncol = 2)


# Scale or rate

# Look at the distribution
hist(param_data$p2, nclass = 100)

# Truncate the extreme values
p2_q99 <- quantile(param_data$p2, .99, na.rm = T)
param_data[p2 > p2_q99, p2 := p2_q99]

hist(param_data$p2, nclass = 100)

data_f_p2 <- alc.tools::flexforecastlc(
  data = param_data,
  forecast_var = "p2",
  time_horizon = 2025,
  smooth = TRUE,
  smooth_n_age = 7,
  smooth_n_year = 3,
  trans = "log"
)

par(mfrow = c(1, 2))
hist(param_data$p2, nclass = 100, main = "observed")
hist(data_f_p2$p2, nclass = 100, main = "predicted")



ggplot(data_f_p2) + 
  geom_line(aes(x = age, y = p2, colour = year, group = year), alpha = .4, size = .4) + 
  facet_wrap(~ imd_quintile + sex, ncol = 2)



# Merge data
data_f <- merge(data_f_p1, data_f_p2, all = T, by = c("age", "sex", "year", "imd_quintile"))

# Save parameter estimates for use in simulation
saveRDS(data_f, "intermediate_data/consumption_params.rds")


######
# Regenerate distributions and plot

# For 1 to 300 units consumed per week
# generate the corresponding probability density from the parameters

# Weibull 
#mat <- t(sapply(1:nrow(data_f), function(x) {
#    dweibull(x = 1:300, shape = data_f[x, p1], scale = data_f[x, p2])
#  }))

# Gamma
mat <- t(sapply(1:nrow(data_f), function(x) {
  dgamma(x = 1:300, shape = data_f[x, p1], rate = data_f[x, p2])
}))



# Tidy
df <- as.data.frame(mat)
setDT(df)
data_d <- data.table(data_f, df)
data_d[ , `:=`(p1 = NULL, p2 = NULL)]

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
       caption = "Distribution of consumption summarised to the shape and rate parameters 
       of a Gamma distribution each parameter was forecast and then the distribution of 
       consumption was reconsituted and summarised.")
dev.off()









