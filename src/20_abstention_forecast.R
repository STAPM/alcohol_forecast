
library(data.table)
library(ggplot2)

# Load data
data <- fread("intermediate_data/HSE_2001_to_2017_alcohol.csv")

# Summarise data to give proportion who drink
data[drinks_now == "non_drinker", drink_bin := 0]
data[drinks_now == "drinker", drink_bin := 1]

data_a <- data[ , .(drink_prop = sum(drink_bin * wt_int) / sum(wt_int)), by = c("year", "age", "sex", "imd_quintile")]

flexforecastlc <- function(
  data,
  forecast_var,
  time_horizon = 2030,
  smooth_window_size = 7
) {
  
  # The ages and years
  ages <- min(data$age):max(data$age)
  years <- min(data$year):max(data$year)
  proj_years <- (max(data$year) + 1):time_horizon
  
  n <- length(ages)
  m <- length(years)
  
  data <- copy(data[age %in% ages])
  
  # Loop through subgroups
  
  counter <- 1
  
  for(sex_i in c("Male", "Female")) {
    
    #sex_i <- "Male"
    #cat(sex_i, "\n")
    
    for(imd_quintile_i in c("1_least_deprived", "2", "3", "4", "5_most_deprived")) {
      
      #imd_quintile_i <- "2"
      #cat(imd_quintile_i, "\n")
      
      # Select the data for one subgroup
      
      subdata <- copy(data[sex == sex_i & imd_quintile == imd_quintile_i])
      
      subdata[ , `:=`(sex = NULL, imd_quintile = NULL)]
      
      # Reshape to wide form with years as columns
      # then make into a matrix
      
      qdat <- dcast(subdata, age ~ year, value.var = forecast_var)
      
      qdat[ , age := NULL]
      
      qdat <- as.matrix(qdat)
      
      # Transpose matrix data and get deaths and logrates
      # replace extreme quit probabilities with NA - then fill with approx trend
      # then logit transform
      
      qdat[qdat <= 0] <- NA
      qdat[qdat >= 1] <- NA
      
      # smooth values in sliding a window
      r <- raster::raster(qdat) # convert to rasterLayer
      qdat <- raster::as.matrix(raster::focal(r, matrix(1, smooth_window_size, smooth_window_size), mean, pad = T, padValue = NA, na.rm = T))
      
      # Fill any remaining missing values
      
      # and remove 0s and 1s from the data because they don't play well with the logit link
      qdat[is.na(qdat)] <- 0
      
      fill.zero <- function(x, method = "constant") {
        tt <- 1:length(x)
        zeros <- abs(x) < 1e-9
        xx <- x[!zeros]
        tt <- tt[!zeros]
        x <- stats::approx(tt, xx, 1:length(x), method = method, f = 0.5, rule = 2)
        return(x$y)
      }
      
      for(i in 1:n) {
        qdat[i,] <- fill.zero(qdat[i,])
      }
      
      # Transpose and transform
      
      qdat <- t(qdat)
      
      qtrans <- VGAM::logitlink(qdat)
      
      # Do SVD
      age_means <- apply(qtrans, 2, mean, na.rm = TRUE) # age_means is mean of qtrans by column
      cqtrans <- sweep(qtrans, 2 , age_means) # logit quit probs (with age_means subtracted) (dimensions m*n)
      svd_qdat <- svd(cqtrans)
      
      # Extract first principal component
      sumv <- sum(svd_qdat$v[ , 1])
      kt <- svd_qdat$d[1] * svd_qdat$u[ , 1] * sumv
      
      bx <- svd_qdat$v[ , 1] / sumv
      
      bx_fit <- stats::predict(stats::lm(bx ~ I(1:length(bx))))
      bx_fit <- bx_fit / sum(bx_fit)
      
      # Fit a linear model through the trend
      kt_data <- data.frame(kt, years)
      m1 <- stats::lm(kt ~ years, data = kt_data)
      newdata <- data.frame(years = proj_years)
      newdata$preds <- stats::predict(m1, newdata = newdata)
      kt_proj <- c(kt, newdata$preds)
      
      # Estimate probabilities from forecast fitted values
      
      cqtransfit <- outer(kt_proj, bx_fit)
      
      qtransfit <- sweep(cqtransfit, 2, age_means, "+")
      
      fit <- boot::inv.logit(qtransfit)
      
      colnames(fit) <- ages
      rownames(fit) <- c(years, proj_years)
      
      fit <- as.data.frame(fit)
      fit$year <- c(years, proj_years)
      
      setDT(fit)
      
      fit <- melt(fit, id.vars = "year", variable.name = "age", value.name = forecast_var)
      
      fit[ , age := as.numeric(as.vector(age))]
      
      fit[ , sex := as.character(sex_i)]
      fit[ , imd_quintile := as.character(imd_quintile_i)]
      
      if(counter == 1) {
        data_proj <- copy(fit)
      } else {
        data_proj <- rbindlist(list(data_proj, copy(fit)), use.names = T)
      }
      
      counter <- counter + 1
    }
  }
  
  data_proj[get(forecast_var) < 0, (forecast_var) := 0]
  data_proj[get(forecast_var) > 1, (forecast_var) := 1]
  
  
  return(data_proj[])
}


data_f <- flexforecastlc(
  data = data_a,
  forecast_var = "drink_prop",
  time_horizon = 2030,
  smooth_window_size = 7
)

data_f[, age_cat := c("11-15",
                    "16-17",
                    "18-24",
                    "25-34",
                    "35-44",
                    "45-54",
                    "55-64",
                    "65-74",
                    "75-89")[findInterval(age, c(-1, 16, 18, 25, 35, 45, 55, 65, 75, 1000))]]

data_c <- data_f[ , .(drink_prop = mean(drink_prop)), by = c("age_cat", "year", "sex", "imd_quintile")]

ggplot() +
  geom_line(data = data_c[year <= 2017], aes(x = year, y = drink_prop, colour = imd_quintile)) +
  geom_line(data = data_c[year > 2017], aes(x = year, y = drink_prop, colour = imd_quintile), linetype = 2) +
  facet_wrap(~ sex + age_cat, nrow = 2) +
  geom_vline(xintercept = 2017, linetype = 1, col = "grey75") +
  theme_minimal() + 
  ylab("proportion drinkers") +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Proportion of people who drink (Health Survey for England 2001-2017, forecast to 2030)") +
  scale_colour_manual(name = "IMD quintile", values = c("#fcc5c0", "#fa9fb5", "#f768a1", "#c51b8a", "#7a0177"))







