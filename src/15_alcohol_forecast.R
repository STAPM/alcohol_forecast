
library(data.table)
library(ggplot2)
library(fitdistrplus)

data <- fread("intermediate_data/HSE_2011_to_2017_alcohol.csv")

data_drinkers <- data[weekmean > 0]

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

AlcFit <- function(data, min_age, max_age, width, min_year, max_year) {
  ages <- min_age:(max_age - width + 1)
  years <- min_year:max_year
  for(y in years) {
    #y <- 2011
    out_mat <- matrix(NA, ncol = 2, nrow = length(ages))
    for(a in ages) {
      #a <- 16
      vec <- data[age >= a & age <= (a + width) & year == y, weekmean]
      fit <- fitdist(vec, distr = "weibull", method = "mle")
      out_mat[a - min_age + 1, ] <- fit$estimate
    }
    data_a <- data.frame(out_mat)
    colnames(data_a) <- c("shape", "scale")
    data_a$age <- ages + width / 2  
    data_a$year <- y
    if(y == min_year) {
      data_y <- data_a
    } else {
      data_y <- rbind(data_y, data_a)
    }
  }
  setDT(data_y)
  return(data_y[])
}

for(s in c("Male", "Female")) {
  for(imd in c("1_least_deprived", "2", "3", "4", "5_most_deprived")) {
    
    test <- AlcFit(
      data = data_drinkers[imd_quintile == imd & sex == s],
      min_age = 11,
      max_age = 89,
      width = 7,
      min_year = 2011,
      max_year = 2017
    )
    
    test <- melt(test, id.vars = c("age", "year"))
    
    test[ , sex := s]
    test[ , imd_quintile := imd]
    
    if(s == "Male" & imd == "1_least_deprived") {
      data_f <- copy(test)
    } else {
      data_f <- rbindlist(list(data_f, copy(test)), use.names = T)
    }
  }
}

ggplot(data_f[variable == "scale"]) + 
  geom_point(aes(x = age, y = value, colour = year, group = year), alpha = .4) + 
  facet_wrap(~ imd_quintile + sex, ncol = 2)


ggplot(data_f[variable == "shape"]) + 
  geom_point(aes(x = age, y = value, colour = year, group = year), alpha = .4) + 
  facet_wrap(~ imd_quintile + sex, ncol = 2) #+
  #ylim(0.3,1.3)

# Next step 
# see if svd and forecast can be done



# Select the data for one subgroup




AlcFore <- function(data, sex_i, imd_quintile_i, variable_i, min_data_year, max_data_year, time_horizon) {
  
  subdata <- copy(data[sex == sex_i & imd_quintile == imd_quintile_i & variable == variable_i])
  
  subdata[ , `:=`(sex = NULL, imd_quintile = NULL, variable = NULL)]
  
  years <- min_data_year:max_data_year
  ages <- unique(subdata$age)
  
  # Reshape to wide form with years as columns
  # then make into a matrix
  
  qdat <- dcast(subdata, age ~ year, value.var = "value")
  
  qdat[ , age := NULL]
  
  qdat <- as.matrix(qdat)
  
  # Transpose matrix data and get deaths and logrates
  # replace extreme quit probabilities with NA - then fill with approx trend
  # then logit transform
  
  qdat[qdat == 0] <- NA
  #qdat[qdat > .4] <- .4
  #qdat[qdat > 1] <- 1
  
  # smooth values in sliding a 3x3 window
  r <- raster::raster(qdat) # convert to rasterLayer
  qdat <- raster::as.matrix(raster::focal(r, matrix(1, 11, 7), mean, pad = T, padValue = NA, na.rm = T))
  
  # Fill any remaining missing values
  
  # and remove 0s and 1s from the data because they don't play well with the logit link
  
  qdat[is.na(qdat)] <- 0
  #qdat[qdat == 1] <- 0
  
  fill.zero <- function(x, method = "constant") {
    tt <- 1:length(x)
    zeros <- abs(x) < 1e-9
    xx <- x[!zeros]
    tt <- tt[!zeros]
    x <- stats::approx(tt, xx, 1:length(x), method = method, f = 0.5, rule = 2)
    return(x$y)
  }
  
  for(i in 1:nrow(qdat)) {
    qdat[i,] <- fill.zero(qdat[i,])
  }
  
  # Transpose and transform
  
  qdat <- t(qdat)
  
  qtrans <- qdat#VGAM::logitlink(qdat)
  
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
  newdata <- data.frame(years = (max_data_year+1):time_horizon)
  newdata$preds <- stats::predict(m1, newdata = newdata)
  #kval <- newdata$preds[newdata$years == 2016]
  #newdata <- newdata[newdata$years <= 2016, ]
  kt_proj <- c(kt, newdata$preds)
  
  
  # Estimate probabilities from forecast fitted values
  
  cqtransfit <- outer(kt_proj, bx_fit)
  
  qtransfit <- sweep(cqtransfit, 2, age_means, "+")
  
  fit <- qtransfit#boot::inv.logit(qtransfit)
  
  colnames(fit) <- ages
  rownames(fit) <- c(years, (max_data_year+1):time_horizon)
  
  fit <- as.data.frame(fit)
  fit$year <- c(years, (max_data_year+1):time_horizon)
  
  setDT(fit)
  
  fit <- melt(fit, id.vars = "year", variable.name = "age", value.name = "value")
  
  fit[ , age := as.numeric(as.vector(age))]
  
  fit[ , sex := as.character(sex_i)]
  fit[ , imd_quintile := as.character(imd_quintile_i)]
  fit[ , variable := as.character(variable_i)]
  
  return(fit[])
}


#data <- data_f
#sex_i <- "Male"
#imd_quintile_i <- "2"
#variable_i <- "shape"


for(v in c("shape", "scale")) {
  for(s in c("Male", "Female")) {
    for(imd in c("1_least_deprived", "2", "3", "4", "5_most_deprived")) {
      
      test <- AlcFore(data = data_f,
                      sex = s,
                      imd_quintile = imd,
                      variable = v,
                      min_data_year = 2011,
                      max_data_year = 2017,
                      time_horizon = 2025)
      
      if(s == "Male" & imd == "1_least_deprived" & v == "shape") {
        data_p <- copy(test)
      } else {
        data_p <- rbindlist(list(data_p, copy(test)), use.names = T)
      }
    }
  }
}


ggplot(data_p[variable == "scale"]) + 
  geom_point(aes(x = age, y = value, colour = year, group = year), alpha = .4) + 
  facet_wrap(~ imd_quintile + sex, ncol = 2)


ggplot(data_p[variable == "shape"]) + 
  geom_point(aes(x = age, y = value, colour = year, group = year), alpha = .4) + 
  facet_wrap(~ imd_quintile + sex, ncol = 2) #+
#ylim(0.3,1.3)

# generate distribution of alcohol consumption
data_d <- dcast(data_p, year + age + sex + imd_quintile ~ variable, value.var = "value")

mat <- t(sapply(1:nrow(data_d), function(x) { dweibull(1:300, data_d[x, shape], data_d[x, scale]) }))

df <- as.data.frame(mat)
setDT(df)

data_d <- data.table(data_d, df)

data_d[ , `:=`(shape = NULL, scale = NULL)]

data_a <- melt(data_d, id.vars = c("year", "age", "sex", "imd_quintile"))

data_a[ , variable := as.numeric(as.factor(variable))]


data_b <- data_a[ , .(av = sum(variable * value, na.rm = T) / sum(value, na.rm = T)), by = c("year", "age", "sex", "imd_quintile")]

ggplot(data_b[year <= 2025]) + 
  geom_line(aes(x = age, y = av, colour = year, group = year), alpha = .4) + 
  facet_wrap(~ imd_quintile + sex, ncol = 2) +
  xlim(11,89) + ylim(0, 30) + 
  ggtitle("Average alcohol consumption of drinkers (2011-2025)") +
  ylab("units / week")

# validate - Colin
# repeat for abstention
# work out how to use in model
# if ok, put into functions / package








