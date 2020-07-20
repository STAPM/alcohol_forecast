
# The aim of this code is to do some basic checks on the simulation results

library(data.table)
library(ggplot2)

# Load the simulation output
data <- fread("output/alc_data_control_.txt")

# Calculate the period trends in alcohol consumption
# by sex and IMD quintile
data1 <- data[ , .(weekmean = mean(weekmean, na.rm = T)), by = c("sex", "imd_quintile", "year")]

# Plot the trends
p1 <- ggplot(data1) +
  geom_line(aes(x = year, y = weekmean, colour = imd_quintile)) +
  facet_wrap(~ sex) +
  scale_colour_manual(name = "IMD quintile", values = c("#fcc5c0", "#fa9fb5", "#f768a1", "#c51b8a", "#7a0177")) +
  theme_minimal()

png("output/alc_trends_check.png", units="in", width=14, height=7, res=300)
  p1
dev.off()

