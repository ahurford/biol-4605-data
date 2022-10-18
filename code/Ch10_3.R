# Title: Chapter 10.3
# Author: Amy Hurford (ahurford@mun.ca)
# Date: Oct 14, 2022
# ------------

# Load data from website
data <- read.csv('https://raw.githubusercontent.com/ahurford/biol-4605-data/main/data/pea_length.csv', fill=TRUE)

# Plot the data
boxplot(length~treatment, data = data)

# Run the t-test (note a 1-way ANOVA with 2 groups is a t-test)
Peamodel <- aov(length~treatment, data = data)

# GLM table
Peamodel

# ANOVA table
anova(Peamodel)

# Checking error
plot(Peamodel)


data2 <- data
data2$treatment[data2$treatment == "Sucrose"] = "Sugar"
data2$treatment[data2$treatment == "Fructose"] = "Sugar"
data2$treatment[data2$treatment == "Glucose"] = "Sugar"
data2$treatment[data2$treatment == "Gluc.Fruc"] = "Sugar"

boxplot(length~treatment, data = data2)
SugarModel <- aov(length~treatment, data = data2)
anova(SugarModel)

# Install the ggplot2 packaage and try this!
# All the data points are shown rather than just quartile stats
# shown with box plots
# The violin plot shows the density of data points for values of
# the explantory variable
require(ggplot2)
violin.plot <- ggplot(data, aes(x=treatment, y=length)) + 
  geom_violin() +
  geom_boxplot(width = 0.2)+
  geom_jitter(position=position_jitter(0.2))
violin.plot

violin.plot2 <- ggplot(data2, aes(x=treatment, y=length)) + 
  geom_violin() +
  geom_boxplot(width = 0.2)+
  geom_jitter(position=position_jitter(0.2))
violin.plot2
