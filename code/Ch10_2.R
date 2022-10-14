# Title: Chapter 10.2: Two sample t-test
# Author: Amy Hurford (ahurford@mun.ca)
# Date: Oct 14, 2022
# ------------

# Load data from website
data <- read.csv('https://raw.githubusercontent.com/ahurford/biol-4605-data/main/data/extrasleep.csv', fill=TRUE)

# Should check what your data look like
# data

# Plot the data
boxplot(extra.sleep~drug, data = data)

# Run the t-test (note a 1-way ANOVA with 2 groups is a t-test)
Sleepmodel <- aov(extra.sleep~drug, data = data)

# GLM table
Sleepmodel

# ANOVA table
anova(Sleepmodel)

# Checking error
plot(Sleepmodel)

### Advanced plotting
# We can now to plots much much advanced than boxplots
# Install the ggplot2 packaage and try this!
# All the data points are shown rather than just quartile stats
# shown with box plots
# The violin plot shows the density of data points for values of
# the explantory variable
require(ggplot2)
violin.plot <- ggplot(data, aes(x=drug, y=extra.sleep)) + 
  geom_violin() +
  geom_boxplot(width = 0.2)+
  geom_jitter(position=position_jitter(0.2))
violin.plot
