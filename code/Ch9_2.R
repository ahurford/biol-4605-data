# Title: Ch. 9.2 Linear regression explanatory variable with classes
# Author: Amy Hurford (ahurford@mun.ca)
# Date: Oct 12, 2022
# =======

# Here we use the evidentalist inference: We decide testing the null
# hypothesis for these data is not appropriate. The relation is
# obvious from the graph; the null hypothesis is of no interest.
# A more interesting (and plausible) model is a 1:1 relation of
# heights of sons to fathers
                          
# This imports the data into R from a website without needing to download
# Heights of father's versus sons
data <- read.csv('https://raw.githubusercontent.com/ahurford/biol-4605-data/main/data/height.csv', fill=TRUE)

# Give the variables shorter names
# Response variable
Hson = data$Hson
# Explanatory variable
Hfather = data$Hfather
# Number of families (determines weightings in linear regression)
Nfamily = data$Nfamily

# Plot the data. Does there look like there is a relationship?
# The size of the points is given by Nfamily. I divided by 100
# to scale down the size of the points to something sensible!
plot(Hfather, Hson, cex = Nfamily/100, pch=19)

# Data equations for the regression model

# I need the fitted coefficients for the regression model
# Do the regression
reg <- lm(Hson~Hfather,weights=Nfamily)
# see the results of your regression
summary(reg)

# ANOVA table - compare with 4. in the notes, i.e. check
# partitioning of df and SS
anova(reg)

# This is a plot of the regression (line)
abline(reg)

# A series of plots to check model assumptions.
plot(reg)

# Likelihood ratio
n = length(Hson)
SS.res = anova(reg)$`Sum Sq`[2]
SS.model = anova(reg)$`Sum Sq`[1]
SS.total = SS.res + SS.model
LR = (SS.res/SS.total)^(-n/2)

#######
# In Ch. 9.2 notes:
# 10. State evidential support. Report effects sizes.
# Interpret parameters of biological
# interest. 

# Run the below command to check for the slope coefficient and
# standard error. Check against notes in sected 
summary(reg)

# The confidence intervals formulas are stated as in the notes.
# The t-value
# can be recovered from R as:
tval = qt(0.975,n-1)

beta.Hfather = coef(summary(reg))[2,1]
Std.Error = coef(summary(reg))[2,2]
lower.CI = beta.Hfather - tval*Std.Error
upper.CI = beta.Hfather + tval*Std.Error
