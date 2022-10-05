# Title: Chapter 9.1. Regression explanatory variable fixed by experiment
# Details: R script based on https://github.com/DavidCSchneider/StatisticalScience/blob/main/Data/Ch09.xls
# Author: Amy Hurford (ahurford@mun.ca)
# Date: 05-10-2022
#--------------------

# This imports the data into R from a website without needing to download
data <- read.csv('https://raw.githubusercontent.com/ahurford/biol-4605-data/main/data/corn.csv', fill=TRUE)

# Give the variables shorter names
# Response variable
Pcorn = data$Pcorn
# Explanatory variable
Psoil = data$Psoil

# Plot the data. Does there look like there is a relationship?
plot(Psoil, Pcorn)

# Data equations for the null model
data.eq_null = data.frame(Psoil = Psoil, Data = Pcorn, Model = rep(mean(Pcorn),length(Pcorn)), res = Pcorn-mean(Pcorn), res2 = (Pcorn-mean(Pcorn))^2)
sum(data.eq_null$res)
SS.total = sum(data.eq_null$res2)

# Data equations for the regression model

# I need the fitted coefficients for the regression model
# Do the regression
reg <- lm(Pcorn~Psoil)
# see the results of your regression
summary(reg)

# Rename the estimated coefficients in the language of the assignment
alpha <- unname(coef(reg)[1])
beta <- unname(coef(reg)[2])

# Use the data equation to find the fitted values
fitted.values <- alpha+beta*Psoil

# This is a plot of the regression (line), data (open circles), and the fitted values (solid circles)
plot(Psoil,Pcorn)
points(Psoil, fitted.values, pch = 19)
abline(reg)

# These are needed in the data.eq_reg table
res = Pcorn-fitted.values
lag1 = c(NA, head(res,-1))

# In data.eq_reg table "prob" is hard to calculate - I had to write an new R function to do it!
inv.qnorm = function(x){
  incr = 0.01
  y = seq(-100,100,incr)
  pdf = dnorm(y, mean, sd)
  cdf = cumsum(pdf)*incr
  i = min(which(y>x))
  val = cdf[i]
}

# This calculates "prob" which is a column in data.eq_reg
mean <- mean(res)
sd<- sd(res)
prob = sapply(res, inv.qnorm)

data.eq_reg = data.frame(Pcorn = Pcorn, Model = fitted.values, res = res, res2 = res^2, lag1 = lag1, prob = prob, rank = rank(res))
SS.res = sum(data.eq_reg$res2)

# more plots
plot(data.eq_reg$lag1, data.eq_reg$res)
plot(data.eq_reg$Model, data.eq_reg$res)
plot(data.eq_reg$rank, data.eq_reg$prob)
# add regression line to last plot
mod = lm(data.eq_reg$prob~data.eq_reg$rank)
abline(mod)
