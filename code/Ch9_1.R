

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
#plot(data.eq_reg$lag1, data.eq_reg$res)
#plot(data.eq_reg$Model, data.eq_reg$res)
#plot(data.eq_reg$rank, data.eq_reg$prob)
# add regression line to last plot
#mod = lm(data.eq_reg$prob~data.eq_reg$rank)
#abline(mod)
# New plot to check for residuals
#plot(Psoil,lag1-res)

# A series of plots to check model assumpations.
plot(reg)

# Randomization function
F.rand = function(x,y){
result = replicate(1000,anova(lm(sample(y,length(y),TRUE)~x))$`F value`[1])
}

# Using the randomization function
rand = F.rand(Psoil,Pcorn)
F.p = data.frame(x = seq(1/1000,1,1/1000),F=sort(rand))
plot(F.p$F,F.p$x, main = "Randomization", xlab = "F-value", ylab = "CDF")

# Actual F-value from Corn data
F.data <- anova(lm(Pcorn~Psoil))$`F value`[1]
lines(c(F.data,F.data), c(0,1))

# proportion of random results larger than F-value for data
p1 = F.p$x[min(which(F.p$F>F.data))]

lines(c(0,max(rand)),c(p1,p1))
p=1-p1

# Likelihood ratio
n = length(Pcorn)
LR = (SS.res/SS.total)^(-n/2)
