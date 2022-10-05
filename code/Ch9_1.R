
# This imports the data into R from a website without needing to download
data <- read.csv('https://raw.githubusercontent.com/ahurford/biol-4605-data/main/corn.csv', fill=TRUE)
Pcorn = data$Pcorn
Psoil = data$Psoil

plot(Psoil, Pcorn)
mod <- lm(Pcorn~Psoil)
alpha <- unname(coef(mod)[1])
beta <- unname(coef(mod)[2])
fitted.values <- alpha+beta*Psoil
points(Psoil, fitted.values, pch = 19)
abline(mod)
res = Pcorn-fitted.values

data.eq_null = data.frame(Psoil = Psoil, Data = Pcorn, Model = rep(mean(Pcorn),length(Pcorn)), res = Pcorn-mean(Pcorn), res2 = (Pcorn-mean(Pcorn))^2)
sum(tab_null$res2)

lag1 = c(NA, head(res,-1))

inv.qnorm = function(x){
  incr = 0.01
  y = seq(-100,100,incr)
  pdf = dnorm(y, mean, sd)
  cdf = cumsum(pdf)*incr
  i = min(which(y>x))
  val = cdf[i]
}

mean <- mean(res)
sd<- sd(res)
prob = sapply(res, inv.qnorm)

data.eq_reg = data.frame(Pcorn = Pcorn, Model = fitted.values, res = res, res2 = res^2, lag1 = lag1, prob = prob, rank = rank(res))


