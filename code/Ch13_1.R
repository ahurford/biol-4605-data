data = read.csv("https://raw.githubusercontent.com/ahurford/biol-4605-data/main/data/limpet.csv")
data = data.frame(tank = c(rep(1,3), rep(2,3), rep(3,3), rep(4,3), rep(5,3)), resp = c(100,98,90,91,102,97,110,89,95,105,110,112,113,94,100))
boxplot(resp~tank, data=data)
mod2 = lm(resp~(1|tank),data=data)