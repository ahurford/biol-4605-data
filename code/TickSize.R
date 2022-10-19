# Data from Box 9.1 Sokal and Rohlf 2012 
#Scutum width in ticks on 4 Hosts (rabbits)
TickSize = read.csv("https://raw.githubusercontent.com/ahurford/biol-4605-data/main/data/TickSize.csv")
boxplot(Wscut~Host, data=TickSize)
TSizeModel<-lm(Wscut~(1|HostNumber),data=TickSize)
summary(TSizeModel)
plot(TSizeModel)
hist(TSizeModel$residuals)
hist(TickSize$Wscut)
qqnorm(TickSize$Wscut)
qqnorm(TSizeModel$residuals)
anova(TSizeModel)    #To see ANOVA table
LR<-(1-0.3236)^(-37/2)
LR
