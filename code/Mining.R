require(ggplot2)
require(reshape2)

fdisaster = c(3,1,1,3,6,1,1)
rfdisaster = fdisaster/sum(fdisaster)
number = seq(0,6)
xbar = sum(rfdisaster*number)
fmodel = dpois(number,lambda = xbar)

output = data.frame(model = fmodel, data = rfdisaster)
output = data.frame(x=number,melt(output))

g1 = ggplot(data = output, aes(x=x, y=value, fill = variable))+
  geom_bar(stat = "identity", position = "dodge")+
  xlab("Number of disasters (yearly)")+
  ylab("Relative frequency")+
  ggtitle("Number of coal mining disasters (England), 1851-1866")
