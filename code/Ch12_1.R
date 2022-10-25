data <- read.csv("https://raw.githubusercontent.com/ahurford/biol-4605-data/main/data/corn2.csv")
mod = lm(PaP~ioP+oP+ioP*oP,data = data)
plot(data$ioP, data$PaP, xlab = "inorganic phosporhous, ppm", ylab = "PaP, ppm")
mod.iop = lm(PaP~ioP, data=data)
abline(mod.iop)
plot(data$oP, data$PaP, xlab = "organic phosporhous, ppm", ylab = "Pap, pmm")
mod.op = lm(PaP~oP, data=data)
abline(mod.op)
mod
