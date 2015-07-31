# EXAMPLE
reg.data <- read.csv("data/tannin.csv")
attach(reg.data)
reg.data
plot(tannin, growth, pch=19, col="cyan")
lm(growth~tannin)
abline(lm(growth~tannin, col="dodgerblue"))

fitted <- predict(lm(growth~tannin))
fitted
lines(c(0,0), c(12, fitted[1]))
for(i in 1:length(fitted))
  lines(c(tannin[i], tannin[i]), c(growth[i], fitted[i]), col="red")
