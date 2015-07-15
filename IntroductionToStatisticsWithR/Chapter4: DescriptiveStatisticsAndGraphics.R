x = rnorm(50)
mean(x)
sd(x)
quantile(x)
summary(x)

decileSpec = seq(0, 1, by=0.10)
quantile(x, decileSpec)

data = 1:10
quantile(data)

