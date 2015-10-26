library(ggplot2)

# simulate from y = B0 + B1x + e
# e = N(0, 2^2)
# Assume x = N(0, 1^1), B0 = 0.5, B1 = 2

set.seed(20)
x <- rnorm(100)
e <- rnorm(100, 0, 2)
y <- 0.5 + 2*x + e
summary(y)

# plot the data
d <- data.frame(x = x, y = y)
ggplot(d, aes(x, y)) + geom_point(shape=19) + geom_smooth(method="lm")


# What if x is binary?
set.seed(10)
x <- rbinom(100, 1, 0.5)
e <- rnorm(100, 0, 2)
y <- 0.5 + 2*x + e
summary(y)

d <- data.frame(x = x, y = y)
ggplot(d, aes(x, y)) + geom_point(shape=19) + geom_smooth(method="lm")



# Y = Poisson(mean)
# log mean = B0 + B1x
set.seed(1)
x <- rnorm(100)
log.mu <- 0.5 + 0.3*x
y <- rpois(100, exp(log.mu))
summary(y)

d1 <- data.frame(x=x, y=log.mu)
d <- data.frame(x = x, y = y)
ggplot(d, aes(x, y)) + geom_point(shape=19)

