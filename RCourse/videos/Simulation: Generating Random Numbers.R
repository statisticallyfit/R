# rnorm, dnorm, pnorm, rpois
x <- rnorm(10)
x
x <- rnorm(10, mean=20, sd=2)
x

set.seed(1)
rnorm(5)
rnorm(5)
set.seed(1)
rnorm(5)

rpois(10, lambda=1)
rpois(10, 2)
rpois(10, 20)
ppois(2, lambda=2) # P(x <= 2)
ppois(4, 2)
ppois(6, 2)
