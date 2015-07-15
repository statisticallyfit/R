# **************** Normal Distribution **************** 

# d = height of probability density function
dnorm(0, mean=0, sd=1) #same as dnorm(0)
dnorm(0, mean=4, sd=1) #same as dnorm(0, mean=4)
dnorm(0, mean=4, sd=10)
#
v = c(0, 1, 2)
dnorm(v)
#
x = seq(-5, 5, by=0.1)
y = dnorm(x) #the default mean=0, sd=1
plot(x, y)
#
y = dnorm(x, mean=2.5, sd=0.1)
plot(x, y)



# p = cumulative density function
pnorm(0) #same as pnorm(0, mean=0, sd=1)
pnorm(1)
pnorm(0, mean=2, sd=3)
v = c(0, 1, 2)
pnorm(v)
pnorm(-1)
1 - pnorm(1)
pnorm(1, lower.tail=FALSE)
1 - (pnorm(-2) + pnorm(2, lower.tail=FALSE))

normalCdf <- function(lowerBound, upperBound) {
  #same as    1 - (pnorm(-2) + pnorm(2, lower.tail=FALSE))
  return (1 - (pnorm(lowerBound) + 1 - pnorm(upperBound)))
}
normalCdf(-1, 1)
normalCdf(-2, 2)
normalCdf(-3, 3)

x = seq(-5, 5, by=0.1)
y1 = pnorm(x, mean=1, sd=1)
y2 = pnorm(x, mean=2, sd=1)
plot(x, y1, col='red')
plot(x, y2, col='blue')



# q = inverse cumulative density function
qnorm(0.5, mean=0, sd=1) #inverse cdf
qnorm(0.02275013, mean=2, sd=1) # ~ rounds to 0
qnorm(0.5, mean=1, sd=2)
qnorm(0.5, mean=2, sd=2)
qnorm(0.5, mean=2, sd=10)
qnorm(0.25, mean=2, sd=2)
qnorm(0.333)
qnorm(0.333, sd=3)
#
v = c(0.1, 0.3, 0.75) #list of areas
qnorm(v)
#
x = seq(0, 1, by=0.05)
y = qnorm(x, mean=30, sd=2)
plot(x, y, col='blue')



# r = random numbers
rnorm(4)
rnorm(4, mean=3, sd=3)
#
y <- rnorm(200)
hist(y, col='pink')
y <- rnorm(200, mean=-2, sd=4)
hist(y)
#
qqnorm(y)
qqline(y, col='green') #line through q-q plot





# **************** t Distribution **************** 

# dt = height of t distrbution
x <- seq(-20, 20, by=0.5)
y <- dt(x, df=10)
plot(x, y, type='l')
y <- dt(x, df=50)
plot(x, y)



# pt = tcdf
pt(-3, df=10)
1 - pt(3, df=10)
pt(3, df=10)
#
tValue <- (mean(x)-2)/sd(x)
pValue <- pt(tValue, df=20)
pValue



# qt = inverse tcdf
qt(0.05, df=10)
qt(0.10, df=10, lower.tail=FALSE)



# rt = random t-distributed numbers
rt(3, df=10)
rt(10, df=100)






# **************** Binomial Distribution **************** 

# dbinom(x, n, p)
x <- seq(0, 50, by=1)
y <- dbinom(x, 50, 0.2)
plot(x, y)
y <- dbinom(x, 50, 0.6)
plot(x, y)



# pbinom(x, n, p)
pbinom(24, 50, 0.5)
1 - pbinom(25, 50, 0.5)
pbinom(25, 50, 0.5)
pbinom(26, 50, 0.5)
pbinom(25, 51, 0.5)
pbinom(25, 50, 0.25)



# qbinom(area left tail, n, p)
qbinom(0.5, 51, 1/2)
qbinom(0.25, 51, 1/2)
pbinom(23, 51, 1/2)



# rbinom(number of random numbers, n, p)
rbinom(200, 50, 0.2)
rbinom(10, 100, 0.2)
rbinom(5, 100, 0.7)






# **************** Chi-Squared Distribution ***************

# dchisq(x, df=df0)
x <- seq(0, 80, by=0.5)
y <- dchisq(x, df=4)
plot(x, y)
y <- dchisq(x, df=30)
plot(x, y)



# pchisq(x, df=df0)
pchisq(2, df=10) # left tailed
pchisq(2, df=10, lower.tail=FALSE)
pchisq(3, df=20)



# qchisq(left area, df=df0)
qchisq(0.05, df=10)
qchisq(0.05, df=10, lower.tail=FALSE)
qchisq(0.95, df=10)
v <- c(0.005, 0.025, 0.05)
qchisq(v, df=25)



# rchisq(number, df=df0)
rchisq(3, df=10)
rchisq(3, df=70)
