setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learneconometrics/CarterHill_PrinciplesOfEconometrics/Chapter14_GARCH")
rm(list=ls())
library(ggfortify)
library(foreign)
library(tseries)
library(fGarch)
#install.packages("moments")




## Part a)  estimate ARCH(1)
sp <- read.dta("sp.dta") # weekly returns to us s&p 500

# (1) check stationarity
mean(sp$r)
res <- dickeyFullerTest(sp$r, type="drift", diffed.lags = 10)
autoplot(acf(res, lag.max = 40, plot = FALSE))

# (2) estimate arch(1)
arch <- ARCH(sp, p=1)
autoplot(acf(arch@residuals, lag.max = 30, plot = FALSE))
# still some conditional heteroskedasticity in sp$r is left
autoplot(acf(arch@residuals^2, lag.max = 30, plot = FALSE))

describe(sp$r)
ggplot(data=sp, aes(x=r)) + geom_histogram(fill="coral")
autoplot(ts(sp$r))
autoplot(ts(arch@h.t))




## Part b) estimate TARCH

## need help, skip for now and develop my own function
# but how would i estimate that first mean equation? 
# y = vector of data time series
TARCH1 <- function(y, p){
      # r-hat = mu + errors
      # errors = y-obs - mu, mu = fit function rt
      mu <- rep(arch@fit$coef[[1]], length(y))
      e <- y - mu # this is e-hat to use in ht
      delta <- sapply(e, 
                      function(elem) {if(elem < 0) 1 else 0})
      e_1 <- c(NA, e[1:(length(e) - 1)])
      lm <- lm()
      # how to go from here? I need a variance vector...
}

