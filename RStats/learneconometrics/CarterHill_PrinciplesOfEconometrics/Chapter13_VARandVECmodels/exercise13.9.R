setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learneconometrics/CarterHill_PrinciplesOfEconometrics/Chapter13_VARandVECmodels")
rm(list=ls())

library(tsDyn)
library(ggplot2)
library(foreign)
library(ggfortify)
library(urca)
library(vars)
# Data from: http://www.principlesofeconometrics.com/poe4/poe4stata.htm



## Part c) confirm the residuals are I(0) (so cointegration exists)
qtm <- read.dta("qtm.dta")
qtm <- data.frame(p=qtm$p, m=qtm$m)

# TODO: why isn't equation same as theirs? 
res <- cointegrationTest(qtm, type="constant", lags = 1)
autoplot(acf(res, lag.max = 20, plot=FALSE))

res <- VEC(qtm, type="const", lag = 1)
autoplot(acf(res$y, lag.max = 20, plot=FALSE))
autoplot(acf(res$x, lag.max = 20, plot=FALSE))

v <- suppressWarnings(VECM(ts(qtm), lag=1, 
                           r=1, include="const", estim="2OLS"))
v
cat("hi")
cat("   there")
