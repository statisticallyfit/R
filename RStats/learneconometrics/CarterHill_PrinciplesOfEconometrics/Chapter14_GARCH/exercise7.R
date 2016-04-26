setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learneconometrics/CarterHill_PrinciplesOfEconometrics/Chapter14_GARCH")
rm(list=ls())
library(ggfortify)
library(foreign)
library(tseries)
library(fGarch)
#install.packages("moments")

library(fBasics)


## Part a) 
exrate <- read.dta("exrate.dta")
autoplot(ts(exrate$s), main="Exchange rate US/AUS", ts.size=1, colour="blue")
ggplot(data=exrate, aes(x=s)) + geom_histogram(fill="red")

descriptiveStats(exrate$s)




## Part b) estimate GARCH(1,1)
garch <- GARCH(euro, p=1, q=1)
mean(exrate$s)

# test for stationarity : IS STATIONARY 
exrate.df <- ur.df(exrate$s, type="drift", lags=0); exrate.df
autoplot(acf(exrate.df@res, lag.max = 20, plot=FALSE))
exrate.df@testreg
