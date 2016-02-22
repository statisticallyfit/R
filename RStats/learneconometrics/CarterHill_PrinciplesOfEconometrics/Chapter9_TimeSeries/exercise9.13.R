setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learneconometrics/CarterHill_PrinciplesOfEconometrics/Chapter9_TimeSeries")
rm(list=ls())

library(foreign)
library(ggfortify)
# Data from: http://www.principlesofeconometrics.com/poe4/poe4stata.htm


sales <- read.dta("ex9_13.dta")
sales
# how to start at Dec 28,2005 and end at Dec 25,2007?
sales.ts <- ts(sales, start=2005, frequency = 52)
sales.ts
autoplot(sales.ts)
