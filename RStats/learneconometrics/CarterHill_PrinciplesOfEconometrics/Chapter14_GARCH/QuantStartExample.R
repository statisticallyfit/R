setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learneconometrics/CarterHill_PrinciplesOfEconometrics/Chapter14_GARCH")
library(quantmod)
library(ggfortify)


getSymbols("^FTSE")

# calculate the differences of logs of closing price
ftrt <- diff(log(Cl(FTSE)))
ft <- as.numeric(ftrt)
ft <- ft[!is.na(ft)] # removing first NA
head(ftrt)
autoplot(ts(ft)) # very clear volatility changes


