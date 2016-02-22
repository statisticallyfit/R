setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learneconometrics/CarterHill_PrinciplesOfEconometrics/Chapter9_TimeSeries")
rm(list=ls())

library(foreign)
library(ggfortify)
# Data from: http://www.principlesofeconometrics.com/poe4/poe4stata.htm


# Part a)
sales <- read.dta("ex9_13.dta")
sales
salesData <- data.frame(Date=seq(1, 157), Sales=sales$sales, Adv=sales$adv)
salesData

# how to start at Dec 28,2005 and end at Dec 25,2007?
sales.ts <- ts(sales$sales, start=2005, frequency = 52)
adv.ts <- ts(sales$adv, start=2005, frequency = 52)
autoplot(sales.ts)
autoplot(adv.ts)
ggplot(data=salesData, aes(x=Date, y=Sales)) + 
      geom_line() + 
      geom_hline(yintercept=mean(salesData$Sales), colour="blue", lwd=1)
ggplot(data=salesData, aes(x=Date, y=Adv)) +
      geom_line() + 
      geom_hline(yintercept=mean(salesData$Adv), colour="red", lwd=1)



#
