setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learneconometrics/CarterHill_PrinciplesOfEconometrics/Chapter9_TimeSeries")
rm(list=ls())

library(lmtest)
library(foreign)
library(ggfortify)
# Data from: http://www.principlesofeconometrics.com/poe4/poe4stata.htm


## Part a)
consumption <- read.dta("consumptn.dta") #from 1960 Q1 to 2009 Q4
c <- consumption$congwth
i <- consumption$incgwth
c.ts <- ts(c, start=1960, frequency = 4); c.ts
i.ts <- ts(i, start=1960, frequency = 4); i.ts

years <- seq(1960, 2009); years
time <- c()
for (y in years) {time <- append(time, rep(y, 4))}
time # col1

df <- data.frame(Date=time, Consump=c, Inc=i)
head(df); tail(df)

autoplot(c.ts)
ggplot(data=df, aes(x=Date, y=Consump)) + 
      geom_line() + 
      geom_hline(yintercept=mean(df$Consump), colour="blue", lwd=1)

autoplot(i.ts)
ggplot(data=df, aes(x=Date, y=Inc)) + 
      geom_line() + 
      geom_hline(yintercept=mean(df$Consump), colour="red", lwd=1)

# They have serial correlation, but seem stationary around their mean
# TODO: how to check for serial correlation?
