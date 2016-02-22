setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learneconometrics/CarterHill_PrinciplesOfEconometrics/Chapter9_TimeSeries")
rm(list=ls())

library(foreign)
library(ggfortify)
# Data from: http://www.principlesofeconometrics.com/poe4/poe4stata.htm


## Part a)

# ln(area)=y, ln(price)=x (not lagged)
bangla <- read.dta("bangla.dta")
lnp <- log(bangla$p)
lna <- log(bangla$a)
banglaData <- data.frame(lnA=lna, lnP=lnp)
head(banglaData)
tail(banglaData)

bangla.lm <- lm(data=banglaData, lnA ~ lnP)
summary(bangla.lm)

autoplot(acf(bangla.lm$residuals, lag.max=34, plot=FALSE))


e <- bangla.lm$residuals 
auxData <- data.frame(lnA=banglaData$lnA, lnP=banglaData$lnP, E_1=c(NA, e[1:33]))
auxData <- na.omit(auxData)
head(auxData)
auxiliary.lm <- lm(data=auxData, E ~ E_1 + lnP)
summary(auxiliary.lm)
