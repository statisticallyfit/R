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



# Part b) LM test of autocorrelation (joint test of acf values)

e <- bangla.lm$residuals 
banglaData$E <- e 
banglaData$E_1 <- c(NA, e[1:33])
banglaData <- na.omit(banglaData)
head(banglaData)
auxiliary.lm <- lm(data=banglaData, E ~ lnP + E_1)
summary(auxiliary.lm)
33*0.1633


phillips <- read.dta("phillips_aus.dta")
phillipsData <- data.frame(inf=phillips$inf, du=c(NA, diff(phillips$u)))
phillipsData <- na.omit(phillipsData)
head(phillipsData)
tail(phillipsData)
phillips.lm <- lm(data=phillipsData, inf ~ du)
summary(phillips.lm)

e <- phillips.lm$residuals
phillipsData$E <- e
phillipsData$E_1 <- c(NA, e[1:89])
phillipsData <- na.omit(phillipsData)
head(phillipsData)

auxiliary.lm <- lm(data=phillipsData, E ~ du + E_1)
summary(auxiliary.lm)
nrow(phillipsData)
89*0.3102
