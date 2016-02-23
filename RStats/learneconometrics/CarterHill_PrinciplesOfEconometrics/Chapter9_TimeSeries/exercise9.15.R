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



# Lagrange Multiplier Test for Autocorrelation of Errors
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
tail(phillipsData)

# Lagrange Multiplier Test for Autocorrelation of Errors
lagrange.lm <- lm(data=phillipsData, E ~ du + E_1)
summary(lagrange.lm)
nrow(phillipsData)
89*0.3102



# T-test for Autocorrelation of Errors 

## method (i)
ttest.lm.1 <- lm(data=phillipsData, inf ~ du + E_1)
summary(ttest.lm.1) # see? t-value is 6.219, df = N-2 = 88-2 = 86
anova(ttest.lm.1)   # see? F-value is 38.67

## method (ii)
phillipsData2 <- data.frame(inf=phillips$inf, du=c(NA, diff(phillips$u)))
phillipsData2 <- na.omit(phillipsData2)
phillipsData2$E <- e
phillipsData2$E_1 <- c(0, e[1:89]) # see? put a zero
head(phillipsData2)

ttest.lm.2 <- lm(data=phillipsData2, inf ~ du + E_1)
summary(ttest.lm.2)     # see? t-value is 6.202, df = N-2 = 89-2 = 87
anova(ttest.lm.2)       # see? F-value is 38.47


