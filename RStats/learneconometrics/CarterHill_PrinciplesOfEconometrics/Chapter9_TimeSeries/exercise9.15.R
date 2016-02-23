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
head(banglaData); tail(banglaData)

bangla.lm <- lm(data=banglaData, lnA ~ lnP)
summary(bangla.lm)

autoplot(acf(bangla.lm$residuals, lag.max=34, plot=FALSE))



# Part b) LM test of autocorrelation (joint test of acf values)

# method (2) way
e <- bangla.lm$residuals 
banglaZero <- banglaData
banglaZero$E <- e 
banglaZero$E_1 <- c(0, e[1:33])     # see? 0 instead of NA
head(banglaZero); tail(banglaZero)
lagrange.lm <- lm(data=banglaZero, E ~ lnP + E_1)
summary(lagrange.lm)
# calculate R^2 by hand
sse <- sum( (banglaZero$E - lagrange.lm$fitted.values)^2 )
sst <- sum( (banglaZero$E - mean(banglaZero$E))^2  )
R2 <- 1 - sse/sst
T <- 34
LM <- T * R2; LM
# now compare to chi-statistic and find p-value
# df for chi statistic is = number of lagged residuals that are included
df <- 1
chi.star <- qchisq(0.95, df); chi.star
p.value <- 1 - pchisq(LM, df); p.value   # reject the null! there is autocorrelation

