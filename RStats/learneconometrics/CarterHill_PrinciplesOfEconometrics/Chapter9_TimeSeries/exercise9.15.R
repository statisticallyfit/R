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

# creating data.frame with NA removed
e <- bangla.lm$residuals
lnp <- banglaData$lnP  
lna <- banglaData$lnA
banglaNA <- data.frame(lnA=lna, lnA_1=c(NA, lna[1:33]), 
                          lnP=lnp, lnP_1=c(NA, lnp[1:33]), 
                          E=e, E_1=c(NA, e[1:33]))
banglaNA <- na.omit(banglaNA)
head(banglaNA); tail(banglaNA)      

banglaZero <- data.frame(lnA=lna, lnA_1=c(0, lna[1:33]), 
                         lnP=lnp, lnP_1=c(0, lnp[1:33]), 
                         E=e, E_1=c(0, e[1:33]))
head(banglaZero); tail(banglaZero)


# Part b) LM test of autocorrelation (joint test of acf values)

# method (2) way
lagrange.lm <- lm(data=banglaZero, E ~ lnP + E_1)
summary(lagrange.lm)
# calculate R^2 by hand
sse <- sum( (banglaZero$E - lagrange.lm$fitted.values)^2 )
sst <- sum( (banglaZero$E - mean(banglaZero$E))^2  )
R2 <- 1 - sse/sst; R2
T <- 34
LM <- T * R2; LM
# now compare to chi-statistic and find p-value
# df for chi statistic is = number of lagged residuals that are included
df <- 1
chi.star <- qchisq(0.95, df); chi.star
p.value <- 1 - pchisq(LM, df); p.value   # reject the null! there is autocorrelation



# Part c) calculating HAC stderrors for confidence intervals

### ??? how to do it if you have only 1 predictor??? cannot use examples12.R
### (wooldridge) method



# Part d) estimating an AR(1) model
ar.lm <- lm(data=banglaNA, lnA ~ lnP + lnA_1 + lnP_1)
summary(ar.lm)
# df = nrow(banglaNA) - 2 = 29
head(banglaNA)


