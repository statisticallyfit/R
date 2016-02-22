setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learneconometrics/CarterHill_PrinciplesOfEconometrics/Chapter9_TimeSeries")
rm(list=ls())
library(foreign)
# Data from: http://www.principlesofeconometrics.com/poe4/poe4stata.htm


okun <- read.dta("okun.dta")
okun

g <- okun$g
okunDataNA <- data.frame(DU=c(NA, diff(okun$u)), 
                       G=g,  # G is same as G_0
                       G_1=c(NA, g[1:97]), 
                       G_2=c(NA, NA, g[1:96]), 
                       G_3=c(NA, NA, NA, g[1:95]),
                       G_4=c(NA, NA, NA, NA, g[1:94]),
                       G_5=c(NA, NA, NA, NA, NA, g[1:93]),
                       G_6=c(NA, NA, NA, NA, NA, NA, g[1:92]))
okunDataNA
okunData <- na.omit(okunDataNA)

# Make the models
okun.lm0 <- lm(data=okunData, DU ~ G)
okun.lm1 <- lm(data=okunData, DU ~ G + G_1)
okun.lm2 <- lm(data=okunData, DU ~ G + G_1 + G_2)
okun.lm3 <- lm(data=okunData, DU ~ G + G_1 + G_2 + G_3)
okun.lm4 <- lm(data=okunData, DU ~ G + G_1 + G_2 + G_3 + G_4)
okun.lm5 <- lm(data=okunData, DU ~ G + G_1 + G_2 + G_3 + G_4 + G_5)
okun.lm6 <- lm(data=okunData, DU ~ G + G_1 + G_2 + G_3 + G_4 + G_5 + G_6)

# View model coefficients
okun.lm0
okun.lm1
okun.lm2
okun.lm3
okun.lm4
okun.lm5
okun.lm6


# Calculate AIC and BIC for all the models!

## SSE = sum(obs - pred)^2
## AIC = ln(SSE/N) + 2K/N
## BIC = ln(SSE/N) + Kln(N)/N

N <- 92

K <- 2
SSE.0 <- sum( (okunData$DU - okun.lm0$fitted.values)^2 ); SSE.0
AIC.0 <- log(SSE.0/N) + 2*K/N; AIC.0
BIC.0 <- log(SSE.0/N) + K*log(N)/N; BIC.0
AICc.0 <- -(abs(AIC.0) - 1 - log(2*pi)); AICc.0
BICc.0 <- -(abs(BIC.0) - 1 - log(2*pi)); BICc.0

K <- 3
SSE.1 <- sum( (okunData$DU - okun.lm1$fitted.values)^2 ); SSE.1
AIC.1 <- log(SSE.1/N) + 2*K/N; AIC.1
BIC.1 <- log(SSE.1/N) + K*log(N)/N; BIC.1
AICc.1 <- -(abs(AIC.1) - 1 - log(2*pi)); AICc.1
BICc.1 <- -(abs(BIC.1) - 1 - log(2*pi)); BICc.1

K <- 4
SSE.2 <- sum( (okunData$DU - okun.lm2$fitted.values)^2 ); SSE.2
AIC.2 <- log(SSE.2/N) + 2*K/N; AIC.2
BIC.2 <- log(SSE.2/N) + K*log(N)/N; BIC.2
AICc.2 <- -(abs(AIC.2) - 1 - log(2*pi)); AICc.2
BICc.2 <- -(abs(BIC.2) - 1 - log(2*pi)); BICc.2

K <- 5
SSE.3 <- sum( (okunData$DU - okun.lm3$fitted.values)^2 ); SSE.3
AIC.3 <- log(SSE.3/N) + 2*K/N; AIC.3
BIC.3 <- log(SSE.3/N) + K*log(N)/N; BIC.3
AICc.3 <- -(abs(AIC.3) - 1 - log(2*pi)); AICc.3
BICc.3 <- -(abs(BIC.3) - 1 - log(2*pi)); BICc.3

K <- 6
SSE.4 <- sum( (okunData$DU - okun.lm4$fitted.values)^2 ); SSE.4
AIC.4 <- log(SSE.4/N) + 2*K/N; AIC.4
BIC.4 <- log(SSE.4/N) + K*log(N)/N; BIC.4
AICc.4 <- -(abs(AIC.4) - 1 - log(2*pi)); AICc.4
BICc.4 <- -(abs(BIC.4) - 1 - log(2*pi)); BICc.4

K <- 7
SSE.5 <- sum( (okunData$DU - okun.lm5$fitted.values)^2 ); SSE.5
AIC.5 <- log(SSE.5/N) + 2*K/N; AIC.5
BIC.5 <- log(SSE.5/N) + K*log(N)/N; BIC.5
AICc.5 <- -(abs(AIC.5) - 1 - log(2*pi)); AICc.5
BICc.5 <- -(abs(BIC.5) - 1 - log(2*pi)); BICc.5

K <- 8
SSE.6 <- sum( (okunData$DU - okun.lm6$fitted.values)^2 ); SSE.6
AIC.6 <- log(SSE.6/N) + 2*K/N; AIC.6
BIC.6 <- log(SSE.6/N) + K*log(N)/N; BIC.6
AICc.6 <- -(abs(AIC.6) - 1 - log(2*pi)); AICc.6
BICc.6 <- -(abs(BIC.6) - 1 - log(2*pi)); BICc.6
