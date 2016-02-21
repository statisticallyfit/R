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

okun.lm0 <- lm(data=okunData, DU ~ G)
okun.lm1 <- lm(data=okunData, DU ~ G + G_1)
okun.lm2 <- lm(data=okunData, DU ~ G + G_1 + G_2)
okun.lm3 <- lm(data=okunData, DU ~ G + G_1 + G_2 + G_3)
okun.lm4 <- lm(data=okunData, DU ~ G + G_1 + G_2 + G_3 + G_4)
okun.lm5 <- lm(data=okunData, DU ~ G + G_1 + G_2 + G_3 + G_4 + G_5)
okun.lm6 <- lm(data=okunData, DU ~ G + G_1 + G_2 + G_3 + G_4 + G_5 + G_6)

okun.lm0
okun.lm1
okun.lm2
okun.lm3
okun.lm4
okun.lm5
okun.lm6
