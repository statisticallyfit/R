setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learneconometrics/CarterHill_PrinciplesOfEconometrics/Chapter9_TimeSeries")
rm(list=ls())
# Data from: http://www.principlesofeconometrics.com/poe4/poe4stata.htm



# QUESTION 9.2

sales <- read.dta("ex9_2.dta")
head(sales)
salesData <- data.frame(SALES=sales$sales, ADV=sales$adv, 
                       ADV_1=c(NA, sales$adv[1:104]), 
                       ADV_2=c(NA, NA, sales$adv[1:103]))
salesData

sales.lm <- lm(data=salesData, SALES ~ ADV + ADV_1 + ADV_2)
summary(sales.lm)



# QUESTION 9.4 (correlogram from 4.b)
e <- c(0.28, -0.31, -0.09, 0.03, -0.37, -0.17, -0.39, -0.03, 0.03, 1.02)

e.ts <- ts(e, start=1, frequency = 1)
autoplot(e.ts)
autoplot(acf(e.ts, plot=FALSE)) # correlogram
e.acf <- acf(e.ts, plot = FALSE)
e.acf # checking r1, and r2



# QUESTION 9.5 (correlogram for 5.a)
growth <- read.dta("growth47.dta")
growth
growth.ts <- ts(growth, start=1947, frequency = 4)
growth.ts <- lag(growth.ts, -1)
growth.ts

autoplot(growth.ts)
autoplot(acf(growth.ts, plot = FALSE))
