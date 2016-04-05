setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learneconometrics/CarterHill_PrinciplesOfEconometrics/Chapter13_VARandVECmodels")
rm(list=ls())

#install.packages("tsDyn")
library(tsDyn)
library(ggfortify)
library(foreign)
library(reshape2)
library(urca)
# Data from: http://www.principlesofeconometrics.com/poe4/poe4stata.htm



fred <- read.dta("fred.dta")
fred$time <- seq(1, 200)
ggplot() + 
      geom_line(data=fred, aes(x=time, y=c), lwd=1, color="red") + 
      geom_line(data=fred, aes(x=time, y=y), lwd=1, color="blue")

# ESTIMATE A VAR MODEL

# Step 1 - adf test for unit roots for C and Y
ur.df(fred$c, type="drift", lags = 0)

adf.test(fred$c, alt="s", k=0)
