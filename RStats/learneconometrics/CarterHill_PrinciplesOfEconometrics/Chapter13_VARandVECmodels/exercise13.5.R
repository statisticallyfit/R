setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learneconometrics/CarterHill_PrinciplesOfEconometrics/Chapter13_VARandVECmodels")
rm(list=ls())

library(ggplot2)
library(foreign)
library(ggfortify)
library(reshape2)
library(urca)
# Data from: http://www.principlesofeconometrics.com/poe4/poe4stata.htm



## Part a) - are the series stationary?
gdp <- read.dta("gdp.dta")
