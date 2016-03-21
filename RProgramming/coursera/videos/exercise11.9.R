setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learneconometrics/CarterHill_PrinciplesOfEconometrics/Chapter11_SimultaneousEquationsModels")
rm(list=ls())

library(lmtest)
library(dplyr)
library(foreign)
library(ggfortify)
#install.packages("systemfit")
library(systemfit)                  ## for automatic 2sls
# Data from: http://www.principlesofeconometrics.com/poe4/poe4stata.htm





## Part a) 
broiler <- read.dta("newbroiler.dta")
head(broiler)
broiler$lnq <- log(broiler$qprod)
broiler$lnp <- log(broiler$p)
broiler$lny <- log(broiler$y)
broiler$lnpb <- log(broiler$pb)
broiler$lnpf <- log(broiler$pf)
broiler$qprod_1 <- c(NA, broiler$qprod[1:51])
broiler$lnqprod_1 <- log(broiler$qprod_1)
broiler$DLPB <- c(NA, diff(broiler$lnpb))
broiler$DLNP <- c(NA, diff(broiler$lnp))
broiler$DLY <- c(NA, diff(broiler$lny))
broiler$time <- broiler$year

broiler <- broiler[11:50,]
head(broiler)
nrow(broiler)



## TODO: why isn't intercept the same?
DLP.reduced.lm <- lm(data=broiler, DLNP ~ DLY + DLPB + lnpf + time + 
                           lnqprod_1)
summary(DLP.reduced.lm)
