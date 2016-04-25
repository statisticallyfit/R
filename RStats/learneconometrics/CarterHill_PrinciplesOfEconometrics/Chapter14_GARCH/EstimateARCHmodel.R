setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learneconometrics/CarterHill_PrinciplesOfEconometrics/Chapter14_GARCH")
rm(list=ls())
library(ggfortify)
library(foreign)
library(tseries)
library(fGarch)

# Tells which models to pick for garch: 
# http://yunus.hacettepe.edu.tr/~iozkan/eco665/archgarch.html


byd <- read.dta("byd.dta")
byd.garch <- garch(byd$r, order=c(1,0), trace=F)
byd.garch
?garch


byd.garch <- garchFit(~garch(1, 0), data=byd, trace=F)
byd.garch@fit$matcoef
ARCH(byd, lags=1)

data=byd; lags=1
