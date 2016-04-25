setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learneconometrics/CarterHill_PrinciplesOfEconometrics/Chapter14_GARCH")
rm(list=ls())
library(ggfortify)
library(foreign)
library(tseries)
library(fGarch)
library(reshape2)

# Tells which models to pick for garch: 
# http://yunus.hacettepe.edu.tr/~iozkan/eco665/archgarch.html


byd <- read.dta("byd.dta")

# Estimate the ARCH: 
ARCH(byd, p=1)

# Estimate the GARCH
GARCH(byd, p=1, q=1)

autoplot(ts(byd$r))
autoplot(ts(arch@h.t), main="Conditional Heteroskedasticity function")

d <- data.frame(time=seq(1,500), returns=byd$r, variance=arch@h.t)
ggplot(data=d, aes(x=time)) + 
      geom_line(aes(y=returns), color="orange") + 
      geom_line(aes(y=variance), color="purple") + 
      ggtitle("Returns and variance function")
d.melt <- melt(d, id="time"); head(melt)
ggplot(data=d.melt, aes(x=time, y=value, colour=variable)) + geom_line()



#install.packages("rugarch")
library(rugarch)
garch.spec <- ugarchspec(variance.model=list(garchOrder=c(1,1)), 
                         mean.model=list(armaOrder=c(0, 0)))
#garch.fit <- 
