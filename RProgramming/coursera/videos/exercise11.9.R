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
newbroiler <- read.dta("newbroiler.dta")
head(newbroiler); tail(newbroiler)
broiler <- data.frame(time=newbroiler$year)
broiler$lnqprod <- log(newbroiler$qprod)
broiler$lnp <- log(newbroiler$p)
broiler$lny <- log(newbroiler$y)
broiler$lnpb <- log(newbroiler$pb)
broiler$lnpf <- log(newbroiler$pf)
broiler$lnexpts <- newbroiler$lexpts
broiler$qprod_1 <- c(NA, newbroiler$qprod[1:51])
broiler$lnqprod_1 <- log(broiler$qprod_1)
broiler$DLPB <- c(NA, diff(broiler$lnpb))
broiler$DLP <- c(NA, diff(broiler$lnp))
broiler$DLY <- c(NA, diff(broiler$lny))
broiler$DLQ <- c(NA, diff(log(broiler$q)))

broiler <- broiler[11:50,]
head(broiler); tail(broiler)
nrow(broiler)
#newbroiler$lny <- log(newbroiler$y)


## TODO: why isn't intercept the same?
DLP.reduced.lm <- lm(data=broiler, DLP ~ DLY + DLPB + lnpf + time + 
                           lnqprod_1)
summary(DLP.reduced.lm)




## Part e) calculating predicted value for t = 2000
coef <- summary((DLP.reduced.lm))$coef[,1]
predVal <- -2.167566 + coef[2]*0.0356 + coef[3]*0.0289 + coef[4]*(-0.4818) + 
      coef[5]*51 + coef[6]*9.936
predVal






## Part f) Estimating for lnP not as above DlnP
LP.reduced.lm <- lm(data=broiler, lnp ~ lnpf + time + lnqprod_1 + 
                          DLY + DLPB)
# TODO: Why is the intercept different??? 
summary(LP.reduced.lm)






## Part g) estimating by hand two-stage least squares
demand.structural.lm <- lm(data=broiler, DLQ ~ DLY + DLP.reduced.lm$fit
                        + DLPB)
summary(demand.structural.lm)   # TODO: how to get to not have intercept?
supply.structural.lm <- lm(data=broiler, lnqprod ~ LP.reduced.lm$fit + 
                              lnpf + time + lnqprod_1)
summary(supply.structural.lm)   # TODO: why is intercept different?


# estimating to get correct std.errors
struct <- list(demand = DLQ ~ DLY + DLP + DLPB, 
               supply = lnqprod ~ lnp + lnpf + time + lnqprod_1)
inst <- ~ lnpf + time + lnqprod_1 + DLY + DLPB
two.sls <- systemfit(struct, inst=inst, method="2SLS", data=broiler)
summary(two.sls)




## Part h) reestimating the supply equation using additional instrumental
# variable: log(expts)
inst <- ~ lnpf + time + lnqprod_1 + DLY + DLPB + lnexpts
two.sls <- systemfit(struct, inst=inst, method="2SLS", data=broiler)
summary(two.sls)

# Sargan's Test for surplus (overidentifying) instruments
iv <- ivreg2(form = lnqprod ~ lnp + lnpf + time + lnqprod_1, 
       endog = "lnp", iv = c("lnpf", "time", "lnqprod_1", "DLY",
                             "DLPB", "lnexpts"), 
       data = broiler, digits=7)
