setwd("/datascience/projects/statisticallyfit/github/
      learningprogramming/R/RStats/learneconometrics/
      CarterHill_PrinciplesOfEconometrics/Chapter10_RandomRegressors")
source("/datascience/projects/statisticallyfit/github/
       learningprogramming/R/RStats/learneconometrics/
       CarterHill_PrinciplesOfEconometrics/Chapter10_RandomRegressors/
       Formulas.R")
rm(list=ls())

library(lmtest)
library(foreign)
library(ggfortify)
# Data from: http://www.principlesofeconometrics.com/poe4/poe4stata.htm


## Part a)
brumm <- read.dta("brumm.dta")
brumm.lm <- lm(data=brumm, inflat ~ money + output)
summary(brumm.lm)


## TODO: help for F-test - how to do it? F.stat must be 10.52
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# Testing H0: B1 = 0, B2 = 1, B3 = -1
# Same as H0: B1 = 0, V2 = 0, V3 = 0, where V2=B2-1, V3=B3+1
q <- 3; n <- 76
v2 <- brumm$money - 1
v3 <- brumm$output + 1
brumm.ftest.lm <- lm(data=brumm, inflat ~ v2 + v3)
summary(brumm.ftest.lm)
sse <- SSE(brumm$inflat, brumm.ftest.lm$fitted.values); sse
sst <- SST(brumm$inflat); sst
ssm <- sst - sse; ssm
dfm <- q; dfm
dfe <- n - q; dfe 
MSM <- ssm / dfm; MSM
MSE <- sse / dfe; MSE
# This is the joint hypothesis F-test
F.statistic <- MSM / MSE; F.statistic
F.crit <- qf(0.95, dfm, dfe); F.crit #ATTENTION! dfm first THEN dfe
p.value <- 1 - pf(F.statistic, dfm, dfe); p.value
# This is the single coefficient hypothesis F-test
anova(brumm.ftest.lm) 


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



## Part b) residuals do they have heteroskedasticity?
res.df <- data.frame(res=brumm.lm$residuals, 
                     fits=brumm.lm$fitted.values)
ggplot(data=res.df, aes(x=fits, y=res)) + 
      geom_point(shape=19) + 
      geom_hline(y=0)

# LM Test for heteroskedasticity (page 214)
# H0: homoskedasticity
# H1: heteroskedasticity
e <- brumm.lm$residuals
e2 <- e^2
lagrange.heter.lm <- lm(data=brumm, e2 ~ money)
s <- summary(lagrange.heter.lm); s
R2 <- s$r.squared; R2
N <- 76
NR2 <- N * R2; NR2
X.crit <- qchisq(0.95, df=1); X.crit   # reject homoskedasticity





## Part c) how to get White HAC stderrors?
HACs <- summaryHAC(brumm.lm)
HACs




## Part d) creating IV estimates using 
# 4 instrumentals: INITIAL, SCHOOL, INV, POPRATE
# 1 endogeneous: OUTPUT
brumm.lm
firststage.lm <- lm(data=brumm, output ~ money + initial + 
                          school + inv + poprate)
summary(firststage.lm)
output.fits <- firststage.lm$fitted.values
secondstage.lm <- lm(data=brumm, inflat ~ money + output.fits)
summary(secondstage.lm)





## Part f) Hausman test for endogeneity (robust to heteroskedasticity)
# (but mine is not robust to it)

firststage.lm <- lm(data=brumm, output ~ money + initial + school
                    + inv + poprate)
summary(firststage.lm)
secondstage.lm <- lm(data=brumm, inflat ~ money + output + 
                           firststage.lm$residuals)
summary(secondstage.lm)


