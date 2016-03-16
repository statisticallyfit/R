setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learneconometrics/CarterHill_PrinciplesOfEconometrics/Chapter11_SimultaneousEquationsModels")
rm(list=ls())

library(lmtest)
library(foreign)
library(ggfortify)
# Data from: http://www.principlesofeconometrics.com/poe4/poe4stata.htm



truffles <- read.dta("truffles.dta")

# Structural (simultaneous) equations
# QD = P + PS + DI + ed
# QS = P + PF + es

# Doing 2sls for the reduced-form equations
# ... all the variables on right (ps, di, pf) are exogenous, while
# all variables on left (q, p) are endogenous
Q.reduced.lm <- lm(data=truffles, q ~ ps + di + pf)
P.reduced.lm <- lm(data=truffles, p ~ ps + di + pf)
summary(Q.reduced.lm)
summary(P.reduced.lm)

# Now on second part of 2sls, estimating the structural equations
# this is where the stderrors are incorrect (and thus t-stats and pvals)
P.fit <- P.reduced.lm$fitted.values
# quantity demanded model
QD.lm <- lm(data=truffles, q ~ P.fit + ps + di)
summary(QD.lm)
# quantity supplied model
QS.lm <- lm(data=truffles, q ~ P.fit + pf)
summary(QS.lm)



## NOTE: there is a Q.reduced.lm model and also QD.lm and QS.lm. This is
# not confusing since QD and QS are created just depending on what
# types of variables are on their right side. Quantity itself is
# just a variable in the data truffles list. 

install.packages()