setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learneconometrics/CarterHill_PrinciplesOfEconometrics/Chapter10_RandomRegressors")
rm(list=ls())

library(lmtest)
library(foreign)
library(ggfortify)
# Data from: http://www.principlesofeconometrics.com/poe4/poe4stata.htm


## Using just mother educ as instrumental variable
mroz <- read.dta("mroz.dta")
mroz <- mroz[1:428, ]

cor(mroz$mothereduc, mroz$educ) # see? positive correlation -> endogeneity
original.lm <- lm(data=mroz, lnw ~ educ + exp + exp2)

exp <- mroz$exper; exp2 <- exp^2
firststage.m.lm <- lm(data=mroz, educ ~ exp + exp2 + mothereduc)

lnw <- log(mroz$wage)
educFits <- firststage.m.lm$fitted.values; length(educFits)
secondstage.m.lm <- lm(lnw ~ educFits + exp + exp2)

# Since there was positive correlation, the previous original.lm overestimates
# the slope on EDUC, and this real one in secondstage.lm is lower. 
# Also the stderror of EDUC below is 2.5 times larger than in original.lm
# which shows that the IV/2SLS estimator is not efficient. 
summary(original.lm)
summary(firststage.m.lm)
summary(secondstage.m.lm)




## Adding father's educ as instrumental variable

firststage.fm.lm <- lm(data=mroz, educ ~ exp + exp2 + mothereduc + fathereduc)
summary(firststage.fm.lm)

educFits <- firststage.fm.lm$fitted.values
secondstage.fm.lm <- lm(lnw ~ educFits + exp + exp2)
summary(secondstage.m.lm)
summary(secondstage.fm.lm)
