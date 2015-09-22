library(ggplot2)
library(boot)
library(car)
library(QuantPsyc)

#install.packages("QuantPsyc")

getwd()
setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learnstatistics/DiscoveringStatswithR")

# ------ Example 1 ------
album1 = read.delim("data/Album Sales 1.dat", header=TRUE)
head(album1)

album1Model = lm(sales ~ adverts, data=album1)
summary(album1Model)
summary.lm(album1Model) # same thing
summary.aov(album1Model)
r = cor(album1$sales, album1$adverts); r

# ------ Example 2 ------
pubsData = read.delim("data/pubs.dat", header=TRUE)
pubsData

pubsModel = lm(mortality ~ pubs, data=pubsData)
summary.lm(pubsModel)
summary.aov(pubsModel)

ggplot(data=pubsData, aes(x=pubs, y=mortality)) + 
  geom_point(shape=19) + 
  geom_smooth(method="lm", lwd=1)

# ------ Example 3 ------
dfbetaData = read.table("data/dfbeta.dat", header=TRUE)
dfbetaData

# data with influential point
dfbetaModel = lm(Y ~ X, data=dfbetaData)

ggplot(data=dfbetaData, aes(x=X, y=Y)) +
  geom_point(shape=19) + 
  geom_smooth(method="lm", lwd=1)

summary.lm(dfbetaModel)
summary.aov(dfbetaModel)

adj.pred.value = 29 - 0.90*1; adj.pred.value
adj.pred.parameter = -0.90
adj.pred.intercept = 29

# data without influential point
dfbetaDataClean = dfbetaData[(dfbetaData$CASE != 30),]

dfbetaModelClean = lm(Y ~ X, data=dfbetaDataClean)

ggplot(data=dfbetaDataClean, aes(x=X, y=Y)) + 
  geom_point(shape=19) + 
  geom_smooth(method="lm", lwd=1)

summary.lm(dfbetaModelClean)
summary.aov(dfbetaModelClean)

original.pred.value = 31 - 1*(1); original.pred.value
original.pred.parameter = -1
original.pred.intercept = 31

# influence statistics 
DFFit = adj.pred.value - original.pred.value; DFFit

DFBeta.slope = adj.pred.parameter - original.pred.parameter; DFBeta.slope
DFBeta.intercept = adj.pred.intercept - original.pred.intercept; DFBeta.intercept



# Cross-validation: assessing accuracy of model for different samples
# METHODS: 
# 1. Adjusted R^2: variance in Y that would be explained if the model 
#     had been derived from the population
# 2. Data splitting: split data randomly into halves and make new 
#     regression models and compare their R^2 and b-values