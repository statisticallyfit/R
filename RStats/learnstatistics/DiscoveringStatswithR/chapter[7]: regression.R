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

album2 = read.delim("data/Album Sales 2.dat", header=TRUE)
head(album2)

album2Model = lm(sales ~ adverts, data=album2)
album2Model 
summary.lm(album2Model)
album3Model = lm(sales ~ adverts + airplay + attract, data=album2)
album3Model
summary.lm(album3Model)
# OR
album3Model = update(album2Model, .~. + airplay + attract)
# Cross-validity of this model is good since adj.Rsquared is 
# close to Rsquared

# lm.beta standardizes parameters, reducing them to stdev units
# and making them easier to compare amongst each other
lm.beta(album3Model) # looks like airplay is most important
confint(album3Model) #interval for attract is wider (worse)


# Comparing Fit of the Models (comparing R^2)
# formula: F = (N - k - 1) R^2 / (k(1 - R^2)), 
# where k = number of predictors, N = number of cases

# First model: (the first model mades R^2 change from 0 to 0.335
# and this change is F1 = 99.59)
album2Model
N = dim(album1)[1]; N
k1 = 1
R.squared1 = summary(album2Model)$r.squared
F1 = (N - k1 - 1) * R.squared1 / (k1*(1 - R.squared1)); F1

# Second model: (the addition of new predictors in the model 3
# makes R^2 increase an additional 0.330 and the F ratio for this
# relative change is = 96.44)
summary.aov(album3Model)
N = dim(album2)[1]; N
k2 = 3
k.change = k2 - k1
R.squared2 = summary(album3Model)$r.squared
R.squared.change = R.squared2 - R.squared1
Fchange = (N - k2 - 1) * R.squared.change / (k.change*(1 - R.squared2)); Fchange
# degrees of freedom are: kchange = 2, N-k2-1 = 200-3-1 = 196
p.value = 1 - pf(Fchange, df1=k.change, df2 = (N-k2-1)); p.value
# The easy way: 
anova(album2Model, album3Model) # model1 must be subset of model2...
# CONCLUDE: model3 is vastly improved compared to model2 



# Testing accuracy of the model
# outliers: resid(), rstandard(), rstudent()
# influentials: cooks.distance(), dfbeta(), hatvalues(), covratio()

album2$resid = resid(album3Model)
album2$stz.r = rstandard(album3Model)
album2$stu.r = rstudent(album3Model)
album2$cooks = cooks.distance(album3Model)
album2$dfbeta = dfbeta(album3Model) # are the model diff without a yvalue
album2$dffit = dffits(album3Model)
album2$lev = hatvalues(album3Model)
album2$covratio = covratio(album3Model)
head(album2)
