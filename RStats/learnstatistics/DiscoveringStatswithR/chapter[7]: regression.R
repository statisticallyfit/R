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
