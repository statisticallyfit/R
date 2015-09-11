#install.packages("fpp")
install.packages("GGally")

library(fpp)
library(ggplot2)
library(ggfortify)
library(GGally)

melsyd
autoplot(melsyd[, "Economy.Class"], main="Economy class 
         passengers: Melbourne-Sydney", xlab="Year", 
         ylab="Thousands")
a10
autoplot(a10, ylab="$ million", xlab="Year",
         main="Antidiabetic drug sales")


#" TIME SERIES PATTERNS:

#Trend: long-term increase or decrease in data
#Seasonal pattern: have fixed/known length
#Cycle: has variable/unknown length;
#data rises and falls due to economic conditions

#* cycle is longer and more variable than seasonal"



# PLOTS FOR SEASONAL PATTERNS

# 1. Seasonal plot
seasonplot(a10, ylab="$ million", xlab="Year", 
           main="Seasonal Plot: antidiabetic drug sales", 
           year.labels=TRUE, year.labels.left=TRUE,
           col=rainbow(16), pch=19)

# 2. Seasonal subseries plots
monthplot(a10, ylab="$ million", xlab="Month", xaxt="n", 
          main="Seasonal deviation plot: antidiabetic drug sales")
axis(1, at=1:12, labels=month.abb, cex=0.8)


# TO SEE RELATION in CROSS-SECTIONAL DATA (not time series)

# 1. Scatter plot
head(fuel)
class(fuel)
ggplot(data=fuel, aes(x=jitter(fuel[,5]), y=jitter(fuel[,8]))) + 
  geom_point(size=3) + xlab("City mpg") + ylab("Carbon footprint")

# 2. Scatterplot matrices
# y value is given by variable on the row, and x-value is given by
# variable on the column. 
pairs(fuel[, -c(1:2, 4, 7)], pch=19)
ggpairs(fuel[, -c(1:2, 4, 7)])


# Statistics
fuel2 = fuel[fuel$Litres < 2, ]
summary(fuel2[, "Carbon"])
summary(fuel[, "Carbon"])
sd(fuel2[, "Carbon"])
sd(fuel[, "Carbon"])
