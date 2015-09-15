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

# Autocorrelations
#ausbeer = read.table("data/beer.csv", dec=",", header=TRUE)
#ausbeer.ts = ts(ausbeer, start=1956, end=c(1995, 8), freq=12)
#ausbeer2.ts = window(ausbeer.ts, start=1992) #, end=2006-0.1)
beer2 = window(ausbeer, start=1992, end=2006-0.1)
lag.plot(beer2, lags=9, do.lines=FALSE)
autoplot(acf(beer2, plot=FALSE))

# White noise
set.seed(30)
x = ts(rnorm(50))
autoplot(x, main="White noise", xlab="Time", ylab="Normal values")

# Acf for white noise
autoplot(acf(x, plot=FALSE))




# 2.3. Simple forecasting methods

# y = contains the time series, h = forecast horizon

# Average method: 
    # meanf(y, h) 
  # for cross sectional and time series data
# Naive method: 
    # naive(y, h)
  # the forecast is the last observation in the time series
# Seasonal naive method: 
    # snaive(y, h)
  # forecast is the last observation from same season
  # of the year (meant for highly seasonal data)
# Drift method: 
    # rwf(y, h, drift=TRUE)
  # like extrapolating the average of the first and last 
  # observation (called drift) into the future

# Examples
beerfit1 = meanf(beer2, h=11)
beerfit2 = naive(beer2, h=11)
beerfit3 = snaive(beer2, h=11)

autoplot(beerfit1, main="Forecasts for quarterly beer production")
lines(beerfit2$mean, col=2)
