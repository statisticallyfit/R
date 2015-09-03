library(ggplot2)
library(ggfortify)
library("TTR")

# 2.2 Reading Data
kings = scan("http://robjhyndman.com/tsdldata/misc/kings.dat", skip=3)
kings.ts = ts(kings)
# freq=number of times per year the data was collected
kings.ts
autoplot(kings.ts)

# 2.3 Plotting Time Series
births = scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")
births.ts = ts(births, freq=12, start=c(1946,1))
births.ts
autoplot(births.ts)

souvenir = scan("http://robjhyndman.com/tsdldata/data/fancy.dat")
souvenir.ts = ts(souvenir, freq=12, start=c(1987, 1))
souvenir.ts
autoplot(souvenir.ts)
# seasonal increases with time so must do log transform
logsouvenir.ts = log(souvenir.ts)
autoplot(logsouvenir.ts)


# 2.4 Decomposing TIme Series

# -- decomposing non-seasonal data

# do smoothing to find simple moving average of non seasonal additive time series
kingsSMA3.ts = SMA(kings.ts, n=3) # order=3
kingsSMA3.ts
autoplot(kings.ts)
autoplot(kingsSMA3.ts) # this has extracted the trend
# using higher order gives better smoothing, less randomness
kingsSMA8.ts = SMA(kings.ts, n=8)
autoplot(kingsSMA8.ts)

# -- decomposing seasonal data

autoplot(births.ts)
birthscomponents.ts = decompose(births.ts)
birthscomponents.ts$seasonal
autoplot(birthscomponents.ts)

# -- seasonally adjusting
birthseasonallyadjusted.ts = births.ts - birthscomponents.ts$seasonal
autoplot(birthseasonallyadjusted.ts)
autoplot(birthscomponents.ts$trend)



# 2.5 Forecasts using exponential smoothing