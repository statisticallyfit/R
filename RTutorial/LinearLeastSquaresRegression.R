year <- c(2000 ,   2001  ,  2002  ,  2003 ,   2004)
rate <- c(9.34 ,   8.50  ,  7.62  ,  6.93  ,  6.60)

plot(year, rate, 
     main="Commercial Bank Interest Rate for 4-Year Car Loan", 
     sub="http://www.federalreserve.gov/releases/g19/20050805/")
abline(fit)
cor(year, rate)

#finding the LSRL
fit <- lm(rate ~ year)
fit
summary(fit)
attributes(fit)
fit$coefficients
fit$coefficients[1] #title as well
fit$coefficients[[1]] #just the value
interestRateEst = fit$coefficients[[2]]*2015 + fit$coefficients[[1]]

# Residuals, method 1
residuals <- rate - interestRateEst
plot(year, residuals)
#method 2
residuals(fit)
#method 3
fit$residuals
plot(year, fit$residuals)

