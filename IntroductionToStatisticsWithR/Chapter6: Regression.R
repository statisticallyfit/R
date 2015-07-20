library(ISwR)
attach(thuesen)

# SCATTER PLOT OF DATA AND SLOPE TEST
lm.velo = lm(short.velocity~blood.glucose) # y ~ x
summary(lm.velo)
plot(blood.glucose, short.velocity); abline(lm.velo)


# RESIDUALS AND FITS
fits = fitted(lm.velo)
resids = resid(lm.velo)
resids

# short.velocity has 1 missing observation, so plot only the observations 
# of fits that exist in short.velocity
lines(blood.glucose[!is.na(short.velocity)], fits)
# OR use:
cc <- complete.cases(thuesen) #attach(thuesen[cc,])
# OR use: 
options(na.action=na.exclude)
lm.velo <- lm(short.velocity~blood.glucose)
fits = fitted(lm.velo)

# regression line, observations, and residual segments
plot(blood.glucose, short.velocity)
segments(blood.glucose, fitted(lm.velo), 
         blood.glucose, short.velocity)
lines(blood.glucose, fits)

# residual plot: both work
plot(fitted(lm.velo), resid(lm.velo))
plot(blood.glucose, resid(lm.velo))

# check for normality of residuals
qqnorm(resid(lm.velo))
hist(resid(lm.velo))



# PREDICTION AND CONFIDENCE BANDS
predict(lm.velo) && fitted(lm.velo)
predict(lm.velo, interval="confidence") #or interval="prediction
predict(lm.velo, interval="prediction")

# left off at page 131