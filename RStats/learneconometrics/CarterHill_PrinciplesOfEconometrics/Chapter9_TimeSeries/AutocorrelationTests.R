#install.packages("fUnitRoots")
# This package's adfTest does not detrend the time series before testing, like
# the adf.test does

setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learneconometrics/CarterHill_PrinciplesOfEconometrics")
rm(list=ls())

www <- "http://www.econometrics.com/comdata/hill4/okun.dat"
okun <- read.table(www, header=TRUE)
head(okun)

# Method 1 of creating the time vector (hard way)
# make col1 - time
years <- seq(1985, 2009)
years <- years[2:24]

time <- rep(1985, 3)
for (y in years) {time <- append(time, rep(y, 4))}
time <- append(time, rep(2009, 3))
time # col1

# make col2 - Ut and its companions
U_1 <- c(NA, okun$U[1:97]); U_1
DU <- c(NA, diff(okun$U))
G_1 <- c(NA, okun$G[1:97]); G_1
G_2 <- c(NA, NA, okun$G[1:96]); G_2

okunData <- data.frame(Quarter=time, Ut=okun$U, DU=DU, Ut_1=U_1, Gt=okun$G, 
                       Gt_1=G_1, Gt_2=G_2)
head(okunData); tail(okunData)


# Method 2 of creating time vector (easy way)
# uses rolling shift provided by lag() method
okun.ts <- ts(okunData$DU, start=1985, frequency=4)
okun.ts <- lag(okun.ts, -1)
okun.ts
okun.ts <- na.omit(okun.ts)
okun.ts


# Calculate autocorrelation coefficients for Gt
g <- okunData$Gt 
c0 <- 1/98 * sum( (g[1:98] - mean(g)) * (g[1:98] - mean(g)) )
c1 <- 1/98 * sum( (g[1:97] - mean(g)) * (g[2:98] - mean(g)) )
c2 <- 1/98 * sum( (g[1:96] - mean(g)) * (g[3:98] - mean(g)) )
c3 <- 1/98 * sum( (g[1:95] - mean(g)) * (g[4:98] - mean(g)) )
c4 <- 1/98 * sum( (g[1:94] - mean(g)) * (g[5:98] - mean(g)) )
c5 <- 1/98 * sum( (g[1:93] - mean(g)) * (g[6:98] - mean(g)) )
c6 <- 1/98 * sum( (g[1:92] - mean(g)) * (g[7:98] - mean(g)) )

r1 <- c1/c0; r2 <- c2/c0; r3 <- c3/c0; r4 <- c4/c0
r5 <- c5/c0; r6 <- c6/c0
r1; r2; r3; r4; r5; r6

# Test significance of autocorrelation coefficients
# Z = (rk - 0) / sqrt(1/T)
z1 <- r1*sqrt(98); z1                 # z-statistic
t1 = r1 / sqrt((1 - r1^2)/(98-2)); t1 # t-statistic
cor.test(g[1:97], g[2:98])            # so autocorcoef is NOT same as corcoef

z2 <- r2*sqrt(98)
z3 <- r3*sqrt(98)
z4 <- r4*sqrt(98)
z5 <- r5*sqrt(98)
z6 <- r6*sqrt(98)

p1 <- 1 - pnorm(z1)
p2 <- 1 - pnorm(z2)
p3 <- 1 - pnorm(z3)
p4 <- 1 - pnorm(z4)
p5 <- 1 - pnorm(z5)
p6 <- 1 - pnorm(z6)
tbl <- data.frame(R=round(c(r1,r2,r3,r4,r5,r6), 5),
                  Z=round(c(z1, z2,z3,z4,z5,z6), 5), 
                  P=round(c(p1,p2,p3,p4,p5,p6), 5)); tbl




# =============================================================================

# METHODS TO TEST FOR AUTOCORRELATION:

# 1. CORRELOGRAM (for acf to work, must be just 1 variable in .ts, alongside time)

# @todo: why when lag.max=12 does graph show only 3 lags?
# Equation: DU ~ Gt + Gt_1 + Gt_2
autoplot(acf(okun.ts, lag.max=12, plot=FALSE)) 
autoplot(acf(okun.ts, lag.max=98/2, plot=FALSE)) 
# WARNING: this is the acfs of Gt, not of y or residuals, so is not same as r1...
okun.acf <- acf(okun.ts, lag.max=46, plot=FALSE) #lag.max=98/2 (halfish num of obs)
okun.acf


4# 2. LAGRANGE MULTIPLIER TEST
www <- "http://www.econometrics.com/comdata/hill4/phillips_aus.dat"
phillips <- read.table(www, header=TRUE)
head(phillips)
tail(phillips)

# add a DU column
phillips$DU <- c(diff(phillips$u), NA)

inf.ts <- ts(phillips$inf, start=1987, frequency = 4)
u.ts <- ts(phillips$u, start=1987, frequency = 4)
du.ts <- na.omit(ts(phillips$DU, start=1987, frequency = 4))

autoplot(inf.ts)
autoplot(u.ts)
autoplot(du.ts)
 
# Equation: INF ~ DU 
inf.lm <- lm(data=phillips, inf ~ DU)  #todo: why different than book? (page 352)
summary(inf.lm)


# Calculate autocorrelation coefficients for residuals from INF ~ DU
res <- inf.lm$residuals
c0 <- 1/90 * sum( (res[1:90] - mean(res)) * (res[1:90] - mean(res)) )
c1 <- 1/90 * sum( (res[1:89] - mean(res)) * (res[2:90] - mean(res)) )
c2 <- 1/90 * sum( (res[1:88] - mean(res)) * (res[3:90] - mean(res)) )
c3 <- 1/90 * sum( (res[1:87] - mean(res)) * (res[4:90] - mean(res)) )
c4 <- 1/90 * sum( (res[1:86] - mean(res)) * (res[5:90] - mean(res)) )
c5 <- 1/90 * sum( (res[1:85] - mean(res)) * (res[6:90] - mean(res)) )
c6 <- 1/90 * sum( (res[1:84] - mean(res)) * (res[7:90] - mean(res)) )

# why aren't the r1.. exactly equal to the ones in the book?
r1 <- c1/c0; r2 <- c2/c0; r3 <- c3/c0; r4 <- c4/c0; r5 <- c5/c0; r6 <- c6/c0
r1; r2; r3; r4; r5; r6

autoplot(acf(res, plot=FALSE), ylab = "residual autocorrelations")
phillips.acf <- acf(res, plot=FALSE)
phillips.acf

# scatterplot
ggplot(data=phillips, aes(x=DU, y=inf)) + geom_point(shape=19) + geom_smooth(method="lm")
phillipsRes <- data.frame(du=na.omit(phillips$DU), resids=res)
# residual plot
ggplot(phillipsRes, aes(x=du, y=resids)) + geom_point(shape=19)


