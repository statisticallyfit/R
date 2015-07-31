# EXAMPLE
reg.data <- read.csv("data/tannin.csv")
attach(reg.data)
reg.data
plot(tannin, growth, pch=19, col="cyan")
lm(growth~tannin)
abline(lm(growth~tannin, col="dodgerblue"))

fitted <- predict(lm(growth~tannin))
fitted
lines(c(0,0), c(12, fitted[1]))
for(i in 1:length(fitted))
  lines(c(tannin[i], tannin[i]), c(growth[i], fitted[i]), col="red")

# Looping through values to extract best estimate of b
b <- seq(-1.43, -1, 0.002)
sse <- numeric(length(b)) # sum of squared residuals
for(i in 1: length(b)){
  a <- mean(growth) - b[i]*mean(tannin)  
  residual <- growth - a - b[i]*tannin
  sse[i] <- sum(residual^2)
}
plot(b, sse, type="l", ylim=c(19, 24)) 
      arrows(-1.216, 20.07225, -1.216, 19, col="red")
      abline(h=20.07225, col="green", lty=2)
      lines(b, sse)
b[which(sse==min(sse))]




# Doing calculation of b by hand

# b = SSXY/SSX
# a = meany - b*meanx
# SSR = SSXY^2/SSX
# r = SSXY/sqrt(SSX*SSY)

sumXY = sum(tannin*growth)
sumXsumY = sum(tannin)*sum(growth)
sumX2_individ = sum(tannin^2)
sumX2_all = (sum(tannin))^2
SSXY = (sumXY - sumXsumY/n)
SSX = (sumX2_individ - sumX2_all/n)
n = length(tannin)
b = SSXY/SSX
a = mean(growth) - b*mean(tannin)

# ANOVA to find standard errors of slope and intercept
# SSY = SSE + SSR
# SSR = b*SSXY
SSY = (sum(growth^2) - (sum(growth))^2/n)
SSR = b*SSXY # explained variation
SSE = SSY - SSR # variation that is unexplained by the LSRL
# df of SSY = n-1 = 8
# df of SSE = n-2 = 7
# df of SSR = 1 
varY = SSY/8
varE = SSE/7
varR = SSR/1
# F ratios - is varR significantly greater than varE? (is there much more explained variance than unexplained variance?)
f.ratio <- varR/varE
f.ratio
1-pf(f.ratio, df1=1, df2=7) # yes


# Standard error of slope
# SEb = sqrt(varE/SSX), where varE = SSE/(n-2)
