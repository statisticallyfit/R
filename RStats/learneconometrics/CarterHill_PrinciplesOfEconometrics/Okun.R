setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learneconometrics/CarterHill_PrinciplesOfEconometrics")

www <- "http://www.econometrics.com/comdata/hill4/okun.dat"
okun <- read.table(www, header=TRUE)
head(okun)

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

okunData <- data.frame(Quarter=time, Ut=okun$U, Ut_1=U_1, Gt=okun$G, 
                       Gt_1=G_1, Gt_2=G_2)
head(okunData); tail(okunData)

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

