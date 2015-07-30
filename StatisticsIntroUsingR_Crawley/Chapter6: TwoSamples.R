setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/StatisticsIntroUsingR_Crawley")

# Comparing Variances


# Fisher's F Test used to compare two variances
# Fisher's Exact Test used do in place of chi square when cells are < 5

# critical value
f.crit <- qf(0.975, df1=9, df2=9)
f.test.data <- read.csv("data/f.test.data.csv")
attach(f.test.data)
f.test.data
f.ratio <- var(gardenC)/var(gardenB)
# since f.ratio > f.crit , reject null that ratio is not 1
p.value <- 2*(1-pf(f.ratio, df1=9, df2=9))

# --> NOTE: since variances are significantly different, don't compare means

#same
var.test(gardenC, gardenB)
var.test(gardenB, gardenC)

detach(f.test.data)





# Comparing Means

#   (1) Student's t Test when samples are independent, variances are constant, and errors are normal
#   (2) Wilcoxon rank-sum test when samples are independent, variances are constant, but errors are not normal
# other tests for when variances are different

t.test.data <- read.csv("data/t.test.data.csv")
attach(t.test.data)
t.test.data
t.crit <- qt(0.975, df=18) # df= n1 + n2-2 = 10 + 10 - 2= 18

ozone <- c(gardenA, gardenB)
label <- factor(c(rep("A", 10), rep("B", 10)))
boxplot(ozone ~ label, notch=TRUE, xlab="Garden", ylab="Ozone pphm", col="cornflowerblue")
# because notches do not overlap, medians are significantly different at the 5% level

# t-test the long way
s2A <- var(gardenA)
s2B <- var(gardenB) 
s2A/s2B # test they are similar
nA <- length(gardenA)
nB <- length(gardenB)
t.statistic <- (mean(gardenA)-mean(gardenB))/(sqrt(s2A/nA + s2B/nB))
p.value <- 2*pt(-abs(t.statistic), df=(nA+nB-2))
p.value

# t-test the short way (Welch two sample t-test)
t.test(gardenA, gardenB)


# Wilcoxon Rank Sum Test: use if errors are not normal 
ozone <- c(gardenA, gardenB)
ozone
label <- factor(c(rep("A", 10), rep("B", 10)))
label
combined.ranks <- rank(ozone)
combined.ranks
# now find sum of ranks for each garden
ranksums <- tapply(combined.ranks, label, sum)
# the smaller of two ranks is the W statistic
W.statistic <- min(ranksums)
W.statistic
W.crit <- qwilcox(0.975, 10, 10)
p.value <- 2* (1-pwilcox(W.statistic, m=10, n=10))
p.value

# this uses normal approximation
# wilcox test is more conservative than t-test and is 95% as powerful with normal errors compared to t test
wilcox.test(gardenA, gardenB, correct=FALSE)
detach(t.test.data)


# Paired Samples
# NOTE: test is more accurate if variances are smaller (page 97)
stream <- read.csv("data/streams.csv")
attach(stream)
stream
t.test(down, up) # assuming independence, there is no sewage outfall impact on biodiversity score
t.test(down, up, paired=TRUE) # but if paired, there is significant impact
t.test(up-down) # test on differences of pairs
