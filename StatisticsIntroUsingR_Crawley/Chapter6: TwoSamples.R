# to compare tests: statpages.org/ctab2x2.html
# good resource: http://www.stat.wisc.edu/~st571-1/06-tables-4.pdf
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


# Binomial Test: 
# how likely is it to have 8 of 9 better on the new regime if there is no difference in the training regimes?
binom.test(1,9)
binom.test(8,9)$p.value 
#conclude that new regime is better than old one

p.value <- 2*(1-pbinom(7, size=9, prob=0.5))
p.value


# Binomial Test for Two Proportions
prop.test(c(4, 196), c(40, 3270)) # 4/40 women and 196/3270 men promoted - significant difference?



# Chisquared Test
chi.crit <- qchisq(0.95, df=1) # chi squared tests are always right-tailed
chi.crit
count <- matrix(c(38, 14, 11, 51), nrow=2)
rownames(count) <- c("Fair", "Dark")
colnames(count) <- c("Blue", "Brown")
count
res <- chisq.test(count, correct=F)
res
res$observed
res$expected




# Fisher's Exact Test: for 2x2 tables + when one or more expected frequencies <5
# f.exact.statistic = the probability of any one particular outcome
# formula = (a+b)!(c+d)!(a+c)!(b+d)!/(a!b!c!d!n!)

# first make the table:
situation1 <- matrix(c(6,2,4,8), byrow=T, nrow=2)
s1 <-situation1
situation1 <- cbind(s1, c(sum(s1[1,]), sum(s1[2,])))
situation1
x <- c("Tree A", "Tree B")
colnames(situation1) = c(x, "    | ROW TOTALS")
situation1
s1 <-situation1
situation1 <- rbind(s1, c(sum(s1[,1]), sum(s1[,2]), sum(s1[,3])))
rownames(situation1) = c("With ants", "Without ants", "COL TOTALS |")
situation1


makeTable <- function(values, colNames, rowNames){
  table <- matrix(args, byrow=T, nrow=2)
  
  table <- cbind(table, c(sum(table[1,]), sum(table[2,])))
  colnames(table) = c(colNames, "ROW TOTALS")
  
  table <- rbind(table, c(sum(table[,1]), sum(table[,2]), sum(table[,3])))
  rownames(table) = c(rowNames, "COL TOTALS")
  
  table
}


# Method 1 for pvalue
situation1 <- makeTable(c(6,2,4,8), c("Tree A", "Tree B"), c("With ants", "Without ants"))



num <- factorial(8)*factorial(12)*factorial(10)*factorial(10)
p1 <- num/(factorial(6)*factorial(4)*factorial(2)*factorial(8)*factorial(20))
p1

# more extreme case: there could have been 1 tree with ants, so the other numbers are shifted

p2 <- num/(factorial(7)*factorial(1)*factorial(3)*factorial(9)*factorial(20))
p2

# the last extreme case: no ants on tree B
p3 <- num/(factorial(8)*factorial(2)*factorial(10)*factorial(20))
p3
p.value = 2*(p1+p2+p3) # doubled since it could have been tree A with fewer ants
p.value

fisher.test(situation1)


# Method 2 for p value
dhyper()

