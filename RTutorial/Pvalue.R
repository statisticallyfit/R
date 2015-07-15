"  H0: mu  = a
   H1: mu != a  "


# p-value from Normal Distribution

getZscore <- function(xbar, mu, s, n) {
  return ((xbar-mu)/(s/sqrt(n)))
}
getPvalueForTwoSidedTest <- function(z) {
  return (2*pnorm(-abs(z)))
}
getPvalueForOneSidedTest <- function(z) {
  return(getPvalueForTwoSidedTest(z)/2)
}
isNullRejected <- function(pvalue, significancelevel){
  if(pvalue < significancelevel)
    return(TRUE)
  return(FALSE)
}
test <- function(nullIsRejected){
  if(nullIsRejected)
    return ("Reject the null")
  return ("Not enough evidence to reject the null")
}
hypTestNormal <- function(xbar, mu, s, n, twosided){
  z <- getZscore(7, 5, 2, 20)
  if (twosided){
    pvalue <- getPvalueForTwoSidedTest(z)
  } else {
    pvalue <- getPvalueForOneSidedTest(z)
  }
  test(isNullRejected(pvalue, 0.05))
}

### Testing
hypTestNormal(7, 5, 2, 20, FALSE)




# p value from t Distribution
mu <- 5
s <- 2
n <- 20
xbar <- 7
t <- (xbar-mu)/(s/sqrt(n))
pvalue <- 2*pt(-abs(t), df=n-1)
pvalue

#
w1 <- read.csv(file="w1.dat", sep=",", head=TRUE)
summary(w1)
length(w1$vals)

" H0: mu = 0.7
  H1: mu != 0.7 "
t <- (mean(w1$vals)-0.7)/(sd(w1$vals)/sqrt(length(w1$vals)))
t
pvalue = 2*pt(-abs(t), df=length(w1$vals)-1)
pvalue



# Many pvalues from t distribution

" H0: mu1 - mu2 = 0
  H1: mu1 - mu2 != 0 "

# Method 1
m1 <- c(10, 12, 30)
m2 <- c(10.5, 13, 28.5)
sd1 <- c(3, 4, 4.5)
sd2 <- c(2.5, 5.3, 3)
num1 <- c(300, 210, 420)
num2 <- c(230, 340, 400)
se <- sqrt(sd1^2/num1 + sd2^2/num2)
t <- (m1 - m2)/se
t
pt(t, df=pmin(num1, num2)-1) #can take a list of ts and dfs



# Method 2

# ---- One Sample t Test
dataSet <- c(9, 9.5, 9.6, 10.2, 11.6)
# two sided 
t.test(dataSet, mu=10) #setting the H0 mean
# one sided 
t.test(dataSet, mu=10, alternative="less")

# ---- Two Sample t Test
# independent data sets
x1 <- c(9, 9.5, 9.6, 10.2, 11.6)
y1 <- c(9.9, 8.7, 9.8, 10.5, 8.9, 8.3, 9.8, 9)
t.test(x1, y1)
# paired t-test
x2 <- c(9, 9.5, 9.6, 10.2, 11.6)
y2 <- c(9.9, 8.7, 9.8, 10.5, 8.9)
t.test(x2, y2, paired=TRUE)

# two sided, one sided options
