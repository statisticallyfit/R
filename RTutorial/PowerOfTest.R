### TO mull over (confusing since it is finding
# type B error on the original null assumed mean graph)

# Normal Distribution
mu <- 5
s <- 2
n <- 20


# Method 1

# (left, right) is the same as (left x*, right x*s)
marginOfError <- qnorm(0.975) * s/sqrt(n)
marginOfError
xleft <- mu-marginOfError
xright <- mu+marginOfError

# power if assumedMu = 6.5
actualMu <- mu+1.5
zleft <- (xleft-actualMu)/(s/sqrt(n))
zright <- (xright-actualMu)/(s/sqrt(n))
typeII <- pnorm(zright) - pnorm(zleft)
power <- 1-typeII
# show
curve(dnorm(x, mean=6.5, sd=2), xlim=c(0, 13), 
      main="Type II Error")
xcont <- seq(xleft, xright, by=0.01)
cord.x <- c(xleft, xcont, xright)
cord.y <- c(0, dnorm(xcont, mean=6.5, sd=2), 0)
polygon(cord.x, cord.y, col='skyblue')





# t Distribution

# Method 2

#tstatistic (noncentrality parameter)
ncp <- (abs(5-6.5))/(s/sqrt(n)) 
tcrit <- qt(0.975, df=n-1)
typeIIerror = pt(tcrit, df=n-1, ncp=ncp) - 
  pt(-tcrit, df=n-1, ncp=ncp)
power = 1- typeIIerror
pt(tcrit, df=n-1, ncp=ncp)

# --> show
# technicalities
mu <- 5
actualMu <- mu+1.5
s <- 2
n <- 20
marginOfError <- qnorm(0.975) * s/sqrt(n)

xleft <- mu-marginOfError
xright <- mu+marginOfError

tleft <- (xleft-actualMu)/(s/sqrt(n))
tright <- (xright-actualMu)/(s/sqrt(n))
pt(xleft, xright, df=n-1)

#plot
curve(dt(x, df=n-1), xlim=c(4, 7), main="Type II Error")
xcont <- seq(xleft, xright, by=0.01)
cord.x <- c(xleft, xcont, xright)
cord.y <- c(0, dt(xcont, df=n-1), 0)
polygon(cord.x, cord.y, col='yellow')


# Method 3

power.t.test(n=n, delta=1.5, sd=s, sig.level=0.05,
             type="one.sample", 
             alternative="two.sided", 
             strict=TRUE)



# Many powers of a t test

# Method 1
m1 <- c(10,12,30)
m2 <- c(10.5,13,28.5)
sd1 <- c(3,4,4.5)
sd2 <- c(2.5,5.3,3)
num1 <- c(300,210,420)
num2 <- c(230,340,400)
se <- sqrt(sd1*sd1/num1+sd2*sd2/num2)

actualMu <- 1

xleft <- qt(0.025, df=pmin(num1, num2)-1)*se
xright <- -xleft
tleft <- (xleft-actualMu)/se
tright <- (xright - actualMu)/se

typeII <- pt(tright, df=pmin(num1, num2)-1) - 
  pt(tleft, df=pmin(num1, num2)-1)
power <- 1 - typeII


# Method 2
tcrit <- qt(0.975, df=pmin(num1, num2)-1)
ncp <- 1/se
typeII = pt(tcrit, df=pmin(num1, num2)-1, ncp=ncp) - 
  pt(-tcrit, df=pmin(num1, num2)-1, ncp=ncp)
power = 1 - typeII
