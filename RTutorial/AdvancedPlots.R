# Shading
x = c(-1,1,1,-1,-1)
y = c(-1,-1,1,1,-1)
plot(x, y)
polygon(x, y, col='blue')

#

stdev <- 0.75
x <- seq(-5, 5, by=0.01)
y <- dnorm(x, sd=stdev)
right <- qnorm(0.95, sd=stdev)
plot(x, y, type='l', xaxt='n', ylab='p',
     xlab=expression(paste('Assumed Distribution of ', bar(x))),
     axes=FALSE, ylim=c(0, max(y)*1.05), xlim=c(min(x), max(x)),
     frame.plot=FALSE)
axis(1, at=c(-5, right, 0, 5), 
     pos=c(0, 0), 
     labels=c(expression(' '), expression(bar(x)[cr]), 
              expression(mu[0]), expression(' ')))
axis(2)
xReject <- seq(right, 5, by=0.01)
yReject <- dnorm(xReject, sd=stdev)
polygon(c(xReject, xReject[length(xReject)], xReject[1]), 
        c(yReject, 0, 0), 
        col='blue')


# another example
curve(dnorm(x, 0, 1), xlim=c(-3,3), main="Normal Density")
cord.x <- c(-3, -3, -2, -2)
cord.y <- c(0, dnorm(-3), dnorm(-2), 0)
polygon(cord.x, cord.y, col='skyblue')

# better
curve(dnorm(x, 0, 1), xlim=c(-3,3), main="Normal Density")
xcont <- seq(-1, 1, by=0.01)
cord.x <- c(-1, xcont, 1)
cord.y <- c(0, dnorm(xcont), 0)
polygon(cord.x, cord.y, col='skyblue')
