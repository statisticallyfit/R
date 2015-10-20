# LAPPLY: returns a list
str(lapply)
str(sapply)

x <- list(a = 1:5, b = rnorm(10))
lapply(x, mean)
class(x)
x

x <- 1:4
lapply(x, runif) # creates random variables

x <- 1:4
lapply(x, runif, min=0, max=10) # they are between 0 and 10


# anonynmous functions example: to extract first column
x <- list(a = matrix(1:4, nrow=2, ncol=2), b=matrix(1:6, nrow=3, ncol=2))
x
lapply(x, function(elt) elt[,1]) # anonymous functions


# SAPPLY: 
      # if result is list with elements length 1, then vector is returned
      # if result is list with elements length > 1, matrix is returned
      # if it can't shorten, a list is returned
x <- list(a = 1:4, b = rnorm(10), c = rnorm(20, 1), d=rnorm(100,5))
x
lapply(x, mean)
sapply(x, mean)
class(lapply(x, mean))
class(sapply(x, mean))
