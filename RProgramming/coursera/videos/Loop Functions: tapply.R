# TAPPLY splits a vector into groups and applies function to them
str(tapply)
# x = vector
# INDEX = factor/factor list
# FUN, ... = function and its arguments
# simplify = like sapply

x  <- c(rnorm(10), runif(10), rnorm(10, 1)); x
f <- gl(3, 10); f

# Find group means
tapply(x, f, mean)

# Find group ranges
tapply(x, f, range, simplify=FALSE)
tapply(x, f, range)

