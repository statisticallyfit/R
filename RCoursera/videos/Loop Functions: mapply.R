# multivariate form of apply: applies function in parallel over args
str(mapply)
# FUN
# ... args to apply over
# MoreArgs: list of other args to FUN

list(rep(1, 4), rep(2,3), rep(3,2), rep(4,1))
mapply(rep, 1:4, 4:1)


#
noise <- function(n, mean, sd){
      rnorm(n, mean, sd)
}
noise(5, 1, 2)
noise(1:5, 1:5, 2) # try to get 1 variable of mean 1, sd 2, 2 vars with mean 2 and sd 2

s1 <- mapply(noise, 1:5, 1:5, 2); s1
s2 <- list(noise(1,1,2), noise(2,2,2), 
     noise(3,3,2), noise(4,4,2), 
     noise(5,5,2))


mapply(noise, 1:5, 5:1, 2)
