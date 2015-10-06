# method 1
x = c(1:10)
m = matrix(x, nrow=2, byrow=T)
m

# method 2 to create matrices
m = 1:10
dim(m) = c(2, 5)
m

# method 3
x = 1:3
y = 10:12
cbind(x, y)
rbind(x, y)
