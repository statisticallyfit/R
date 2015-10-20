# APPLY: to evaluate function over margins of an array
      # to rows or cols of matrix
      # can be used with general arrays
      # not faster than loop
str(apply)
# x = vector, MARGIN = integer vector showing which margins are kept

x <- matrix(rnorm(200), 20, 10)
x
# MARGIN = 2 means work on second dimension (cols)
apply(x, 2, mean)

f <- function(x) {
      colMeanList <- vector()
      for(col in seq_len(ncol(x))){
            colMeanList <- c(colMeanList, mean(x[, col]))
      }
      colMeanList
}
identical(f(x), apply(x, 2, mean), colMeans(x))

# MARGIN = 1 means work on first dimension (rows)
apply(x, 1, sum)
identical(apply(x, 1, sum), rowSums(x))



# rowSums = apply(x, 1, sum)
# rowMeans = apply(x, 1, mean)
# colSums = apply(x, 2, sum)
# colMeans = apply(x, 2, mean)

# Find quantiles of the rows of a matrix
x <- matrix(rnorm(200), 20, 10)
x
quantiles <- data.frame(apply(x, 1, quantile, probs=c(0.25, 0.75)))
quantiles$X1 # row 1 with these quantiles
quantiles$X2 # row 2 has these quantiles


# Three-dimensional array
a <- array(rnorm(2*2*10), c(2,2,10))
a
# Find means of all the 2by2 matrices
apply(a, c(1,2), mean) # telescoped (2 by 2 matrix)
rowMeans(a, dims=2)

# Find mean of each matrix
apply(a, 3, mean) # 2 by 2 matrix

# Find row means of each of the 10 matrices
apply(a, c(1,3), mean) # 2 by 10 matrix
a
(1.07785 + 0.216637)/2
(-1.198974 + 0.143087)/2

# Find col means for each of the 10 matrices
apply(a, c(2,3), mean) # 2 by 10 matrix
a
(1.07785 - 1.198974)/2
(0.216637 + 0.143087)/2
