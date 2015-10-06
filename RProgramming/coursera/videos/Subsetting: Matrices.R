x = matrix(1:6, nrow=2)
x
x[1, 2]
x[2, 1]
x[1, ]
x[, 2]

x[1, 2] # type is not matrix, it's vector
x[1, 2, drop=FALSE]
x[1, , drop=FALSE]
x[ ,2, drop=FALSE]
