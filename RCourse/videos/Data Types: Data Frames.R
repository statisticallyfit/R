# can store different classes in each column
# made with read.table() or read.csv()
# converted to matrix with data.matrix()
x = data.frame(foo = 1:4, bar=c(T, T, F, F))
x
nrow(x)
ncol(x)
