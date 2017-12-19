# objects can have names
x = 1:3
names(x)
names(x) = c("foo", "bar", "norf")
x

# lists can have names
x = list(a=1, b=2, c=3)
x

# matrices can have names
m = matrix(1:4, nrow=2)
dimnames(m) = list(c("a", "b"), c("c", "d"))
m
