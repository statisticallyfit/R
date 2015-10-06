x = list(aardvark = 1:5)
x$a
x[["a"]]
x["a"] # not correct

x[["a", exact=FALSE]]
