x = list(foo=1:4, bar=0.6, baz="hello")
x
x[1]
x[[1]]
x$bar
x[["bar"]]
x["bar"]
x["bar"]$bar

x[c(1,3)]

name = "foo"
x[[name]]
x[name]
x$name # element name doesn't exist
x$foo # element foo does exist
x[name]$foo


# subsetting nested elements of a list
# the [[]] can take an integer sequence
x = list(a = list(10, 12, 14), b=c(3.14, 2.81))
x
x[[c(1, 3)]]
x$a[[3]]
x[[1]][[3]]

x[[c(2,1)]]
x[c(2,1)]
