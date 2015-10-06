x = factor(c("yes", "yes", "no", "yes", "no"))
x # no is first because it was in alphabetical order
# no is now the baseline level

# to make yes the baseline: 
levels(x) = c("yes", "no")
x

table(x)
unclass(x)
