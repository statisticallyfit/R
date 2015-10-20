# SPLIT takes vector and splits into groups with factor or factor list
# so it's like tapply, just doesn't apply the function to the groups
str(split)
# x
# f - factor
# drop = whether empty factor levels should be dropped
# always returns a list

x <- c(rnorm(10), runif(10), rnorm(10, 1)); x
f <- gl(3, 10); f
list <- split(x, f)
sapply(split(x, f), mean)
tapply(x, f, mean)


# Splitting a Data Frame:
library(datasets)
head(airquality)

# Find the mean within each month
s <- split(airquality, airquality$Month); s
sapply(s, function(x) colMeans(x[, c("Ozone", "Solar.R", "Wind")], na.rm=TRUE))



# Splitting on More than one Level: 
x <- rnorm(10); 
x
f1 <- gl(2, 5); f1
f2 <- gl(5, 2); f2
interaction(f1, f2) # total combination is 10 levels

split(x, list(f1, f2))
str(split(x, list(f1, f2)))
str(split(x, interaction(f1, f2)))
str(split(x, list(f1, f2), drop=TRUE))
