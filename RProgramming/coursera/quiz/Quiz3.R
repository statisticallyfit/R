library(datasets)
data(iris)

# lapply, sapply, mapply, apply, tapply, split

head(iris)

# 1
str(tapply)
?tapply

tapply(iris$Sepal.Length, "virginica", mean)
colMeans(subset(iris, Species=="virginica", Sepal.Length))

mean(iris$Sepal.Length[which(iris$Species == "virginica")])
tapply(iris)

# 2
apply(iris[, 1:4], 2, mean)

# 3
data(mtcars)
head(mtcars)
?split
sapply(split(mtcars$mpg, mtcars$cyl), mean)
tapply(mtcars$mpg, mtcars$cyl, mean)

# 4
avgs <- tapply(mtcars$mpg, mtcars$cyl, mean)
abs(avgs[1] - avgs[3])
