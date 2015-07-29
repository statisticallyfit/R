y <- c(13,7,5,12,9,15,6,11,9,7,12)
plot(y, ylim=c(0, 20), pch=19, col="dodgerblue")

# measures of variation
# 1
range(y)
#plot(1:11, y, ylim=c(0, 20), pch=19, col="blue")
lines(c(4.5, 4.5), c(5, 15), col="brown")
lines(c(3.5, 4.5), c(5,5), col="brown", lty=2)
lines(c(4.5, 5.5), c(15, 15), col="brown", lty=2)
# 2
# use deviations from mean
plot(y, ylim=c(0, 20), pch=19, col="hotpink")
abline(h=mean(y), col="green")


drawSegments <- function(points, segColor) {
  for(i in 1:11) 
    lines(c(i, i), 
          c(mean(y), y[i]),
          col=segColor)  
}
drawSegments(y, "mediumpurple2")


# Example
ozone <- read.csv("data/gardens.csv")
attach(ozone)
ozone
sum((gardenA - mean(gardenA))^2)/(length(gardenA)-1)
var(gardenA)
var(gardenC)

# F-test: are the variances significantly different?
f.ratio <- var(gardenC)/var(gardenA)
2*pf(f.ratio, df1=9, df2=9, lower.tail=FALSE)
# OR
var.test(gardenA, gardenC)




# Variance and Sample size relationship
plot(c(0, 32), c(0, 15), type="n", 
     xlab="Sample size", ylab="Variance", 
     pch=4)
# type="n" means withold plotting (until you add the points)

for(n in seq(from=3, to=31, by=2)){
  for(i in 1:30){
    x <- rnorm(n, mean=10, sd=2)
    points(n, var(x))
  }
}


