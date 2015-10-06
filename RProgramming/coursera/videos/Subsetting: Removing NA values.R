# Vector NA removal
x = c(1, 2, NA, 4, NA, 5)
bad = is.na(x)
x[!bad]

# Complete cases - vectors
x = c( 1,   2,  NA,  4,  NA,  5,  NA)
y = c("a", "b", NA, "d", NA, "f", "g")
good = complete.cases(x, y)
good
x[good]
y[good]

# Complete cases - data frames
airquality[1:6, ]
good = complete.cases(airquality)
airquality[good, ][1:6, ]
