# q11
data <- read.csv("data/hw1_data.csv", header=TRUE)
class(data)
head(data)

# q11
data[1:2,]

#q13
dim(data)
r <- dim(data)[1]

# q14
data[(r-1):r,]

# q15
data$Ozone[47]

# q16
sum(is.na(data$Ozone))

# q17
mean(data$Ozone, na.rm=TRUE)

# q18
sub <- subset(data, Ozone > 31 & Temp > 90)
mean(sub$Solar.R)

# q19
month6 <- subset(data, Month==6)
month6
mean(month6$Temp)

# q20
month5 <- subset(data, Month==5)
month5
max(month5$Ozone, na.rm=TRUE)
