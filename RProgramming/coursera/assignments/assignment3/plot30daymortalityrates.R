library(ggplot2)
setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RProgramming/coursera/assignments/assignment3")

outcome <- read.csv("data/outcome-of-care-measures.csv", colClasses = "character")
head(outcome)
dim(outcome)

outcome[,11] <- as.numeric(outcome[,11])
d <- data.frame(x=outcome[,11])
ggplot(d, aes(x)) + geom_histogram(fill="dodgerblue") +
      xlab("30-day death rates from heart attack") + ylab("Frequency")
