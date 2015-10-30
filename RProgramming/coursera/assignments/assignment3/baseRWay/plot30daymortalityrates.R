library(ggplot2)

source("loaddata.R")
head(outcomeData)
dim(outcomeData)

d <- data.frame(x=outcomeData[,11])
ggplot(d, aes(x)) + geom_histogram(fill="dodgerblue") +
      xlab("30-day death rates from heart attack") + ylab("Frequency")
