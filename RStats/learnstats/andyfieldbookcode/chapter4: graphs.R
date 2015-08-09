library(ggplot2)

# install.packages("DSUR")
# library(DSUR)

setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learnstats/andyfieldbookcode")

facebookData <- read.delim("data/FacebookNarcissism.dat", header=TRUE)
facebookData

graph <- ggplot(facebookData, aes(NPQC_R_Total, Rating))
graph + geom_point()
graph + geom_point(shape=19, 
                   col="dodgerblue", size=3)
graph + geom_point(shape=19, 
                   aes(colour=Rating_Type))
graph + geom_point(shape=19, 
                   aes(colour=Rating_Type),
                   position="jitter")
graph + geom_point(aes(shape=Rating_Type),
                   position="jitter")


# Scatterplot
examData <- read.delim("data/Exam Anxiety.dat", header=TRUE)
examData
