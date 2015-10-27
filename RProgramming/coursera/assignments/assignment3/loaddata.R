" This function sets the working directory, reads in data, and
transforms certain columns to have numeric types. Returns the data.
"
loadData <- function(){
      setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RProgramming/coursera/assignments/assignment3")
      outcomeData <- read.csv("data/outcome-of-care-measures.csv", colClasses = "character")
      outcomeData[,11] <- suppressWarnings(as.numeric(outcomeData[,11]))
      outcomeData[,17] <- suppressWarnings(as.numeric(outcomeData[,17]))
      outcomeData[,23] <- suppressWarnings(as.numeric(outcomeData[,23]))
      return(outcomeData)
}
