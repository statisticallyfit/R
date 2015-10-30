" 
This function sets the working directory, reads and returns the data.
"
loadData <- function(){
      setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RProgramming/coursera/assignments/assignment3")
      outcomeData <- read.csv("data/outcome-of-care-measures.csv", colClasses = "character")
      return(outcomeData)
}