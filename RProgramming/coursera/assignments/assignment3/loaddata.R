

outcomeData <- read.csv("data/outcome-of-care-measures.csv", colClasses = "character")
outcomeData[,11] <- as.numeric(outcomeData[,11])
