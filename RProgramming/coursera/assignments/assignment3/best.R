library(ggplot2)
setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RProgramming/coursera/assignments/assignment3")

source("plot30daymortalityrates.R")

outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack
outcome$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia
outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure

best <- function(state, outcome) {
      ## Read outcome data
      
}



scrapBest <- function(info) {
      #rates <- subset(outcome, State==state, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)[,]
      #names <- outcome$Hospital.Name[which(outcome$State==state)]
      #info <- data.frame(hospital=names, rate=rates)
      
      # Find minimum rate for the hospitals
      bestRate <- min(info$rate, na.rm=TRUE)
      
      hospitalWithBestRate <- ""
      
      # Find if the minimum is repeated in the list
      dupIndex <- anyDuplicated(info$rate)
      # If so, get names of hospitals with same rates. 
      if(dupIndex != 0){
            if(bestRate == )
            hospitalsWithSameRates <- info$hospital[duplicated(info$rate)]
            # Sort alphabetically out of these hospitals and keep first
            hospitalWithBestRate <- (as.character(sort(hospitalsWithSameRates)[1]))
      } else {
            hospitalWithBestRate <- info$hospital[which(info$rate == bestRate)]
      }
      return(hospitalWithBestRate)
}


# Test framework
assert <- function(P, Q){
      if(!P) 
            print(Q)
}
# Test data
names <- c("Mary's", "John's", "Gleeson", "Samantha Hospital", "Newton", 
           "Princeton", "Jake", "Robinson", "Henry", "Tens", "Heart")

# Test case: minimum is duplicated and it comes BEFORE other duplicates
rates1 <- c(13.4, 50.1, 50.3, 19, 13.5, 13.4, 20, 70, 13.4, 15, 15)
d1 <- data.frame(hospital=names, rate=rates1); d1
d1$hospital[which(d1$rate==13.4)]
bool <- scrapBest(d1) == "Henry"
assert(bool, "Failed test 1")

# Test case: minimum is duplicated but comes AFTER other duplicates
rates2 <- c(13.4, 50.1, 50.3, 19, 15, 15, 20, 70, 13.4, 15, 15)
d2 <- data.frame(hospital=names, rate=rates2); d2
d2$hospital[which(d2$rate==13.4)]
assert(scrapBest(d2) == "Henry", "Failed test 2")

# Test case: minimum is not duplicated and comes BEFORE duplicates
rates3 <- c(135, 50.1, 50.3, 13.4, 15, 15, 20, 70, 15, 15, 21)
d3 <- data.frame(hospital=names, rate=rates3); d3
d3$hospital[which(d3$rate==13.4)]
assert(scrapBest(d3) == "Samantha Hospital", "Failed test 3")

# Test case: minimum is not duplicated but comes AFTER duplicates
rates4 <- c(135, 50.1, 50.3, 19, 15, 15, 20, 70, 13.4, 15, 15)
d4 <- data.frame(hospital=names, rate=rates4); d4
d4$hospital[which(d4$rate==13.4)]
assert(scrapBest(d4) == "Henry", "Failed test 4")

# Test case: minimum is not duplicated; no duplicates
rates5 <- c(135, 50.1, 50.3, 19, 15, 16, 20, 70, 13.4, 19.4, 18)
d5 <- data.frame(hospital=names, rate=rates5); d5
d5$hospital[which(d5$rate==13.4)]
assert(scrapBest(d5) == "Henry", "Failed test 5")

# Test case: all numbers are the same
rates6 <- rep(13.4, 11)
d6 <- data.frame(hospital=names, rate=rates6); d6
d6$hospital[which(d6$rate==13.4)]
assert(scrapBest(d6) == "Gleeson", "Failed test 6")
