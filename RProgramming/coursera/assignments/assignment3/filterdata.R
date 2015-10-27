" This function finds the names of hospitals for a given state, 
finds the rates of hospitals given state and outcome, and then
cleans this information so that NAs are removed. Returns a data
frame composed of this information.
"
filterAndMakeData <- function(state, outcome, outcomeData) {
      # Match outcome string to actual columns
      rates <- c()
      if(outcome == "heart attack")
            rates <- subset(outcomeData, State==state, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)[,]
      else if(outcome == "heart failure")
            rates <- subset(outcomeData, State==state, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)[,]
      else if(outcome == "pneumonia")
            rates <- subset(outcomeData, State==state, Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)[,]
      
      # Find names of hospitals for the specified state
      names <- outcomeData$Hospital.Name[which(outcomeData$State==state)]
      # Get names and rates for which rates are not NA
      filteredNames <- names[which(!is.na(rates))]
      filteredRates <- rates[!is.na(rates)]
      # Create the data frame that has hospitals alongside their rates
      return(data.frame(hospital=filteredNames, rate=filteredRates))
}