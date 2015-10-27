" This function takes a state, outcome, and num and returns the 
num-th hospital from a data.frame that is ordered first by rates and
then alphabetically, by hospital names. 
"
rankhospital <- function(state, outcome, num="best"){
      ## Read and organize outcome data
      outcomeData <- loadData()
      # Check the data if state or outcome are invalid
      checkData(state, outcome, outcomeData)
      # Remove NA's in rates and remove the hospitals with NA rates
      info <- filterAndMakeData(state, outcome, outcomeData)
      # If num was given as a string, turn it to a number
      numCode <- translateNum(info, num) 
      # If numCode is not within info's row index bounds, return NA
      if(!(numCodeWithinBounds(info, numCode))) return(NA)
      # Else, order the data
      orderedInfo <- orderData(info)
      # Find the num-th hospital
      return(as.character(orderedInfo$hospital[numCode]))
}