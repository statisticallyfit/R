
" This function translates 'num' to a number index if it is either 'best' or
'worst'
"
translateNum <- function(outcomeData, num){
      if(num == "best") return(1)
      else if(num == "worst") return(dim(outcomeData)[1])
      else return(num)
}

#---------------------------------------------------------------------------------------------

" This function returns true if the numCode is within the number of rows
of the info data.frame and false otherwise
"
withinBounds <- function(outcomeData, numCode){
      return(numCode >= 1) & (numCode <= nrow(outcomeData))
}

#---------------------------------------------------------------------------------------------

" This function takes a state, outcome, and num and returns the 
num-th hospital from a data.frame that is ordered first by rates and
then alphabetically, by hospital names. 
"
rankhospital <- function(state, outcome, num="best"){
      ## Read and organize outcome data
      outcomeData <- orderData(selectData(loadData(), state, outcome))
      # Translate num into an actual number and see if bounded correctly:
      numCode <- translateNum(outcomeData, num)
      if(!withinBounds(outcomeData, numCode)) return(NA)
      # If NA was not returned, find numth element of the outcomeData: 
      return(outcomeData$hospital[numCode])
}