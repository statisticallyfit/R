" Given the state and outcome names, best() finds which hospital has the 
minimum mortality (or best) rate. If multiple hospitals have the 
same minimum rate, the best() function returns the hospital that 
comes alphabetically first. 
"
best <- function(state, outcome) {
      ## Read and organize outcome data
      outcomeData <- loadData()
      checkData(state, outcome, outcomeData)
      info <- filterAndMakeData(state, outcome, outcomeData)
      orderedInfo <- orderData(info)
      
      # Find the hospital with the minimum rate. 
      # This one is at the top of the alphabetically ordered list.
      return(as.character(orderedInfo$hospital[1]))
}
