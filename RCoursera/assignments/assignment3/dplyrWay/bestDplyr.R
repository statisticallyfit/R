" Given the state and outcome names, best() finds which hospital has the minimum mortality 
(or best) rate. If multiple hospitals have the same minimum rate, the best() function returns the hospital that 
comes alphabetically first. 
"
best <- function(state, outcome) {
      ## Read and organize outcome data
      outcomeData <- orderData(selectData(loadData(), state, outcome))
      # Now just return the first element of the ordered tbl_df
      return(outcomeData$hospital[1])
}
