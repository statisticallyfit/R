" 
This function uses the rankhospital() function to rank all the
hospitals in all states.
"
rankall <- function(outcome, num="best"){
      # Apply rankhospital() function for each state
      states <- sort(unique(outcomeData$State))
      hospitalList <- c()
      for(theState in states){
            hospitalList <- c(hospitalList, rankhospital(theState, outcome, num))
      }
      return(data.frame(hospital=hospitalList, state=states))
}