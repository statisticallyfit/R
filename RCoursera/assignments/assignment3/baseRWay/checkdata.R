
" This function stops execution of the program if either the 
state or outcome given are incorrect.
"
checkData <- function(state, outcome, outcomeData){
      outcomes <- c("heart attack", "heart failure", "pneumonia")
      states  <- unique(outcomeData$State)
      
      # Stop if invalid state or outcome name given
      if(! state %in% states) stop("invalid state")
      if(! outcome %in% outcomes) stop("invalid outcome")
}