"
This function takes the data.frame from loadData(), transforming it into a dplyr object and cleaning it, 
so removing all NA values and selecting only the columns necessary for this study. Returns tbl_df object.
"
selectData <- function(outcomeData, state, outcome) {
      library(dplyr)
      # Make into dplyr object
      outcomeData <- tbl_df(outcomeData)
      
      # filter to get only relevant state and outcome data
      # rename columns
      # remove NA rows
      
      # Rename outcomes to shorter names
      outcomeData <- rename(outcomeData, Pneumonia=Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
      outcomeData <- rename(outcomeData, Heart.Attack=Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
      outcomeData <- rename(outcomeData, Heart.Failure=Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
      # Coerce to numeric
      outcomeData$Pneumonia <- suppressWarnings(as.numeric(outcomeData$Pneumonia))
      outcomeData$Heart.Attack <- suppressWarnings(as.numeric(outcomeData$Heart.Attack))
      outcomeData$Heart.Failure <- suppressWarnings(as.numeric(outcomeData$Heart.Failure))
      # Select only relevant columns
      outcomeData <- select(outcomeData, State, Hospital.Name, Pneumonia, Heart.Attack, Heart.Failure)
      # Remove rows for which Pneumonia, Heart.Attack, or Heart.Failure are NA
      outcomeData <- filter(outcomeData, !is.na(Pneumonia), !is.na(Heart.Attack), !is.na(Heart.Failure))
      
      # Filter by state
      outcomeData <- filter(outcomeData, State==state)
      
      # Filter by outcome and rename the columns
      if(outcome == "heart attack"){
            outcomeData <- select(outcomeData, Hospital.Name, Heart.Attack)
            outcomeData <- rename(outcomeData, hospital=Hospital.Name, outcome=Heart.Attack)
      } else if(outcome == "heart failure"){
            outcomeData <- select(outcomeData, Hospital.Name, Heart.Failure)
            outcomeData <- rename(outcomeData, hospital=Hospital.Name, outcome=Heart.Failure)
      }else if(outcome == "pneumonia"){
            outcomeData <- select(outcomeData, Hospital.Name, Pneumonia)
            outcomeData <- rename(outcomeData, hospital=Hospital.Name, outcome=Pneumonia)
      }
      
      return(outcomeData)
}