"
This function takes tbl_df object from selectData() and arranges the columns first by outcome level and then
by hospital names.
"

orderData <- function(outcomeData) {
      # Order outcome (low to high) then hospitals
      return(arrange(outcomeData, outcome, hospital))
}