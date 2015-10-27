" This function takes a specific data frame with numeric items in the
'rate' column and character items in the 'hospital' column. It creates
a data frame that is ordered first by the rate column and second by
the hospital name column.
"
orderData <- function(info) {
      orderedIndexes <- order(info$rate, info$hospital)
      rates <- info$rate[orderedIndexes]
      names <- info$hospital[orderedIndexes]
      return(data.frame(hospital=names, rate=rates))
}