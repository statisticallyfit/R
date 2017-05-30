" This function returns true if the numCode is within the number of rows
of the info data.frame and false otherwise
"
numCodeWithinBounds <- function(info, numCode){
      return(numCode >= 1) & (numCode <= nrow(info))
}