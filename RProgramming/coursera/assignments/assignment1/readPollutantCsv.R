# Help from: https://github.com/gberger/rprog/tree/master/assignment1
readPollutantCsv <- function(directory, id) {
      read.csv(paste("data/", directory, "/", sprintf("%03d", id), ".csv", sep=''))
}