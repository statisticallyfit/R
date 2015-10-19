" Create list of file names based on id's given
"
makeNames <- function(id) {
      fileNameList <- vector()
      for(value in id){
            if(value < 10){
                  zeroes <- "00"
            }
            else if(value >= 10 & value < 100){
                  zeroes <- "0"
            }
            else {
                  zeroes <- ""
            }
            fileName <- paste(zeroes, value, ".csv", sep="")
            fileNameList <- c(fileNameList, fileName)
      } 
      return(fileNameList)
}

" Create list of data locations based on id's given
"
makeLocations <- function(directory, fileNameList) {
      fileLocList <- vector()
      for(name in fileNameList){
            fileLoc <- file.path("/data", directory, name)
            fileLocList <- c(fileLocList, fileLoc)
      }
      return(fileLocList)
}

" Read data and calculate pollutant mean
"
pollutantmean <- function(directory, pollutant, id=1:332) {
      ## 'directory' is a character vector of length 1 indicating
      ## the location of the CSV files
      
      ## 'pollutant' is a character vector of length 1 indicating
      ## the name of the pollutant for which we will calculate the
      ## mean; either "sulfate" or "nitrate".
      
      ## 'id' is an integer vector indicating the monitor ID numbers
      ## to be used
      
      ## Return the mean of the pollutant across all monitors list
      ## in the 'id' vector (ignoring NA values)
      ## NOTE: Do not round the result!
      
      fileNameList <- makeNames(id)
      fileLocList <- makeLocations(directory, fileNameList)
      
      # Read data; each element in dataList is each csv file
      cur.dir <- "/datascience/projects/statisticallyfit/github/learningprogramming/R/RProgramming/coursera/assignments/assignment1"
      total <- 0
      n <- 0
      for(loc in fileLocList){
            location <- paste(cur.dir, loc, sep="")
            data <- read.csv(location, header=TRUE)
            # add the values
            s <- sum(data[, pollutant], na.rm=TRUE)
            total <- total + s
            n <- n + sum(!is.na(data[, pollutant]))
      }
      return(total / n)
}