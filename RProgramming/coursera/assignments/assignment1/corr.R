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

" Read data and make the correlations
"
corr <- function(directory, threshold=0) {
      ## 'directory' is a character vector of length 1 indicating
      ## the location of the CSV files
      
      ## 'threshold' is a numeric vector of length 1 indicating the
      ## number of completely observed observations (on all
      ## variables) required to compute the correlation between
      ## nitrate and sulfate; the default is 0
      
      ## Return a numeric vector of correlations
      ## NOTE: Do not round the result!
      fileNameList <- makeNames(1:332)
      fileLocList <- makeLocations(directory, fileNameList)
      
      # Read data; each element in dataList is each csv file
      cur.dir <- "/datascience/projects/statisticallyfit/github/learningprogramming/R/RProgramming/coursera/assignments/assignment1"
      corList <- numeric(0)
      for(loc in fileLocList){
            location <- paste(cur.dir, loc, sep="")
            data <- read.csv(location, header=TRUE)
            
            if(sum(!is.na(data$sulfate)) > threshold & sum(!is.na(data$nitrate)) > threshold){
                  corList <- c(corList, cor(data$sulfate, data$nitrate, use="pairwise.complete"))
            }
      }
      return(corList)
}