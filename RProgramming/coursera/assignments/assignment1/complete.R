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

" Read data and make the data frame
"
complete <- function(directory, id=1:332) {
      ## 'directory' is a character vector of length 1 indicating
      ## the location of the CSV files
      
      ## 'id' is an integer vector indicating the monitor ID numbers
      ## to be used
      
      ## Return a data frame of the form: (this is for SULFATE data)
      ## id nobs
      ## 1  117
      ## 2  1041
      ## ...
      ## where 'id' is the monitor ID number and 'nobs' is the
      ## number of complete cases
      
      fileNameList <- makeNames(id)
      fileLocList <- makeLocations(directory, fileNameList)
      
      # Read data; each element in dataList is each csv file
      cur.dir <- "/datascience/projects/statisticallyfit/github/learningprogramming/R/RProgramming/coursera/assignments/assignment1"
      idList <- vector()
      nobsList <- vector()
      for(loc in fileLocList){
            location <- paste(cur.dir, loc, sep="")
            data <- read.csv(location, header=TRUE)
            
            # note the id in the list
            idList <- c(idList, data$id[1])
            nobsList <- c(nobsList, sum(!is.na(data$sulfate)))
      }
      return(data.frame(idList, nobsList))
}