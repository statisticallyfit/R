# READING DATA
# read.table(), read.csv() for tabluar data
# readLines for reading lines in text file
# source for reading R code files( inverse of dump)
# dget for reading in R code files (inverse of dput)
# load for reading in saved workspaces
# unserialize for reading single R objects in binaryform

# WRITING DATA
# write.table
# writeLines
# dump
# dput
# save
# serialize

# read.table() - default separator is the space
# arguments: 
  # file
  # header = does file have header line or not?
  # sep = how columns are separated
  # colClasses = character vector showing the class
    # of each column
  # nrows
  # comment.char = character string for comment character
  # skip = number of lines to skip from beginning
  # stringsAsFactors = should character variables
    # be coded as factors?

# read.csv() - separator is the comma, header=TRUE 
