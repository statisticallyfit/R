# to make reading in large data sets more efficient: 
  # set comment.char = "" if there are no commented 
    # lines in the file
  # read help page and estimate RAM needed
  # colClasses = specify what classes are in each column
  # set nrows argument to help memory usage
initial = read.table("datatable.txt", nrows=100)
classes = sapply(initial, class)
tabAll = read.table("datatable.txt", colClasses=classes)

# Calculating Memory requirements: 
# data frame has 1,500,00 rows and 120 cols of numeric data
# memory needed to store: 
  # 1,500,00 * 120 * 8 bytes/numeric
  # 1440000000 bytes
  # 1440000000 / 2^20 bytes/MB
  # 1,373.29 MB
  # 1.34 GB 
  # to read in takes twice this amount