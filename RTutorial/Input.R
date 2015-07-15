# Tutorial: http://www.cyclismo.org/tutorial/R/

# Read csv file

setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RTutorial")
heisenberg <- read.csv(file="simple.csv", head=TRUE, sep=",")
heisenberg
summary(heisenberg)

dir() #to see what files are in this directory
getwd()

# the $ symbol is used to access the columns of table variable
heisenberg$trial
heisenberg$mass
heisenberg$velocity

# gets column names
names(heisenberg)



# Trees cdiac data
tree <- read.csv(file="trees91.csv", header=TRUE, sep=",")
tree
typeof(tree)
attributes(tree)

names(tree)
tree$C  #get column data that has label C
tree$N
tree$RTBM
tree$CHBR
tree$LFNCC




# Fixed width file has preset character spaces
# 1st col = 17 chars wide
# 2nd col = 15 chars wide
# last col = 7 chars wide

info <- read.fwf('fixedWidthFile.dat', widths=c(-17, 15, 7), col.names=c('temp', 'offices'))
info
