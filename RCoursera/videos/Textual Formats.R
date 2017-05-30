# dumping and dputing make textual format editable
# dump and dput preserve the metadata 
# text formats adhere to Unix philosophy
# text formats are long-lived
# textual formats work better with version control
# con: not very space-efficient

setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RProgramming/coursera/videos")
y = data.frame(a=1, b="a")
y
dput(y)
dput(y, file="outputFiles/y.R")
new.y = dget("outputFiles/y.R")
new.y

# dump can only be used on single objects
x = "foo"
y = data.frame(a=1, b="a")
dump(c("x", "y"), file="outputFiles/data.R")
rm(x, y) # remove from workspace
# now reconstruct the objects in workspace
source("outputFiles/data.R")
y
x
