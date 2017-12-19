# Data are read in using connection interfaces
# file opens connection to file
# gzfile opens connection to gzip file
# bzfile, to file compressed with bzip2
# url, connection to webpage

str(file)
# description: name of file
# open = code indicating
  # r = read only
  # w = writing
  # a = appending
  # rb, wb, ab = reading, writing or appending 
    # in binary mode (Windows)

# Connections
con = file("foo.txt", "r")
data = read.csv(con)
close(con)
# Connections are useful for reading parts of file
con = gzfile("words.gz")
x = readLines(con, 10) # read first ten lines

con = url("http://www.jhsph.edu", "r")
x = readLines(con)
head(x)
