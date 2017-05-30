# Dates are stored as objects in Date class
x <- as.Date("1970-01-01")
unclass(x)
unclass(as.Date("1970-01-02"))
unclass(as.Date("1970-02-01"))

# Times are represented either by POSIXct or POSIXlt class
# POSIXct = large integer; class for storing times in data frame
# POSIXlt = list; stores day of week/year/month, month...
# generic functions: weekdays(), months(), quarters()

# Can convert between POSIX classes
x <- Sys.time()
x
class(x) # type is POSIXct
p <- as.POSIXlt(x)
p
class(p)
names(p)
unclass(p)
names(unclass(p))
p$sec
p$hour
p$mday

# Using POSIXct format
x <- Sys.time()
x
class(x)
names(x)
unclass(x) #different
names(unclass(x)) # different than above
x$sec



# STRPTIME function
datestring <- c("January 10, 2012 10:40", "December 9, 2011 9:10")
x <- strptime(datestring, "%b %d, %Y %H:%M")
x
class(x)
?strptime

# date operations
x <- as.Date("2012-01-01")
y <- strptime("9 Jan 2011 11:34:21", "%d %b %Y %H:%M:%S")
y
x - y # not same type
class(x); class(y)
x <- as.POSIXlt(x)
x - y

# another example
x <- as.Date("2012-03-01"); y <- as.Date("2012-02-28")
x;y
x - y
x <- as.POSIXct("2012-10-25 01:00:00")
y <- as.POSIXct("2012-10-25 06:00:00", tz="GMT")
x - y
y - x
