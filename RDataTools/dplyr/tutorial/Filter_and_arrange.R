library(dplyr)
library(tidyr)
library(knitr)
library(hflights)


# using logical operators ... x &in& c(a,b,c) is TRUE if x is in the vector

str(hflights)


# FILTER RETURNS COLUMNS THAT FIT A GIVEN CONDITION -------------------------
# All flights that traveled 3000 miles or more
f1 <- filter(hflights, Distance >= 3000)
str(f1)
# All flights flown by either airline...
f2 <- filter(hflights, UniqueCarrier %in% c("JetBlue", "Southwest", "Delta"))
str(f2)
# All flights where taxiing took longer than flying
f3 <- filter(hflights, TaxiIn + TaxiOut > AirTime)
str(f3)

# All flights that departed before 5am or arrived after 10pm
f1 <- filter(hflights, DepTime < 500 | ArrTime > 2200)
str(f1)
# All flights that departed late but arrived ahead of schedule
f2 <- filter(hflights, DepDelay > 0, ArrDelay < 0)
str(f2)
# All cancelled weekend flights
f3 <- filter(hflights, DayOfWeek %in% c(6,7), Cancelled == 1)
# All flights that were cancelled after being delayed
f4 <- filter(hflights, Cancelled == 1, DepDelay > 0)
str(f4)
f5 <- filter(hflights, DepDelay > 0, Cancelled == 1)
identical(f4, f5)



# PRACTICE ------------------------------------------------------------------
# Select flights that had JFK as their destination
c1 <- filter(hflights, Dest == "JFK")
# Combine the Year, Month, and DayofMonth variables to create a Date column
c2 <- mutate(c1, Date = paste(Year, Month, DayofMonth, sep = "-"))
# Retain only a subset of columns to provide an overview
c3 <- select(c2, Date, DepTime, ArrTime, TailNum)
kable(head(c3), align='c')

# How many weekend flights flew more than 1000 miles but taxi time was <15min
dim(filter(hflights, DayOfWeek %in% c(6,7), Distance > 1000, TaxiIn + TaxiOut < 15))



# ARRANGE ORDERS THE COLUMNS -----------------------------------------------
# arrange(data, Var0, Var1..)

dtc <- filter(hflights, Cancelled == 1, !is.na(DepDelay))
str(dtc)

# Arrange dtc by departure delays
a1 <- arrange(dtc, DepDelay)
str(a1)
# Arrange dtc so that cancellation reasons are grouped
a2 <- arrange(dtc, CancellationCode)
str(a2)
# Arrange according to carrier and departure delays
a3 <- arrange(dtc, UniqueCarrier, DepDelay)
str(a3)

# Reverse the order of arranging
# Arrange according to carrier and decreasing departure delays
a1 <- arrange(hflights, UniqueCarrier, desc(DepDelay))
# Arrange flights by total delay (normal order)
a2 <- arrange(hflights, ArrDelay + DepDelay)
# Keep flights leaving to DFW before 8am and arrange according to decreasing
# AirTime
a3 <- arrange(filter(hflights, Dest == "DFW", DepTime < 800), desc(AirTime))
