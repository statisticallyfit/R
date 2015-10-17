library(dplyr)
library(tidyr)
library(knitr)
#library(printr)  # download failed
library(hflights)

#install.packages("hflights")

str(hflights)

# change to tbl
hflights <- tbl_df(hflights)
glimpse(hflights)
str(hflights)

# clean up data
lut  <- c("AA" = "American", "AS" = "Alaska", "B6" = "JetBlue", 
          "CO" = "Continental", "DL" = "Delta", "OO" = "SkyWest", 
          "UA" = "United", "US" = "US_Airways", "WN" = "Southwest", 
          "EV" = "Atlantic_Southeast", "F9" = "Frontier", "FL" = "AirTran", 
          "MQ" = "American_Eagle", "XE" = "ExpressJet", "YV" = "Mesa")
hflights$UniqueCarrier <- lut[hflights$UniqueCarrier]
glimpse(hflights)


# Select() and mutate()


# SELECT TAKES A SUBSET ----------------------------------------------------
# select(tbldata, Var1, Var2, ...)
hflights_subset <- select(hflights, ActualElapsedTime, AirTime, ArrDelay, DepDelay)
kable(head(hflights_subset), align='c')

hflights_subset2 <- select(hflights, Year:DayOfWeek, ArrDelay:Diverted)


# Helper functions for select()
# starts_with(), ends_with(), contains(), matches(), num_range(), one_of()
select(hflights, matches("ArrDelay"), matches("DepDelay"))
select(hflights, one_of(c("UniqueCarrier", "FlightNum", "TailNum", 
                          "Cancelled", "CancellationCode")))
select(hflights, ends_with("Time"), ends_with("Delay"))



# MUTATE ADDS COLUMNS ------------------------------------------------------
# mutate(data, Mutant1=expr(Var0, Var1...))

# Add the new variable to a copy of hflights and save result as g1
g1 <- mutate(hflights, ActualGroundTime = ActualElapsedTime - AirTime)
str(g1)
# Add the new variable to a copy of g1 and save result as g2
g2 <- mutate(g1, GroundTime = TaxiIn + TaxiOut)
str(g2)
# Add the new variable AverageSpeed to a copy of g2 and save result as g3
g3 <- mutate(g2, AverageSpeed = Distance / AirTime * 60)
str(g3)

# Multiple variables
m1 <- mutate(hflights, loss = ArrDelay - DepDelay, loss_percent = (ArrDelay - DepDelay)/DepDelay * 100)
m2 <- mutate(hflights, loss = ArrDelay - DepDelay, loss_percent = loss / DepDelay * 100)
m3 <- mutate(hflights, TotalTaxi = TaxiIn + TaxiOut, 
                       ActualGroundTime = ActualElapsedTime - AirTime, 
                       Diff = TotalTaxi - ActualGroundTime)
str(hflights)
str(g3)
str(m1)
str(m2)
str(m3)
