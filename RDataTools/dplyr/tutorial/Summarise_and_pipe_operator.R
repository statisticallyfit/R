library(dplyr)
library(hflights)

# SUMMARISE SUBSETS AND DOES STATISTICS ON IT
# summarise(data, sumvar=sum(A), argvar=avg(B))

# Determine the shortest and longest distance flown and save statistics to 
# min_dist and max_dist
s1 <- summarise(hflights, min_dist = min(Distance), max_dist = max(Distance))
# Determine longest distance for diverted flights and save to max_div
s2 <- summarise(filter(hflights, Diverted == 1), max_div = max(Distance))

# Calculate summarizing statistics for flights that have ArrDelay that is not NA
s1 <- summarise(filter(hflights, !is.na(ArrDelay)), 
           earliest = min(ArrDelay), 
           average = mean(ArrDelay), 
           latest = max(ArrDelay), 
           sd = sd(ArrDelay))
# Calculate the max taxiing difference for flights that have taxi data
s2 <- summarise(filter(hflights, !is.na(TaxiIn), !is.na(TaxiOut)), 
                max_taxi_diff = max(abs(TaxiIn - TaxiOut)))
print(s2)


# DPLYR AGGREGATE FUNCTIONS
# first(x)
# last(x)
# nth(x, n)
# n() - num rows in data.frame summarise describes
# n_distinct(x) - returns number of unique values in vector x

# Calculate the summarizing statistics of hflights
s1 <- summarise(hflights, n_obs = n(), 
                n_carrier = n_distinct(UniqueCarrier), 
                n_dest = n_distinct(Dest), 
                dest100 = nth(Dest, 100))
kable(s1, align='c')
# Calculate statistics for flights flown by American Airlines
s2 <- summarise(filter(hflights, UniqueCarrier == "American"), 
          n_flights = n(), 
          n_canc = sum(Cancelled == 1), 
          p_canc = mean(Cancelled == 1) * 100, 
          avg_delay = mean(ArrDelay, na.rm = TRUE))
kable(head(s2), align='c')



# PIPE OPERATOR ------------------------------------------------------------
p <- hflights %>%
  mutate(diff = TaxiOut - TaxiIn) %>%
  filter(!is.na(diff)) %>%
  summarise(avg = mean(diff))
print(p)


# DRIVE OR FLY -------------------------------------------------------------

# Part 1, selection and creation of relevant columns
d <- hflights %>%
  select(Dest, UniqueCarrier, Distance, ActualElapsedTime) %>%
  mutate(RealTime = ActualElapsedTime + 100, mph = Distance / RealTime * 60)
str(d)
head(d)

# Part 2, concerning flights that had actual avgspeed < 70 mph
d %>%
  filter(!is.na(mph), mph < 70) %>%
  summarise(n_less_than_70 = n(), 
            n_dest = n_distinct(Dest), 
            min_dist = min(Distance), 
            max_dist = max(Distance))

# other burdens of air traveling...
hflights %>%
  select(Dest, Cancelled, Distance, ActualElapsedTime, Diverted) %>%
  mutate(RealTime = ActualElapsedTime + 100, mph = Distance/RealTime*60) %>%
  filter(mph < 105 | Cancelled == 1 | Diverted == 1) %>%
  summarise(n_non = n(), 
            p_non = n_non / nrow(hflights) * 100, 
            n_dest = n_distinct(Dest), 
            min_dist = min(Distance), 
            max_dist = max(Distance))

# Count the number of overnight flights
hflights %>%
  filter(!is.na(DepTime), !is.na(ArrTime), DepTime > ArrTime) %>%
  summarise(n = n())



# GROUP_BY and databases ---------------------------------------------------
# group_by(data, Var0, Var1, ...)

# Make calculations to end up with ordered statistics per carrier
hflights %>%
  group_by(UniqueCarrier) %>%
  summarise(n_flights = n(), 
            n_canc = sum(Cancelled == 1), 
            p_canc = mean(Cancelled == 1) * 100, 
            avg_delay = mean(ArrDelay, na.rm=TRUE)) %>%
  arrange(avg_delay, p_canc)

# Which day of the week is average total taxiing time the highest?
str(hflights)
hflights %>%
  group_by(DayOfWeek) %>%
  summarise(avg_taxi = mean(TaxiIn + TaxiOut, na.rm=TRUE)) %>%
  arrange(desc(avg_taxi))



# COMBINE GROUP_BY AND MUTATE ---------------------------------------------

# Part 1
# Line 4 finds mean of T/F vector ==> probability, not actual mean
hflights %>%
  group_by(UniqueCarrier) %>%
  filter(!is.na(ArrDelay)) %>%
  summarise(prob_delay = mean(ArrDelay > 0)) %>%
  mutate(rank = rank(prob_delay)) %>%
  arrange(rank)

# Part 2
hflights %>%
      group_by(UniqueCarrier) %>%
      filter(!is.na(ArrDelay), ArrDelay > 0) %>%
      summarise(avg = mean(ArrDelay)) %>%
      mutate(rank = rank(avg)) %>%
      arrange(rank)


# ---------------------------------------------------------------------
# Group_by exercises

# Which plane (by tail number) flew out of Houston the most times? 
# How many times?
adv1 <- hflights %>%
      group_by(TailNum) %>%
      summarise(n = n()) %>%
      filter(n == max(n))
adv1

# How many airplanes only flew to one destination from Houston?
adv2 <- hflights %>%
      group_by(TailNum) %>%
      summarise(ndest = n_distinct(Dest)) %>%
      filter(ndest == 1) %>%
      summarise(nplanes = n())
adv2

# Find the most visited destination for each carrier
adv3 <- hflights %>%
      group_by(UniqueCarrier, Dest) %>%
      summarise(n = n()) %>%
      mutate(rank = rank(desc(n))) %>%
      filter(rank == 1)
adv3

# Find the carrier that travels to each destination the most
adv4 <- hflights %>%
      group_by(Dest, UniqueCarrier) %>%
      summarise(n = n()) %>%
      mutate(rank = rank(desc(n))) %>%
      filter(rank == 1)
adv4



# ---------------------------------------------------------------------
# Dplyr can deal with different data types

#install.packages("data.table")
library(data.table)

hflights2 <- as.data.table(hflights)
s2 <- hflights2 %>%
      summarise(n_carrier = n_distinct(UniqueCarrier))
s2


# ---------------------------------------------------------------------
# Dplyr and mySQL databases (dplyr converts queries, converts 
# needed data only, so no memory problems in R)

#install.packages("nycflights13")
library(nycflights13)

summarise(flights, 
          n_carriers = n_distinct(carrier), 
          n_flights = n())
