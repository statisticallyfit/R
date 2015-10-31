# peaople.math.aau.dk/~sorenh/teaching/2014-cowidur/misc/dplyr-tutorial.pdf

library(nycflights13)
library(dplyr)

glimpse(flights)
glimpse(weather)
glimpse(planes)
glimpse(airports)

# make date cols in flights and weather as.Date() - but not exist???

# EXAMPLE ---------------------------------------------------------------
df <- data.frame(color=c("blue", "black", "blue", "blue", "black"), 
                 value=1:5)
df

filter(df, color == "blue")
filter(df, value %in% c(1,4))
filter(df, color %in% "black")

# ----------------------------------------------------------------------


# Find all flights to SFO or OAK
filter(flights, dest == "SFO" | dest == "OAK")
filter(flights, dest %in% c("SFO", "OAK"))
# In January
filter(flights, month == 1)
# Delayed by more than an hour
filter(flights, dep_delay > 60)
# That departed between midnight and 5 am
filter(flights, hour >= 0 & hour <= 5) # why not dep_delay?
# Where the arrival delay was more than twice the departure delay
filter(flights, arr_delay > 2 * dep_delay)


# Three ways to select the two delay variables
select(flights, arr_delay, dep_delay)
select(flights, ends_with("delay"))
select(flights, contains("delay"))
select(flights, arr_delay:dep_delay)


# EXAMPLE ---------------------------------------------------------------
df
arrange(df, color, value)
# ----------------------------------------------------------------------


# Order flights by departure date and time
arrange(flights, dep_time, hour, minute)
# Which flights were most delayed?
arrange(flights, desc(dep_delay))
arrange(flights, desc(arr_delay))
# Which flights caught up the most time during the flight?
arrange(flights, desc(dep_delay - arr_delay))


# EXAMPLE ---------------------------------------------------------------
df
mutate(df, double = 2*value, quadruple = 2*double)
# ----------------------------------------------------------------------


# Compute speed in mph from time (in minutes) and distance (in miles). 
# Which flight flew fastest?
flights <- mutate(flights, speed = distance/(minute/60 + hour))
arrange(flights, desc(speed))
# Add variable showing how much time was made up or lost in flight
mutate(flights, delta = dep_delay - arr_delay)
# How to compute hour and minute from dep ? 
# mutate(flights, hour = dep %/% 100, minute = dep %/% 100)


# EXAMPLE ---------------------------------------------------------------
df
summarise(df, total = sum(value))
df %>%
      group_by(color) %>%
      summarise(total = sum(value))
# ----------------------------------------------------------------------

#by_date <- group_by(flights, date)
#by_hour <- group_by(flights, date, hour)
#by_plane <- group_by(flights, plane)
by_dest <- group_by(flights, dest)
by_dest
flights


# Summarise dep_delay for each day
by_date <- group_by(flights, date)
delays <- summarise(by_date, 
                    mean = mean(dep_delay), 
                    median = median(dep_delay), 
                    q75 = quantile(dep_delay, 0.75), 
                    p_over_15 = mean(dep_delay > 15), 
                    P_over_30 = mean(dep_delay > 30),
                    p_over_60 = mean(dep_delay > 60))



# Pipeline exercises

# Which destinations have highest average delays?
flights %>%
      group_by(dest) %>%
      summarise(mean_arr_delay = mean(arr_delay, na.rm=TRUE), 
                n = n()) %>%
      arrange(desc(mean_arr_delay))

.Last.value %>% View()

# Which flights (carrier + flight) happen every day? Where is dest?
flights %>%
      group_by(carrier, flight, dest) %>%
      tally(sort = TRUE) %>%
      filter(n == 365)

flights %>%
      group_by(carrier, flight, dest) %>%
      summarise(n = n()) %>%
      arrange(desc(n)) %>%
      filter(n == 365)

flights %>%
      group_by(carrier, flight) %>%
      filter(n() == 365)

# On average, how do delays (of non-cancelled flights) vary over
# the course of a day (Hint: hour + min/60)
per_hour <- flights %>%
      filter(cancelled == 0) %>%
      mutate(time = hour + minute / 60) %>%
      group_by(time) %>%
      summarise(arr_delay = mean(arr_delay, na.rm = TRUE), n=n())

qplot(time, arr_delay, data = per_hour)
qplot(time, arr_delay, data=per_hour, size = n) + scale_size_area()
qplot(time, arr_delay, data = filter(per_hour, n > 30), size = n) + 
      scale_size_area()

ggplot(filter(per_hour, n > 30), aes(time, arr_delay)) + 
      geom_vline(xintercept = 5:24, colour = "white", size = 2) + 
      geom_point(shape = 19)
