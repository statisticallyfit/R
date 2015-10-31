# https://cran.r-project.org/web/packages/dplyr/vignettes/two-table.html

library(dplyr)
library(nycflights13)

# Drop unimportant variables
flightsOriginal <- flights
flights <- flights %>%
      select(year:day, hour, origin, dest, tailnum, carrier)
flights %>%
      left_join(airlines)

names(flights)
names(flightsOriginal)
