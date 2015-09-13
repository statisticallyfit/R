library(dplyr)

gapminderUrl = "http://tiny.cc/gapminder"
gapminderFile = read.delim(file=gapminderUrl)
str(gapminderFile)

head(gapminderFile)

gapminderTable = tbl_df(gapminderFile)
gapminderTable
glimpse(gapminderTable)

snippet = subset(gapminderTable, country=="Romania")
snippet

# filter to subset data row wise
filter(gapminderTable, lifeExp < 29)
filter(gapminderTable, country=="Rwanda")
filter(gapminderTable, country %in% c("Rwanda", "Afghanistan"))

# Pipe operator
gapminderTable %>% head 
gapminderTable %>% head(3)

# subsetting data
select(gapminderTable, year, lifeExp)

gapminderTable %>% 
  select(year, lifeExp) %>% 
  head(4)

# base call
gapminderFile[gapminderFile$country == "Cambodia", c("year", "lifeExp")]
subset(gapminderFile, country=="Cambodia", select=c("year", "lifeExp"))
# dplyr
gapminderTable %>%
  filter(country=="Cambodia") %>%
  select(year, lifeExp) 


# mutate to add new variables
gapminderTable = gapminderTable %>%
  mutate(gdp = pop*gdpPercap)
gapminderTable %>% head

# Just Canada
justCanada = gapminderTable %>% filter(country=="Canada")
head(justCanada)
gapminderTable = gapminderTable %>%
  mutate(canada=justCanada$gdpPercap[match(year, justCanada$year)], 
         gdpPercapRel = gdpPercap / canada)
head(gapminderTable)

gapminderTable %>%
  select(country, year, gdpPercap, canada, gdpPercapRel)

gapminderTable %>%
  select(gdpPercapRel) %>%
  summary

# how to take out a column
#gapminderTable = select(gapminderTable, -canada)


# Row-order by switching the columns

# sort by year then country
gapminderTable %>% 
  arrange(year, country)

# just data from 2007 sorted on life expectancy, or sort descendingly
gapminderTable %>%
  filter(year==2007) %>%
  arrange(desc(lifeExp))


# how to rename variables
gapminderTable %>%
  rename(life_exp = lifeExp, gdp_percap=gdpPercap, gdp_percap_rel = gdpPercapRel)



# Using group_by()

# 1) counting
gapminderTable %>%
  group_by(continent) %>%
  summarize(n_obs=n())

# another way
gapminderTable %>%
  group_by(continent) %>%
  tally

# 2) number of unique countries for each continent
gapminderTable %>%
  group_by(continent) %>%
  summarize(n_obs = n(), n_countries = n_distinct(country))

# 3) average lifeExp by continent
gapminderTable %>%
  group_by(continent) %>%
  summarize(avg_lifeExp = mean(lifeExp))

# 4) mean and median lifeExp and GDPpercapita by continent by year for 1952 and 2007
gapminderTable %>%
  filter(year %in% c(1952, 2007)) %>%
  group_by(continent, year) %>%
  summarise_each(funs(mean, median), lifeExp, gdpPercap)

# 5) Asia: min and max lifeExp by year
gapminderTable %>%
  filter(continent=="Asia") %>%
  group_by(year) %>%
  summarize(min_lifeExp = min(lifeExp), max_lifeExp = max(lifeExp))
