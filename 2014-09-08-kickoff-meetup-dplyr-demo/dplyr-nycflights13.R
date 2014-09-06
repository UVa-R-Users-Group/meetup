# Load packages
library("dplyr")
library("ggplot2")
library("nycflights13")
library("lubridate")

# it's a data.frame, but also a tbl_df.
# doesn't print entire thing to screen.
flights
class(flights)
weather
planes
airports
airlines

# dplyr also gives you verbs. All take a tbl_df as first argument.
# on their own, not much that base R can't do.
## select particular variables from flights
select(flights, year, month, day)
## filter based on some condition. all fights taken by this plane.
filter(flights, tailnum=="N14228")
## mutate adds new columns. time made up in air:
mutate(flights, madeup=dep_delay-arr_delay)
## summarize reduces grouped data to a single row.
summarize(flights, avgdelay=mean(arr_delay, na.rm=TRUE))
## group_by turns existing table into grouped_df class
## summary operations are performed by the group
group_by(flights, dest)
summarize(group_by(flights, dest), avgdelay=mean(arr_delay, na.rm=TRUE))

# Combining
# 1. flights dataset
# 2. mutate to add "madeup" variable
# 3. filter it where madeup>60
# 4. select particular columns

# %>% as "then"
select(
  filter(
    mutate(flights,
      madeup=dep_delay-arr_delay
    ), madeup>60
  ), dep_delay, arr_delay, dest, madeup
)

flights
flights %>%
  mutate(madeup=dep_delay-arr_delay) %>%
  filter(madeup>60) %>%
  select(dep_delay, arr_delay, dest, madeup)

# add a date to flights
flights
flights <- flights %>%
  mutate(date=ymd(paste(year, month, day, sep="-"))) %>%
  na.omit()
flights
flights %>% select(date)
class(flights$date)

# How many flights departed each day?
flights %>% group_by(date) %>% summarize(n=n())
# How many each day from each origin?
flights %>% group_by(date, origin) %>% summarize(n=n())
# how many by week by origin?
flights %>% group_by(week(date), origin) %>% summarize(n=n())
# we can name a variable in the resulting tbl_df
flights %>% group_by(week=week(date), origin) %>% summarize(n=n())

# let's create new data grouped by week and destination
# showing number of flights, average delays, and number distinct destinations
byweek <- flights %>%
  group_by(week=week(date), origin) %>%
  summarize(n=n(), delay=mean(arr_delay), ndests=n_distinct(dest))
byweek

# plot number of flights by week by origin city
byweek %>% ggplot(aes(week, n, colour=origin)) + geom_line()
# why the dip at the end?
# let's get total number of flights by week
byweek %>% summarize(sum(n))

# only 364 days in year, Dec 31 is probably day 1 of week 53
52*7

# same plot but not looking at week 53
byweek %>%
  filter(week<=52) %>%
  ggplot(aes(week, n, colour=origin)) + geom_line()
ymd("2013-01-01") + weeks(21) # mem day
ymd("2013-01-01") + weeks(26) # july 4
ymd("2013-01-01") + weeks(35) # labor day. why JFK continues decline?
ymd("2013-01-01") + weeks(47) # TG, xmas, NY

# why JFK lose number after labor day?
# LGA added a LOT more destinations just before this decline.
# makes more people fly out of there as opposed to JFK!
byweek %>%
  filter(week<=52) %>%
  ggplot(aes(week, ndests, colour=origin)) + geom_line()

# What causes delays?
# do older planes have more delays?
flights
planes
flights %>%
  inner_join(planes, by="tailnum") %>% # explain year.x, .y
  group_by(year.y) %>%
  summarize(delay=mean(arr_delay)) %>%
  filter(!is.na(year.y)) %>%
  ggplot(aes(year.y, delay)) + geom_point() + geom_smooth(method="loess")


# what about weather?
# look at delays by month by destination
flights %>%
  group_by(month, origin) %>%
  summarize(delay=mean(arr_delay)) %>%
  ggplot(aes(month, delay, fill=origin)) + geom_bar(stat="identity", position="dodge")

# summertime delays from thunderstorms? winter delays from snow?
# let's look at precipitation
# first lets get the delay by date and origin

# how does weather affect flights?
weather
# create new dataset grouped by date and origin adding mean delay
delay_byday_byorigin <- flights %>%
  group_by(date, origin) %>%
  summarize(delay=mean(arr_delay))
delay_byday_byorigin

# Take weather data, group by YMD, summarize mean participation
# remove missing data, add date with lubridate
# join to data created last time
# plot delay by precip coloring by flight's origin, add loess
weather %>% group_by(year, month, day) %>%
  summarize(precip = mean(precip)) %>%
  na.omit() %>%
  mutate(date=ymd(paste(year, month, day, sep="-"))) %>%
  inner_join(delay_byday_byorigin, by="date") %>%
  ggplot(aes(precip, delay, col=origin)) + geom_point() + geom_smooth()

# Is the average delay related to the average distance flown by a plane?
flights %>% group_by(tailnum) %>%
  summarize(dist=mean(distance), delay=mean(arr_delay), count=n()) %>%
  ggplot(aes(dist, delay)) + geom_point(aes(size=count), alpha=1/2) + scale_size_area()
