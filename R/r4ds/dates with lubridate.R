
# handling dates  ---------------------------------------------------------

ipk = function(pkg){
  new.pkg = list.of.pkgs[!(list.of.pkgs %in% .packages(all.available = TRUE))]
  if(length(new.pkg)) install.packages(new.pkg, dependencies = TRUE)
  suppressPackageStartupMessages(sapply(pkg, library, character.only = TRUE))
}

list.of.pkgs = c('tidyverse', 'magrittr', 'nycflights13', 'lubridate')
ipk(list.of.pkgs)


# getting dates  ----------------------------------------------------------

now()
today()

# from strings ------------------------------------------------------------

ymd("2017-01-31")
mdy("January 31st, 2017")
dmy("31-Jan-2017")


# also take unquoted numbers ----------------------------------------------

ymd(20170131)

# date and time -----------------------------------------------------------

ymd_hms("2017-01- 31 20:11:59")
mdy_hm("01/31/2017 08:01")


# from individual components with make_date() ----------------------------------------------

flights %>% select(year, month, day, hour, minute) %>%
  mutate(departure = make_datetime(year, month, day, hour, minute))

make_datetime_100 = function(year, month, day, time){
  make_datetime(year, month, day, time %/% 100, time %% 100)
}

flights_dt = flights %>%
  filter(!is.na(dep_time), !is.na(arr_time)) %>%
  mutate(
    dep_time = make_datetime_100(year, month, day, dep_time),
    arr_time = make_datetime_100(year, month, day, arr_time),
    sched_dep_time = make_datetime_100(year, month, day, sched_dep_time),
    sched_arr_time = make_datetime_100(year, month, day, sched_arr_time)
  ) %>%
  select(origin, dest, ends_with("delay"), ends_with("time"))

flights_dt

flights_dt %>%
  ggplot(aes(dep_time)) + 
  geom_freqpoly(binwidth = 86400) # 86400 secs = 1 day


# from other types with as_datetime() or as_date()--------------------------------------------------------

as_datetime(today())
as_date(365*10 + 2)


# getting components ------------------------------------------------------
datetime <- ymd_hms("2016-07-08 12:34:56")
year(datetime)
month(datetime)
mday(datetime)
yday(datetime)
wday(datetime)
wday(datetime, label = T)
minute(datetime)
hour(datetime)


# setting components ------------------------------------------------------

(datetime <- ymd_hms("2016-07-08 12:34:56"))
update(datetime, year = 2020, month = 2, mday = 2, hour = 2)

# You can use update() to show the distribution of flights across the
# course of the day for every day of the year:

flights_dt %>%
mutate(dep_hour = update(dep_time, yday = 1)) %>%
ggplot(aes(dep_hour)) +
geom_freqpoly(binwidth = 300)


# Rounding dates floor_date(), round_date(), ceiling_date() -------------------

flights_dt %>%
  count(week = floor_date(dep_time, 'week')) %>%
  ggplot(aes(week, n)) + geom_line()

# time spans - durations, periods, intervals ------------------------------

h_age = today() - ymd(19930801)
as.duration(h_age)

## durations have a bunch of constructs
dseconds(15); dminutes(10); dhours(c(12, 24)); ddays(0:5) # durations record
# time spans in seconds
# you can add and multiply durations

2 * dyears(1)
dyears(1) + dweeks(12) + dhours(15)
tomorrow = today() + ddays(1)
last_year = today() - dyears(1)

# periods - not fixed time spans like durations ---------------------------

seconds(15); minutes(10); hours(c(12, 24)); days(7); months(1:6); weeks(3); years(1)
## you can add and multiply periods

10 * (months(6) + days(1))
days(50) + hours(25) + minutes(2)

## unlike durations, periods do what you expect
## a leap year
ymd("2016-01-01") + dyears(1)
ymd("2016-01-01") + years(1)

## solving problem of flights arriving earlier than departure time

flights_dt %>% filter(arr_time < dep_time) # these are overnight flights

## we fix this by adding days(1) to the arrival time of each overnight flight
flights_dt = flights_dt %>%
  mutate(
    overnight = arr_time < dep_time,
    arr_time = arr_time + days(overnight * 1),
    sched_arr_time = sched_arr_time + days(overnight *1)
  )
flights_dt %>%
  filter(overnight, arr_time < dep_time)


# intervals ---------------------------------------------------------------

next_year = today() + years(1)
(today() %--% next_year) %/% days(1)

## to find how nay periods fall into an interval, use integer division
(today() %--% next_year) %/% days(1)

# Create a vector of dates giving the first day of every month in
# 2015
ymd("2015-01-01") + months(0:11)

# Create a vector of dates giving the first day of every month
# in the current year
floor_date(today(), unit = "year") + months(0:11)


# time zones --------------------------------------------------------------

Sys.timezone()
length(OlsonNames()) # complete length of time zones
head(OlsonNames())
# change time zones
(x1 <- ymd_hms("2015-06-01 12:00:00", tz = "America/New_York"))
(x2 <- ymd_hms("2015-06-01 18:00:00", tz = "Europe/Copenhagen"))
(x3 <- ymd_hms("2015-06-02 04:00:00", tz = "Pacific/Auckland"))
x4 = c(x1, x2, x3)
x4b = force_tz(x4, tzone = "Europe/Paris")
