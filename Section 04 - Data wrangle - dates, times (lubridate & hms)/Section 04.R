# Remove previous objects:
rm(list = ls())
graphics.off()

# ------------------------------ Libraries ------------------------------
# install.packages("data.table")
# install.packages("dplyr")
# install.packages("feather")
# install.packages("forcats")
# install.packages("ggplot2")
# install.packages("hms")
# install.packages("languageserver")
# install.packages("lintr")
# install.packages("lubridate")
# install.packages("nycflights13")
# install.packages("readr")
# install.packages("readxl")
# install.packages("rio")
# install.packages("stringr")
# install.packages("tibble")
# install.packages("tidyr")
# install.packages("tidyverse")

# ------------------------------ Load Libraries ------------------------------
library(hms)
library(lubridate)
library(nycflights13)
library(tidyverse)

# ------------------------------ Date & Time ------------------------------
# date (2021-12-31) - stored as the number of days since 1970-01-01
# time (12:00:00) - an hms is a time stored as the number of seconds since 00:00:00
# date-time (2021-12-31 12:00:00) - is a point on the timeline stored as the number of seconds since 00:00:00

# ------------------------------ Create Dates & Times ------------------------------
d <- as_date(18992)                     # Number of days since 1970-01-01
d   # "2021-12-31"

t <- as_hms(120)                        # Number of seconds since 00:00:00
t   # 00:02:00

dt <- as_datetime((1640952000))         # Number of days and seconds since 1970-01-01 00:00:00
dt  # [1] "2021-12-31 12:00:00 UTC"     

# Parsing: string or number conversion
ymd_hms("2021-12-31 12:00:00")                      # "2021-12-31 12:00:00 UTC"  
ymd_hm("2021-12-31 12:00")                          # "2021-12-31 12:00:00 UTC"
ymd_h("2021-12-31 12")                              # "2021-12-31 12:00:00 UTC"

dmy_hms("31 Dec 2021 22/15:00")                     # "2021-12-31 22:15:00 UTC"
yq("2021: Q3")                                      # "2021-07-01"

hms::hms(seconds = 5, minutes = 1, hours = 0)       # 00:01:05
lubridate::hms("00:01:05")                          # "1M 5S"

# Parse date stored as decimal number - date_decimal()
d <- seq(2021, 2022, 0.25)
d                                                   # 2021.00 2021.25 2021.50 2021.75 2022.00
date_decimal(d)

                    # [1] "2021-01-01 00:00:00 UTC" "2021-04-02 06:00:00 UTC"
                    # [3] "2021-07-02 12:00:00 UTC" "2021-10-01 18:00:00 UTC"
                    # [5] "2022-01-01 00:00:00 UTC"

# Parse datetime - faste_strptime()
fast_strptime(x = "2021-12-31 12:00:00", format = "%Y-%m-%d %H:%M:%S")      # "2021-12-31 12:00:00 UTC"

# Parse datetime - parse_date_time()
parse_date_time(x = "2021-12-31 12:00:00", orders = "ymd HMS")              # "2021-12-31 12:00:00 UTC"

# Create datetime from individual components
flights

flights %>%
    select(year, month, day, hour, minute)

                    # # A tibble: 336,776 × 5
                    #     year month   day  hour minute
                    #    <int> <int> <int> <dbl>  <dbl>
                    #  1  2013     1     1     5     15
                    #  2  2013     1     1     5     29
                    #  3  2013     1     1     5     40
                    #  4  2013     1     1     5     45
                    #  5  2013     1     1     6      0
                    #  6  2013     1     1     5     58
                    #  7  2013     1     1     6      0
                    #  8  2013     1     1     6      0
                    #  9  2013     1     1     6      0
                    # 10  2013     1     1     6      0
                    # # ℹ 336,766 more rows

flights %>%
    select(year, month, day, hour, minute) %>%
    mutate(datetime = make_datetime(year, month, day, hour, minute),
           date = make_date(year, month, day))

                    # # A tibble: 336,776 × 7
                    #     year month   day  hour minute datetime            date
                    #    <int> <int> <int> <dbl>  <dbl> <dttm>              <date>
                    #  1  2013     1     1     5     15 2013-01-01 05:15:00 2013-01-01
                    #  2  2013     1     1     5     29 2013-01-01 05:29:00 2013-01-01
                    #  3  2013     1     1     5     40 2013-01-01 05:40:00 2013-01-01
                    #  4  2013     1     1     5     45 2013-01-01 05:45:00 2013-01-01
                    #  5  2013     1     1     6      0 2013-01-01 06:00:00 2013-01-01
                    #  6  2013     1     1     5     58 2013-01-01 05:58:00 2013-01-01
                    #  7  2013     1     1     6      0 2013-01-01 06:00:00 2013-01-01
                    #  8  2013     1     1     6      0 2013-01-01 06:00:00 2013-01-01
                    #  9  2013     1     1     6      0 2013-01-01 06:00:00 2013-01-01
                    # 10  2013     1     1     6      0 2013-01-01 06:00:00 2013-01-01
                    # # ℹ 336,766 more rows

# Create datetime from existing objects
now()                   # "2024-05-27 09:47:23 EDT"
today()                 # "2024-05-27"

# Conversion
as_date(now())          # "2024-05-27"
as_datetime(today())    # "2024-05-27 UTC"
as_datetime(now())      # "2024-05-27 09:48:19 EDT"

# ------------------------------ Date & Time Code Values ------------------------------
# Code  Value
# %d    Day of the month (decimal number)
# %a    Abbreviated weekday
# %m    Month (decimal number)
# %A    Full weekday
# %b    Month (abbreviated)
# %I    Decimal hour (12 hour)
# %B    Month (full name)
# %j    Decimal day of the year
# %y    Year (2 digits)
# %w    Decimal weekday (0=Sunday)
# %Y    Year (4 digits)
# %W    Decimal week of the year (Starting on Monday)
# %H    Decimal hour (24 hour)
# %p    Locale-specific AM/PM
# %M    Decimal minute
# %x    Locale-specifc date
# %S    Decimal second
# %X    Locale-specific time

# ------------------------------ Extracting Date/Time Components ------------------------------
dt <- now()
dt                      # "2024-05-27 09:59:39 EDT"

# Extracting individual components:
year(dt)                # 2024
month(dt)               # 5
day(dt)                 # 27
hour(dt)                # 9
minute(dt)              # 59
second(dt)              # 39.23522

# Day of the week:
wday(dt)                # 2

# Store values of a component into column
flights %>% 
    select(year, month, day, hour, minute) %>%
    mutate(datetime = make_datetime(year, month, day, hour, minute)) %>%
    # Extract week day, week, and quarter
    mutate(wday = wday(datetime, week_start = 1),
           week = week(datetime),
           Q = quarter(datetime)) %>%
    arrange(desc(datetime))

                        # # A tibble: 336,776 × 9
                        #     year month   day  hour minute datetime             wday  week     Q
                        #    <int> <int> <int> <dbl>  <dbl> <dttm>              <dbl> <dbl> <int>
                        #  1  2013    12    31    23     59 2013-12-31 23:59:00     2    53     4
                        #  2  2013    12    31    23     59 2013-12-31 23:59:00     2    53     4
                        #  3  2013    12    31    23     59 2013-12-31 23:59:00     2    53     4
                        #  4  2013    12    31    23     59 2013-12-31 23:59:00     2    53     4
                        #  5  2013    12    31    23     30 2013-12-31 23:30:00     2    53     4
                        #  6  2013    12    31    22     55 2013-12-31 22:55:00     2    53     4
                        #  7  2013    12    31    22     50 2013-12-31 22:50:00     2    53     4
                        #  8  2013    12    31    22     50 2013-12-31 22:50:00     2    53     4
                        #  9  2013    12    31    22     45 2013-12-31 22:45:00     2    53     4
                        # 10  2013    12    31    22     45 2013-12-31 22:45:00     2    53     4
                        # # ℹ 336,766 more rows

# ------------------------------ Rounding & Setting Date/Time Components ------------------------------
# Rounding components

# floor_date(x, unit)       Round down to nearest unit
# round_date(x, unit)       Round to nearest unit
# ceiling_date(x, unit)     Round to nearest unit
# rollback(dates)           Rollback to last day of previous month

d <- today()
d                                   # "2024-05-27"

# Round down the month
floor_date(d, unit = "month")       # "2024-05-01"

# Round up the month
ceiling_date(d, unit = "month")     # "2024-06-01"

# Round to the nearest month
round_date(d, unit = "month")       # "2024-06-01"

# Rollback to the last day of the previous month
rollback(d)                         # "2024-04-30"

# Updating components

# year(datetime) <- 2022
# update(datetime, year = 2022, month = 11, mday = 5, hour = 8)

dt                                  # "2024-05-27 09:59:39 EDT"
year(dt) <- 2022
month(dt) <- 12
day(dt) <- 31
hour(dt) <- 1
minute(dt) <- 59
second(dt) <- 1
dt                                  # "2022-12-31 01:59:01 EST"

# Update all components with update()
dt <- update(dt, year = 2020, month = 1, day = 11, hour = 1, minute = 1, second = 1)
dt                                  # "2020-01-11 01:01:01 EST"

# ------------------------------ Date/Time Arithmetics & Durations ------------------------------
today <- today()                    # "2024-05-27"

# Add one day
today + 1                           # "2024-05-28"

now <- now()                        # "2024-05-27 12:44:39 EDT"

# Add one hour
now + 3600                          # "2024-05-27 13:44:39 EDT"

birth_date <- ymd("1975-05-01")
age <- today - birth_date
age                                 # Time difference of 17924 days

# Durations

# Convert age to duration 
as.duration(age)                    # "1548633600s (~49.07 years)"

# Constructor functions

x <- 1                              # number of seconds

dyears(x)                           # "31557600s (~1 years)"
dmonths(x)                          # "2629800s (~4.35 weeks)"
dweeks(x)                           # "604800s (~1 weeks)"
dmilliseconds(x)                    # "0.001s"

is.duration(as.duration(age))       # TRUE

# Durations - Arithmetics
dseconds(10) + dminutes(1)          # "70s (~1.17 minutes)"
dyears(1) - dweeks(27)              # "15228000s (~25.18 weeks)"
10 * dmonths(1)                     # "26298000s (~43.48 weeks)"

# Inconsistent timeline behavior (Durations)
# Daylight Savings Time (DST)
dt <- ymd_hms("2016-03-12 13:00:00", tz = "America/New_york")       # "2016-03-12 13:00:00 EST"
leap_year(dt)                                                       # TRUE

# Add one day 
dt + ddays(1)                                                       # "2016-03-13 14:00:00 EDT"     <- daylight savings taken into account

# Leap Year
dt <- ymd_hms("2020-02-28 23:00:00")                                # "2020-02-28 23:00:00 UTC"
leap_year(dt)                                                       # TRUE
dt + dyears(1)                                                      # "2021-02-28 05:00:00 UTC"     <- leap year taken into account

# ------------------------------ Periods ------------------------------
# Periods track changes in clock times, which ignore timeline irregularities (different than durations)

age                                 # Time difference of 17924 days
as.period(age)                      # "17924d 0H 0M 0S"

# Constructor function
seconds(3600)                       # "3600S"
minutes(60)                         # "60M 0S"
hours(1)                            # "1H 0M 0S"
days(1)                             # "1d 0H 0M 0S"
weeks(52)                           # "364d 0H 0M 0S"
months(1)                           # "1m 0d 0H 0M 0S"
years(1)                            # "1y 0m 0d 0H 0M 0S"

period_to_seconds(years(1))         # 31557600
seconds_to_period(31557600)         # "365d 6H 0M 0S"
period(3600, units = "minute")      # "3600M 0S"

# Periods - Arithmetics
seconds(10) + minutes(1)            # "1M 10S"
years(1) + weeks(27)                # "1y 0m 189d 0H 0M 0S"
10 * months(1)                      # "10m 0d 0H 0M 0S"

# Inconsistent timeline behavior (Periods)
# Daylight Savings Time (DST)
dt <- ymd_hms("2016-03-12 13:00:00", tz = "America/New_york")       # "2016-03-12 13:00:00 EST"
leap_year(dt)                                                       # TRUE
dt + days(1)                                                        # "2016-03-13 13:00:00 EDT"     <- daylight savings not taken into account

# Leap Year
dt <- ymd_hms("2020-02-28 23:00:00")                                # "2020-02-28 23:00:00 UTC"
leap_year(dt)                                                       # TRUE
dt + years(1)                                                       # "2021-02-28 23:00:00 UTC"     <- leap year not taken into account

# ------------------------------ Interval ------------------------------
# Interval is a duration with a starting point: that makes it precise - you can determine
# exactly how long it is. 

# Interval Helper Functions
# a%within%b                    *Does the interval or date-time "a" fall within interval "b"?
# int_start(int)                *Access/set the start/end date-time
# int_end(int)                   of an interval
# int_aligns(int1, int1)        *Do two intervals overlap,
# int_overlaps(int)              share boundary?
# int_diff(times)               *Make the intervals that occur between the date-times in a vector.  
# int_flip(int)                 *Reverse the direction of an interval.
# int_length(int)               *Length in seconds.
# int_shift(int, by)            *Shifts an interval up or down the timeline by a timespan.
# as_interval(x, start)         *Coerce time spans to an interval with the start date-time
# is_interval()                 *Test if interval object.

d1 <- ymd("2021-12-30")
d2 <- ymd("2021-12-31")
d1
d2

i1 <- interval(d1, d2)          # 2021-12-30 UTC--2021-12-31 UTC
i2 <- d2 %--% d1                # 2021-12-31 UTC--2021-12-30 UTC
i1
i2

# Extract boundaries
int_start(i1)                   # "2021-12-30 UTC"
int_end(i1)                     # "2021-12-31 UTC"

# Is a time point within a given interval?
today() %within% i1             # FALSE

# Do intervals overlap?
int_overlaps(i1, i2)            # TRUE

# Create intervals from vector of dates
dates <- now() + days(1:365)
length(dates)                   # 365
int_diff(dates)

# Length of an interval / Flip interval
int_length(i1)                  # 86400
int_flip(i1)                    # 2021-12-31 UTC--2021-12-30 UTC

# ------------------------------ Time Zones ------------------------------
# R supports around 600 different time zones
# Use the UTC - Universal Time Coordinated to avoid Daylight Savings Time occurences

# Lubridate - Time zone functions
# Sys.timezone()                R returns your current time zone based on your computer settings.
# OlsonNames()                  R's complete list of time zones
# with_tz(time, tz="...")       Get the same date-time in a new time zone.
# force_tz(time, tz="...")      Get the same clock-time in a new time zone.
# ymd_hms(..., tz="...")        Print date time with selected time zone

# My computer's time zone
Sys.timezone()                  # "America/New_York"

# Get a list of all time zones in R
OlsonNames()

