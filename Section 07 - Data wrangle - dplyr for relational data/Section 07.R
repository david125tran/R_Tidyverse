# Remove previous objects:
rm(list = ls())
graphics.off()

# ------------------------------ Libraries ------------------------------
# install.packages("cowplot")
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
# install.packages("purrr")
# install.packages("readr")
# install.packages("readxl")
# install.packages("rio")
# install.packages("stringr")
# install.packages("tibble")
# install.packages("tidyr")
# install.packages("tidyverse")

# ------------------------------ Load Libraries ------------------------------
library(nycflights13)
library(tidyverse)

# ------------------------------ dplyr for Relational Database Management Systems (RDBMS) ------------------------------
# dbplyr - Database backend for dplyr (convert dplyr code to SQL)

# nycflights 13 tables:
# View(airlines)
# View(airports)
# View(flights)
# View(planes)
# View(weather)

# Create simple tables
table_x <- tribble(~key, ~val, 
                   1,    "a1",
                   2,    "a2",
                   3,    "a3")

table_y <- tribble(~key, ~val, 
                   1,    "b1",
                   2,    "b2",
                   3,    "b3")  
              
# ------------------------------ SQL - Inner Join ------------------------------
inner_join(x = table_x,
           y = table_y,
           by = "key")

                    # # A tibble: 3 × 3
                    #     key val.x val.y
                    #   <dbl> <chr> <chr>
                    # 1     1 a1    b1
                    # 2     2 a2    b2
                    # 3     3 a3    b3

airlines
flights %>% colnames()

df <- flights %>%
    inner_join(x = .,       # this retrieves what is left from the pipe (flights)
               y = airlines,
               by = c("carrier" = "carrier")) %>%
    rename(carrier_name = name)

                    # # A tibble: 336,776 × 20
                    #     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
                    #    <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
                    #  1  2013     1     1      517            515         2      830            819
                    #  2  2013     1     1      533            529         4      850            830
                    #  3  2013     1     1      542            540         2      923            850
                    #  4  2013     1     1      544            545        -1     1004           1022
                    #  5  2013     1     1      554            600        -6      812            837
                    #  6  2013     1     1      554            558        -4      740            728
                    #  7  2013     1     1      555            600        -5      913            854
                    #  8  2013     1     1      557            600        -3      709            723
                    #  9  2013     1     1      557            600        -3      838            846
                    # 10  2013     1     1      558            600        -2      753            745
                    # # ℹ 336,766 more rows
                    # # ℹ 12 more variables: arr_delay <dbl>, carrier <chr>, flight <int>,
                    # #   tailnum <chr>, origin <chr>, dest <chr>, air_time <dbl>, distance <dbl>,
                    # #   hour <dbl>, minute <dbl>, time_hour <dttm>, carrier_name <chr>

# Get the counts
df %>%
    count(carrier_name) %>%
    arrange(desc(n))

                    # # A tibble: 16 × 2
                    #    carrier_name                    n
                    #    <chr>                       <int>
                    #  1 United Air Lines Inc.       58665
                    #  2 JetBlue Airways             54635
                    #  3 ExpressJet Airlines Inc.    54173
                    #  4 Delta Air Lines Inc.        48110
                    #  5 American Airlines Inc.      32729
                    #  6 Envoy Air                   26397
                    #  7 US Airways Inc.             20536
                    #  8 Endeavor Air Inc.           18460
                    #  9 Southwest Airlines Co.      12275
                    # 10 Virgin America               5162
                    # 11 AirTran Airways Corporation  3260
                    # 12 Alaska Airlines Inc.          714
                    # 13 Frontier Airlines Inc.        685
                    # 14 Mesa Airlines Inc.            601
                    # 15 Hawaiian Airlines Inc.        342
                    # 16 SkyWest Airlines Inc.          32

# ------------------------------ SQL - Left Join ------------------------------
left_join(x = table_x,
          y = table_y,
          by = "key")

# Rename year column in planes table
df.planes <- planes %>%
    rename(year_plane = year)

# Multiple left joins
df.all <- flights %>%
    left_join(x = .,                    # this retrieves what is left from the pipe (flights)
              y = airlines,
              by = "carrier") %>%
    rename(carrier_name = name) %>%
    left_join(x = .,                    # this retrieves what is left from the pipe 
              y = airports,
              by = c("dest" = "faa")) %>%
    rename(dest_name = name) %>%
    left_join(x = .,
              y = df.planes,
              by = c("tailnum" = "tailnum")) %>%
    left_join(x = .,
              y = weather,
              by = c("year" = "year",
                     "month" = "month",
                     "day" = "day",
                     "hour" = "hour"))

                        # # A tibble: 1,006,987 × 46
                        #     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
                        #    <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
                        #  1  2013     1     1      517            515         2      830            819
                        #  2  2013     1     1      517            515         2      830            819
                        #  3  2013     1     1      517            515         2      830            819
                        #  4  2013     1     1      533            529         4      850            830
                        #  5  2013     1     1      533            529         4      850            830
                        #  6  2013     1     1      533            529         4      850            830
                        #  7  2013     1     1      542            540         2      923            850
                        #  8  2013     1     1      542            540         2      923            850
                        #  9  2013     1     1      542            540         2      923            850
                        # 10  2013     1     1      544            545        -1     1004           1022
                        # # ℹ 1,006,977 more rows
                        # # ℹ 38 more variables: arr_delay <dbl>, carrier <chr>, flight <int>,
                        # #   tailnum <chr>, origin.x <chr>, dest <chr>, air_time <dbl>, distance <dbl>,
                        # #   hour <dbl>, minute <dbl>, time_hour.x <dttm>, carrier_name <chr>,
                        # #   dest_name <chr>, lat <dbl>, lon <dbl>, alt <dbl>, tz <dbl>, dst <chr>,
                        # #   tzone <chr>, year_plane <int>, type <chr>, manufacturer <chr>, model <chr>,
                        # #   engines <int>, seats <int>, speed <int>, engine <chr>, origin.y <chr>, …

# View(df.all)

# ------------------------------ SQL - Right Join ------------------------------
right_join(x = table_x,
           y = table_y,
           by = "key")

# Flight counts
df.flights.counts <- flights %>%
    count(tailnum)

# Bright flights counts to planes table
df.planes <- df.flights.counts %>%
    right_join(x = .,                   # this retrieves what is left from the pipe
               y = planes,
               by = "tailnum") %>%
    rename(`number of flights` = n)

# View(df.planes)

# ------------------------------ SQL - Full Join ------------------------------
full_join(x = table_x,
          y = table_y,
          by = "key")

# Select only relevant columns
df.dest <- flights %>%
    select(carrier, dest)

# Full join
df.carrier_dest <- airlines %>%
    full_join(x = .,
              y = df.dest,
              by = "carrier")

# View(df.carrier_dest)

# Find nas in `dest` column
df.carrier_dest %>%
    filter(is.na(dest))
# There are no missing values from the full join as expected


# ------------------------------ Filtering Joins ------------------------------
# semi_join(x, y)       - Keeps all observations in x that have a match in y
# anti_join(x, y)       - Drops all observations in x that have a match in y

semi_join(x = table_x,
          y = table_y,
          by = "key")

                        # # A tibble: 3 × 2
                        #     key val
                        #   <dbl> <chr>
                        # 1     1 a1
                        # 2     2 a2
                        # 3     3 a3

airlines1 <- airlines %>%
    filter(carrier %in% c("AA", "DL", "VX"))

semi_join(x = airlines1,
          y = flights,
          by = "carrier")

                        # # A tibble: 3 × 2
                        #   carrier name
                        #   <chr>   <chr>
                        # 1 AA      American Airlines Inc.
                        # 2 DL      Delta Air Lines Inc.
                        # 3 VX      Virgin America

# ------------------------------ Binding Tables & Set Operations ------------------------------
# Binding Tables:
# bind_cols(x, y)       - Paste tables side by side and return a single table 
# bind_row(x, y)        - Paste tables on top of the other and return a single table

# Set Operations:
# intersect(x, y)       - Returns only observations in both x and y
# union(x, y)           - Returns unique observations in x and y
# setdiff(x, y)         - Returns observations in x, but not in y

# Create two simple tables
airlines1 <- airlines %>%
    slice(c(1, 3, 5, 7, 9))

airlines2 <- airlines %>%
    slice(c(2, 4, 6, 8, 10))

# bind_cols()
bind_cols(airlines1, airlines2)

# bind_rows()
bind_rows(airlines1, airlines2)

# intersect()
intersect(airlines1, airlines2)

# setdiff()
setdiff(airlines1, airlines2)

# union()
union(airlines1, airlines2)

# See the unique values:
union(airlines1, airlines2) %>% distinct()
# As expected, returns the same thing as: union(airlines1, airlines2)

# ------------------------------ dplyr's Additional Functions ------------------------------
# add_row(.data, ...)           - Add one or more rows to a table
# add_column(.data, ...)        - Add new column or columns to a table
# rownames_to_column()          - Move row names into column, or move column into row names
# columns_to_rownames()         - Move row names into column, or move column into row names
# lag()                         - Offset elements by -1 position
# lead()                        - Offset elements by +1 position
# cumsum()                      - Calculate cumulative sum on selected column
# cumprod()                     - Calculate cumulative product on selected column
# cummin()                      - Calculate cumulative minimum on selected column
# cummax()                      - Calculate cumulative maximum on a selected column
# cummean()                     - Calculate cumulative mean on a selected column
# dense_rank()                  - Rank values in a selected column

df <- flights %>%
    filter(carrier == "AA") %>%
    arrange(time_hour)

# Check if two successful flights flew from the same origin airport
df <- df %>% 
    mutate(`origin prev flight` = lag(x = origin, n = 1)) %>%
    mutate(`origin test` = case_when(origin == `origin prev flight` ~ TRUE, T ~ FALSE))

df %>% filter(`origin test`) %>%
    count()

# Check if two successful flights had a total distance of over 2,000 miles
df <- df %>%
    mutate(`distance successive flights` = distance + lead(x = distance, n = 1)) %>%
    mutate(`distance test` = case_when(`distance successive flights` >= 2000 ~ TRUE, T ~ FALSE))

df %>% filter(`distance test`) %>%
    count()

# Check first time a flight has a total distance of over 1,000,000 miles
df <- df %>%
    mutate(`distance running total` = cumsum(distance))

df %>%
    mutate(`flight id` = row_number()) %>%
    filter(`distance running total` >= 1000000) %>%
    select(`flight id`, everything()) %>%
    head(1) %>%
    as.data.frame()

# Rank flights based on the distance of the flights
df <- df %>%
    mutate(`rank flight` = dense_rank(distance))
