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
library(hflights)
library(purrr)
library(tidyverse)

# ------------------------------ map() ------------------------------
# Example - Using hflights
df <- hflights %>%
    select(ActualElapsedTime, AirTime, Distance, TaxiIn, TaxiOut)

df <- as_tibble(df)

# Average values
df %>%
    map(.x = ., .f = mean, na.rm = TRUE)

                    # $ActualElapsedTime
                    # [1] 129.3237

                    # $AirTime
                    # [1] 108.1423

                    # $Distance
                    # [1] 787.7832

                    # $TaxiIn
                    # [1] 6.098855

                    # $TaxiOut
                    # [1] 15.0911
# Minimum values
df %>%
    map(min, na.rm = TRUE)

# Maximum values
df %>%
    map(max, na.rm = TRUE)

# Standard deviation
df %>%
    map(sd, na.rm = TRUE)

# Check the structure of hflights
str(hflights)
                    # 'data.frame':   227496 obs. of  21 variables:
                    #  $ Year             : int  2011 2011 2011 2011 2011 2011 2011 2011 2011 2011 ...
                    #  $ Month            : int  1 1 1 1 1 1 1 1 1 1 ...
                    #  $ DayofMonth       : int  1 2 3 4 5 6 7 8 9 10 ...
                    #  $ DayOfWeek        : int  6 7 1 2 3 4 5 6 7 1 ...
                    #  $ DepTime          : int  1400 1401 1352 1403 1405 1359 1359 1355 1443 1443 ...
                    #  $ ArrTime          : int  1500 1501 1502 1513 1507 1503 1509 1454 1554 1553 ...
                    #  $ UniqueCarrier    : chr  "AA" "AA" "AA" "AA" ...
                    #  $ FlightNum        : int  428 428 428 428 428 428 428 428 428 428 ...
                    #  $ TailNum          : chr  "N576AA" "N557AA" "N541AA" "N403AA" ...
                    #  $ ActualElapsedTime: int  60 60 70 70 62 64 70 59 71 70 ...
                    #  $ AirTime          : int  40 45 48 39 44 45 43 40 41 45 ...
                    #  $ ArrDelay         : int  -10 -9 -8 3 -3 -7 -1 -16 44 43 ...
                    #  $ DepDelay         : int  0 1 -8 3 5 -1 -1 -5 43 43 ...
                    #  $ Origin           : chr  "IAH" "IAH" "IAH" "IAH" ...
                    #  $ Dest             : chr  "DFW" "DFW" "DFW" "DFW" ...
                    #  $ Distance         : int  224 224 224 224 224 224 224 224 224 224 ...
                    #  $ TaxiIn           : int  7 6 5 9 9 6 12 7 8 6 ...
                    #  $ TaxiOut          : int  13 9 17 22 9 13 15 12 22 19 ...
                    #  $ Cancelled        : int  0 0 0 0 0 0 0 0 0 0 ...
                    #  $ CancellationCode : chr  "" "" "" "" ...
                    #  $ Diverted         : int  0 0 0 0 0 0 0 0 0 0 ...
                    
# Another way to check if columns are numeric
numeric.cols <- map(.x = hflights, .f = is.numeric) %>%
    unlist() %>%
    tibble(column = names(.),
    numeric = .) %>%
    filter(numeric == T) %>%
    pull(column)

                    #  [1] "Year"              "Month"             "DayofMonth"
                    #  [4] "DayOfWeek"         "DepTime"           "ArrTime"
                    #  [7] "FlightNum"         "ActualElapsedTime" "AirTime"
                    # [10] "ArrDelay"          "DepDelay"          "Distance"
                    # [13] "TaxiIn"            "TaxiOut"           "Cancelled"        
                    # [16] "Diverted"

# Example - Using mpg 
mpg %>%
    select(displ, year, cyl, cty, hwy)

numeric.cols <- map(.x = mpg, .f = is.numeric)

# Check if columns are numeric
                    # $model
                    # [1] FALSE

                    # $displ
                    # [1] TRUE

                    # $year
                    # [1] TRUE

                    # $cyl
                    # [1] TRUE

                    # $trans
                    # [1] FALSE

                    # $drv
                    # [1] FALSE

                    # $cty
                    # [1] TRUE

                    # $hwy
                    # [1] TRUE

                    # $fl
                    # [1] FALSE

                    # $class
                    # [1] FALSE

# Another way to check if columns are numeric
numeric.cols <- map(.x = mpg, .f = is.numeric) %>%
    unlist() %>%
    tibble(column = names(.),
    numeric = .) %>%
    filter(numeric == T) %>%
    pull(column)

# "displ" "year"  "cyl"   "cty"   "hwy"

# Map output types:
# map()         - Returns a list
# map_chr()     - Returns a character vector
# map_lgl()     - Returns a logical vector
# map_int()     - Returns an integer vector
# map_dbl()     - Returns a double (numeric) vector
# map_dfc()     - Returns a data frame (column bind)
# map_dfr()     - Returns a data frame (row bind)

# ------------------------------ Map With Controlled Output ------------------------------
# map_dbl()     - Returns a double (numeric) vector
df %>% map_dbl(.x = ., .f = mean, na.rm = TRUE)
df %>% map_dbl(min, na.rm = TRUE)
df %>% map_dbl(max, na.rm = TRUE)
df %>% map_dbl(sd, na.rm = TRUE)

# Create a summary table
df %>% 
    colnames() %>%
    tibble(variable = .,
    mean = df %>% map_dbl(., mean, na.rm = TRUE),
    sd = df %>% map_dbl(sd, na.rm = TRUE))

                    # # A tibble: 5 × 3
                    #   variable            mean     sd
                    #   <chr>              <dbl>  <dbl>
                    # 1 ActualElapsedTime  129.    59.3
                    # 2 AirTime            108.    56.6
                    # 3 Distance           788.    454.
                    # 4 TaxiIn             6.10    3.96
                    # 5 TaxiOut            15.1    7.74

# map_int()     - Returns an integer vector
# Simple list
list <- list(a = 1,
             b = "word",
             v = 1:10,
             df = mpg)

# Return length of each list object
list %>% map_int(., length)

                    #  a  b  v df
                    #  1  1 10 11

# map_dfr()     - Returns a data frame (row bind)
# Summaries
df %>% map_dfc(., mean, na.rm = TRUE)

# ------------------------------ Map Shortcuts ------------------------------
# You can use map's argument, ".f" to have lesser lines of code
# Extract elements using a string:              map_*(.x, "<component name>")
# Extract elements by position:                 map_*(.x, <position index>)
# Anonymous functions:                          map_*(.x, function(df) <function body)           map_*(.x, ~<function body>)
# Anonymous functions (Example with lm()):      map_*(.x, function(df) lm(y~x, data = df))       map_*(.x, ~lm(y~x), data = .)

# Prepare data
df.mpg <- mpg %>%
    select(hwy, displ, cyl) %>%
    mutate(cyl = as.factor(cyl))

# Fit model - map() no shortcuts (Linear Model)
models <- df.mpg %>%
    split(.$cyl) %>%
    map(function(df) lm(formula = hwy ~ displ, 
                        data = df))

            # Coefficients:
            # (Intercept)        displ
            #      10.974        1.296

# Fit model - map() with shortcuts (Linear Model)
models <- df.mpg %>%
    split(.$cyl) %>%
    map(~lm(hwy ~ displ, 
            .))

            # Coefficients:
            # (Intercept)        displ
            #      10.974        1.296

# Extract R squared for each model - Longer syntax
models %>%
    map(summary) %>%
    map_dbl(~.$"r.squared")

# Extract R squared for each model - Shorter syntax
models %>%
    map(summary) %>%
    map_dbl("r.squared")

# Shortcuts for extracting elements by position
list <- list(list(1:3, 4:6, 7:9), 
             list(10:12, 13:15, 16:18),
             list(19:21, 22:24, 25:27))

# Extract third element from each sub-list and put the extraction inside of a list
list %>% map(3)
