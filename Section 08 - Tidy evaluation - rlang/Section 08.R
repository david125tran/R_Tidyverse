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
library(tidyverse)
library(rlang)

# ------------------------------ Tidy Evaluation ------------------------------
# Tidy Evaluation - A framework for doing non-standard evaluation in R.  
# Quotation  - Stores an expression without evaluating it. 
# Quasiquotation - Quoting some parts of an expression while evaluating and then inserting the
# results of others (unquoting them)
# Quoted expression - An expression that has been saved by itself.  It can be evaluated later to
# return result that will depend on the environment it is evaluated in.

# Unquote an expression                                                                                     "!!": 
#   *expr(log(!!a + b))

# Unquote a vector or list and splice the results as arguments into surrounding cell                        "!!!":
#   *expr(log(!!!x))

# Replace an "=" to allow unquoting within the name that appears on the left hand side of the "=".          ":=":
#   *tibble::tibble(!!n:=1)

# ------------------------------ Programming Recipes ------------------------------
# Programming with quoting function
data_median <- function(data, var) {
    require(dplyr)

    var <- rlang::enquo(var)

    data %>%
        # Unquote "var"
        summarise(median = median(!!var))
}

data_median(mpg, hwy)

                    # # A tibble: 1 × 1
                    #   median
                    #    <dbl>
                    # 1     24

data_median(diamonds, price)

                    # # A tibble: 1 × 1
                    #   median
                    #    <dbl>
                    # 1   2401

# Multiple arguments in quoting function
group_median <- function(data, var, ...) {
    require(dplyr)

    var <- rlang::enquo(var)
    # Capture grouping variables
    group_vars <- rlang::enquos(...)

    data %>%
        group_by(!!!group_vars) %>%
        # Unquote "var"
        summarise(median = median(!!var)) %>%
        ungroup()
}

group_median(mpg, hwy, model)

                    # # A tibble: 38 × 2
                    #    model              median
                    #    <chr>               <dbl>
                    #  1 4runner 4wd          19.5
                    #  2 a4                   29
                    #  3 a4 quattro           25
                    #  4 a6 quattro           24
                    #  5 altima               28
                    #  6 c1500 suburban 2wd   17
                    #  7 camry                28
                    #  8 camry solara         27
                    #  9 caravan 2wd          23
                    # 10 civic                32
                    # # ℹ 28 more rows

group_median(mpg, hwy, manufacturer)

                    # # A tibble: 15 × 2
                    #    manufacturer median
                    #    <chr>         <dbl>
                    #  1 audi           26  
                    #  2 chevrolet      23
                    #  3 dodge          17
                    #  4 ford           18
                    #  5 honda          32
                    #  6 hyundai        26.5
                    #  7 jeep           18.5
                    #  8 land rover     16.5
                    #  9 lincoln        17
                    # 10 mercury        18
                    # 11 nissan         26
                    # 12 pontiac        26
                    # 13 subaru         26  
                    # 14 toyota         26
                    # 15 volkswagen     29

group_median(mpg, hwy, model, manufacturer, fl)

                    # # A tibble: 63 × 4
                    #    model              manufacturer fl    median
                    #    <chr>              <chr>        <chr>  <dbl>
                    #  1 4runner 4wd        toyota       r       19.5
                    #  2 a4                 audi         p       29
                    #  3 a4 quattro         audi         p       25
                    #  4 a6 quattro         audi         p       24
                    #  5 altima             nissan       p       26.5
                    #  6 altima             nissan       r       30
                    #  7 c1500 suburban 2wd chevrolet    e       15
                    #  8 c1500 suburban 2wd chevrolet    r       18.5
                    #  9 camry              toyota       r       28
                    # 10 camry solara       toyota       r       27
                    # # ℹ 53 more rows

# Argument names of a quoting function
named_median <- function(data, var, name) {
    require(dplyr)

    var <- rlang::enquo(var)
    name <- rlang::ensym(name)

    data %>%
        summarise(!!name := median(!!var))
}

named_median(mpg, hwy, `median_hwy`)

                    # # A tibble: 1 × 1
                    #   median_hwy
                    #        <dbl>
                    # 1         24

# ------------------------------ Write Functions: dplyr ------------------------------
# Summary function
my_summary <- function(df, var, ...) {
    require(dplyr)
    require(rlang)

    var <- enquo(var)
    group_vars <- enquos(...)

    df %>%
        group_by(!!!group_vars) %>%
        summarise(min = min(!!var),
                  max = max(!!var),
                  med = median(!!var),
                  mean = mean(!!var),
                  sd = sd(!!var),
                  range = max - min
                  ) %>%
        ungroup()
}

my_summary(mpg, hwy, model)

                    # # A tibble: 38 × 7
                    #    model                min   max   med mean    sd range
                    #    <chr>              <int> <int> <dbl> <dbl> <dbl> <int>
                    #  1 4runner 4wd           17    20  19.5  18.8  1.47     3
                    #  2 a4                    26    31  29    28.3  1.98     5
                    #  3 a4 quattro            25    28  25    25.8  1.16     3
                    #  4 a6 quattro            23    25  24    24    1        2
                    #  5 altima                26    32  28    28.7  2.42     6
                    #  6 c1500 suburban 2wd    15    20  17    17.8  2.17     5
                    #  7 camry                 26    31  28    28.3  2.14     5
                    #  8 camry solara          26    31  27    28.1  2.19     5
                    #  9 caravan 2wd           17    24  23    22.4  2.06     7
                    # 10 civic                 29    36  32    32.6  2.55     7
                    # # ℹ 28 more rows

# Count frequencies function
count_freq <- function(df, ...) {
    require(dplyr)
    require(rlang)

    group_vars <- enquos(...)
    
    df %>%
        group_by(!!!group_vars) %>%
        summarise(freq = n()) %>%
        ungroup()
}

count_freq(mpg)

                    # # A tibble: 1 × 1
                    #    freq
                    #   <int>
                    # 1   234

count_freq(mpg, manufacturer)

                    # # A tibble: 15 × 2
                    #    manufacturer  freq
                    #    <chr>        <int>
                    #  1 audi            18
                    #  2 chevrolet       19
                    #  3 dodge           37
                    #  4 ford            25
                    #  5 honda            9
                    #  6 hyundai         14
                    #  7 jeep             8
                    #  8 land rover       4
                    #  9 lincoln          3
                    # 10 mercury          4
                    # 11 nissan          13
                    # 12 pontiac          5
                    # 13 subaru          14
                    # 14 toyota          34
                    # 15 volkswagen      27

# Moving average function
# First create some data:
df.infections <- tibble(date = seq.Date(from = as.Date("2021-01-01"),
                                        to = as.Date("2021-01-12"),
                                        by = "day"),
                        inf = c(100, 120, 60, 20,
                                180, 160, 150, 140,
                                100, 400, 320, 220))

moving_average_infections <- function(df = df.infections, var = inf) {
    require(rlang)
    require(dplyr)
    require(tidyr)

    var <- enquo(var)

    df %>%
        # Add lag of values from t-1 up to t-6
        mutate(x1 = lag(!!var, 1),
               x2 = lag(!!var, 2),
               x3 = lag(!!var, 3),
               x4 = lag(!!var, 4),
               x5 = lag(!!var, 5),
               x6 = lag(!!var, 6)) %>%
        # Replace NA with 0
        mutate_at(., .vars = paste0("x", 1:6), .funs = replace_na, 0) %>%
        # Calculate moving averages
        mutate(`3 day avg` = (!!var + x1 + x2) / 3,
               `7 day avg` = (!!var + x1 + x2 + x3 + x4 + x5 + x6) / 7)
}

moving_average_infections()

                    # # A tibble: 12 × 10
                    #    date         inf    x1    x2    x3    x4    x5    x6 `3 day avg` `7 day avg`
                    #    <date>     <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>       <dbl>       <dbl>
                    #  1 2021-01-01   100     0     0     0     0     0     0        33.3        14.3
                    #  2 2021-01-02   120   100     0     0     0     0     0        73.3        31.4
                    #  3 2021-01-03    60   120   100     0     0     0     0        93.3        40
                    #  4 2021-01-04    20    60   120   100     0     0     0        66.7        42.9
                    #  5 2021-01-05   180    20    60   120   100     0     0        86.7        68.6
                    #  6 2021-01-06   160   180    20    60   120   100     0       120          91.4
                    #  7 2021-01-07   150   160   180    20    60   120   100       163.        113. 
                    #  8 2021-01-08   140   150   160   180    20    60   120       150         119.
                    #  9 2021-01-09   100   140   150   160   180    20    60       130         116.
                    # 10 2021-01-10   400   100   140   150   160   180    20       213.        164.
                    # 11 2021-01-11   320   400   100   140   150   160   180       273.        207.
                    # 12 2021-01-12   220   320   400   100   140   150   160       313.        213.

