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
# *Tidy Evaluation - A framework for doing non-standard evaluation in R.  
# *Quosure - An expression that has been saved in an environment (closure).  It can be evaluated later in the stored environment. 
# *Quotation  - Stores an expression without evaluating it. 
# *Quasiquotation - Quoting some parts of an expression while evaluating and then inserting the results of others (unquoting them)
# *Quoted Expression - An expression that has been saved by itself.  It can be evaluated later to return result that will depend on the environment it is evaluated in.

# ------------------------------ Tidy Evaluation: rlang ~ Quosure ------------------------------
# Quote expression or expressions as a quosure(s):
#   *rlang::quo(expression)
#   *rlang:quos(expressions)

# Call from within a function to quote what the user passed to an argument(s) as a quosure(s):
#   *rlang::enquo(argument)
#   *rlang::enquos(arguments)

# ------------------------------ Tidy Evaluation: rlang ~ Quasiquotation ------------------------------
# Unquote an expression                                                                                     "!!": 
#   *expr(log(!!a + b))

# Unquote a vector or list and splice the results as arguments into surrounding cell                        "!!!":
#   *expr(log(!!!x))

# Replace an "=" to allow unquoting within the name that appears on the left hand side of the "=".          ":=":
#   *tibble::tibble(!!n:=1)

# ------------------------------ Tidy Evaluation: rlang ~ Expressions ------------------------------
# Quote contents:
#   *rlang::expr(expression)
#   *rlang::exprs(expressions)

# Call from within a function to quote what the user passed to an argument(s) as a symbol(s):
#   *rlang::ensym(x)
#   *rlang::ensyms(...)

# ------------------------------ Tidy Evaluation: rlang ~ Quosure - Single Argument ------------------------------
data_median <- function(data, var) {
    # This function takes in a table (as "data") and a variable (as "var") and returns the median result of the variable.

    # Output error if package is not found
    require(dplyr) 

    # Capture user's argument that will be quoted 
    var <- rlang::enquo(var)

    data %>%
        # Unquote user's argument into the quoting function
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

# ------------------------------ Tidy Evaluation: rlang ~ Quosure - Multiple Arguments ------------------------------
group_median <- function(data, var, ...) {
    # This function takes in a table (as "data"), a variable (as "var"), and a grouping by varable (as "...") 
    # and returns the median result of the variable ("var") grouped by the grouping varable(s) ("...").
    # The ellipse ("...") allows for multiple arguments to be passed in allowing for group_by() to be performed multiple times.
    # Example. group_by(model) %>% group_by(manufacturer) %>% group_by(fl).

    # Output error if package is not found
    require(dplyr)

    # Capture user's argument(s) that will be quoted 
    var <- rlang::enquo(var)
    group_vars <- rlang::enquos(...)

    data %>%
        # Unquote user's arguments into the quoting function
        group_by(!!!group_vars) %>%
        # Unquote user's argument into the quoting function
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

# ------------------------------ Tidy Evaluation: (1) rlang ~ Quosure - Single Argument, (2) rlang ~ Quasiquotation, and (3) rlang ~ Expressions------------------------------
# Adding a name to the median:
named_median <- function(data, var, name) {
    # This function takes in a table (as "data"), a variable (as "var"), and a new column name (as "name").
    # It then returns the median result of the variable with the new column name ("name").

    # Output error if package is not found
    require(dplyr)

    # Capture user's argument that will be quoted 
    var <- rlang::enquo(var)
    name <- rlang::ensym(name)

    data %>%
        # Unquote user's argument into the quoting function
        summarise(!!name := median(!!var))
}

named_median(mpg, hwy, `median_hwy`)

                    # # A tibble: 1 × 1
                    #   median_hwy
                    #        <dbl>
                    # 1         24

# ------------------------------ Tidy Evaluation: (1) rlang ~ Quosure - Single Argument and (2) rlang ~ Quasiquotation ------------------------------
# Summary function
my_summary <- function(df, var, ...) {
    # This function takes in a dataframe (as "df"), a variable (as "var"), and a grouping by varable (as "...") 
    # and returns a summary of the variable ("var") grouped by the grouping varable(s) ("...").
    # The ellipse ("...") allows for multiple arguments to be passed in allowing for group_by() to be performed multiple times.
    # Example. group_by(model) %>% group_by(manufacturer) %>% group_by(fl).

    # Output error if package is not found
    require(dplyr)
    require(rlang)

    # Capture user's argument(s) that will be quoted 
    var <- enquo(var)
    group_vars <- enquos(...)

    df %>%
        # Unquote user's arguments into the quoting function
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

my_summary(mpg, hwy, model, manufacturer, fl)

                    # # A tibble: 63 × 9
                    #    model              manufacturer fl      min   max   med  mean     sd range
                    #    <chr>              <chr>        <chr> <int> <int> <dbl> <dbl>  <dbl> <int>
                    #  1 4runner 4wd        toyota       r        17    20  19.5  18.8  1.47      3
                    #  2 a4                 audi         p        26    31  29    28.3  1.98      5
                    #  3 a4 quattro         audi         p        25    28  25    25.8  1.16      3
                    #  4 a6 quattro         audi         p        23    25  24    24    1         2
                    #  5 altima             nissan       p        26    27  26.5  26.5  0.707     1
                    #  6 altima             nissan       r        27    32  30    29.8  2.22      5
                    #  7 c1500 suburban 2wd chevrolet    e        15    15  15    15   NA         0
                    #  8 c1500 suburban 2wd chevrolet    r        17    20  18.5  18.5  1.73      3
                    #  9 camry              toyota       r        26    31  28    28.3  2.14      5
                    # 10 camry solara       toyota       r        26    31  27    28.1  2.19      5
                    # # ℹ 53 more rows

# ------------------------------ Tidy Evaluation: (1) rlang ~ Quosure - Single Argument and (2) rlang ~ Quasiquotation ------------------------------
# Count frequencies function
count_freq <- function(df, ...) {
    # This function takes in a dataframe (as "df") and a grouping by varable (as "...") 
    # and returns a count of the grouping varable(s) ("...").
    # The ellipse ("...") allows for multiple arguments to be passed in allowing for group_by() to be performed multiple times.
    # Example. group_by(model) %>% group_by(manufacturer) %>% group_by(fl).

    # Output error if package is not found
    require(dplyr)
    require(rlang)

    # Capture user's arguments that will be quoted 
    group_vars <- enquos(...)
    
    df %>%
        # Unquote user's arguments into the quoting function
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

count_freq(mpg, manufacturer, model)

                    # # A tibble: 38 × 3
                    #    manufacturer model               freq
                    #    <chr>        <chr>              <int>
                    #  1 audi         a4                     7
                    #  2 audi         a4 quattro             8
                    #  3 audi         a6 quattro             3
                    #  4 chevrolet    c1500 suburban 2wd     5
                    #  5 chevrolet    corvette               5
                    #  6 chevrolet    k1500 tahoe 4wd        4
                    #  7 chevrolet    malibu                 5
                    #  8 dodge        caravan 2wd           11
                    #  9 dodge        dakota pickup 4wd      9
                    # 10 dodge        durango 4wd            7
                    # # ℹ 28 more rows

# ------------------------------ Tidy Evaluation: (1) rlang ~ Quosure - Single Argument and (2) rlang ~ Quasiquotation ------------------------------
# First create some data of infection count where "inf" is the infection count of each date:
df.infections <- tibble(date = seq.Date(from = as.Date("2021-01-01"),
                                        to = as.Date("2021-01-12"),
                                        by = "day"),
                        inf = c(100, 120, 60, 20,
                                180, 160, 150, 140,
                                100, 400, 320, 220))

# Moving average function
moving_average_infections <- function(df, var) {
    # This function takes in a dataframe (as "df") and a variable (as "var") and returns a 3 day moving average
    # and a 7 day moving average of infection count.  It also returns:
    #   *inf - infection count on that date
    #   *x1 - infection count 1 day prior
    #   *x2 - infection count 2 days prior
    #   *x3 - infection count 3 days prior
    #   *x4 - infection count 4 days prior
    #   *x5 - infection count 5 days prior
    #   *x6 - infection count 6 days prior

    # Output error if package is not found
    require(rlang)
    require(dplyr)
    require(tidyr)

    # Capture user's argument that will be quoted 
    var <- enquo(var)

    df %>%
        # Add lag of values from t-1 up to t-6
        # Unquote user's arguments into the quoting function
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

moving_average_infections(df.infections, inf)

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

# ------------------------------ Write Functions (ggplot2): (1) rlang ~ Quosure - Single Argument and (2) rlang ~ Quasiquotation ------------------------------
# Histogram:
plot_histogram <- function(df, x) {
    # This function takes in a dataframe (as "df") and a variable (as "x") and returns a histogram of the
    # count (y-axis) of the variable (x-axis).

    # Output error if package is not found
    require(ggplot2)
    require(rlang)

    # Capture user's argument that will be quoted 
    x_var <- enquo(x)

    df %>%
        # Unquote user's arguments into the quoting function
        ggplot(aes(x = !!x_var)) +
        geom_histogram()
}

plot_histogram(mpg, hwy)
plot_histogram(mpg, hwy) + ggtitle("My Histogram")

# ------------------------------ Write Functions (ggplot2): (1) rlang ~ Quosure - Single Argument and (2) rlang ~ Quasiquotation ------------------------------
# Scatter plot:
plot_scatter <- function(df, x, y) {
    # This function takes in a dataframe (as "df"), a variable (as "x"), and a variable (as "y").
    # It returns a scatter plot of x (x-axis) vs. y (y-axis).  

    # Output error if package is not found
    require(ggplot2)
    require(rlang)

    # Capture user's arguments that will be quoted 
    x_var <- enquo(x)
    y_var <- enquo(y)

    df %>%
        # Unquote user's arguments into the quoting function
        ggplot(aes(x = !!x_var,
                   y = !!y_var)) +
        geom_point()
}

plot_scatter(df = sample_n(diamonds, size = 5000), x = carat, y = price)

# ------------------------------ Write Functions (ggplot2): (1) rlang ~ Quosure - Single Argument and (2) rlang ~ Quasiquotation ------------------------------
# Generic scatter plot with a custom theme:
theme_fonts <- theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 16, face = "italic", hjust = 0.5),
    axis.text = element_text(size = 14, face = "bold", hjust = 0.5)
)

plot_scatter_custom_theme <- function(df, 
                                      x, 
                                      y, 
                                      color, 
                                      title = "", 
                                      x_title = "",
                                      y_title = "") {
    # This function takes in a dataframe (as "df"), a variable (as "x"), and a variable (as "y").
    # It also takes in custom theme variables: (1) a color theme (as "color"), (2) a custom title
    # (as "title"), (3) a custom x-axis title (as "x_title"), and (4) a custom y-axis title (as "y_title").
    # It returns a scatter plot of x (x-axis) vs. y (y-axis) with the added custom themes.

    # Output error if package is not found                                    
    require(ggplot2)
    require(rlang)
    col_var <- enquo(color)

    # Capture user's arguments that will be quoted 
    x_var <- enquo(x)
    y_var <- enquo(y)

    df %>%
        # Unquote user's arguments into the quoting function
        ggplot(aes(x = !!x_var,
                   y = !!y_var,
                   color = !!col_var)) +
        geom_point() +
        # Add custom theme to scatter plot
        ggtitle(title) +
        xlab(x_title) +
        ylab(y_title) +
        theme_minimal() +
        theme_fonts
}

plot_scatter_custom_theme(df = sample_n(diamonds, size = 5000), 
                          x = carat, 
                          y = price,
                          color = color,
                          title = "Diamond Price",
                          x_title = "carat",
                          y_title = "price in USD")

