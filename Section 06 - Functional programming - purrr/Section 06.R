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

# ------------------------------ Map Over More Than One Argument: map2() ------------------------------
?rnorm 

# distribution parameters
mu <- c(0, -4, 5, 8)
sig <- c(1, 2, 1, 3)

data <- map2(.x = mu, 
             .y = sig, 
             .f = rnorm, 
             n = 1000) %>%
    enframe() %>%
    mutate(name = paste0("norm", 1:4)) %>%
    unnest(cols = c(value))

data %>% count(name)

# Visualize
data %>%
    ggplot(aes(x = value,
               color = name,
               fill = name)) +
    geom_density(size = 1.3,
                 alpha = 0.25) +
    theme_minimal()

# ------------------------------ Map Over More Than One Argument: pmap() ------------------------------
# Distribution parameters
n <- c(100, 100, 1500, 10000)
mu <- c(0, -4, 5, 8)
sig <- c(1, 2, 1, 3)

args <- list(n, mu, sig)

# Generate data
pmap(.l = args, .f = rnorm) %>% str()

# Put inside of a tibble
args <- tibble(mean = mu,
               sd = sig,
               n = n)

# Generate data
data <- args %>%
    pmap(.l = ., .f = rnorm) %>%
    enframe() %>%
    mutate(name = paste0("norm", 1:4)) %>%
    unnest(cols = c(value))

data %>% count(name)

# Visualize
# Visualize
data %>%
    ggplot(aes(x = value,
               color = name,
               fill = name)) +
    geom_density(size = 1.3,
                 alpha = 0.25) +
    theme_minimal()

# ------------------------------ Map Over More Than One Argument: walk() ------------------------------
# Simple example with print
l <- list(1:3, c(2, 4, 6))
map(l, print)

walk(l, print)

# Create plots
plots <- mpg %>%
    split(.$manufacturer) %>%
    map(~ggplot(., aes(displ, hwy)) + geom_point() + ggtitle(paste0(.$manufacturer)))

# Create directory for plots
setwd("C:/Users/Laptop/Desktop/R - Tidyverse/Section 06 - Functional programming - purrr")
path <- "C:/Users/Laptop/Desktop/R - Tidyverse/Section 06 - Functional programming - purrr/data/pwalk_plots"

if (dir.exists(path) == FALSE) {
    dir.create(path, recursive = TRUE)

}

# Export plots
list(str_c(path, "/", names(plots), ".pdf"), plots) %>%
    pwalk(., ggsave)

# ------------------------------ Work with Lists ------------------------------
# Create a list
set.seed(123)

# Single variables
l1 <- T
l2 <- F
s1 <- words %>%
    sample(1)
s2 <- words %>%
    sample(1)
n1 <- runif(1, 0, 1000)
n2 <- runif(1, 0, 1000)

# Vectors
vec.l1 <- sample(c(T, F), size = round(runif(1, 1, 100)), replace = T)
vec.l2 <- sample(c(T, F), size = round(runif(1, 1, 100)), replace = T)
vec.s1 <- words %>% sample(size = round(runif(1, 1, 100)), replace = T)
vec.s2 <- words %>% sample(size = round(runif(1, 1, 100)), replace = T)
vec.n1 <- runif(1, 0, 1000) %>% sample(size = round(runif(1, 1, 100)), replace = T)
vec.n2 <- runif(1, 0, 1000) %>% sample(size = round(runif(1, 1, 100)), replace = T)

# Tables
t1 <- mpg
t2 <- diamonds %>% sample_n(size = 500)

# Lists
list1 <- list(a = 1, b = "b", vec = 1:10)
list2 <- list(vec = seq(0, 10, 0.05), words = words[1:10])

# Put everything into one list
list <- list(l1 = l1,
             l2 = l2,
             s1 = s1,
             s2 = s2,
             n1 = n1,
             n2 = n2,
             vec.l1 = vec.l1,
             vec.l2 = vec.l2,
             vec.s1 = vec.s1,
             vec.s2 = vec.s2,
             vec.n1 = vec.n1,
             vec.n2 = vec.n2,
             t1 = t1,
             t2 = t2)

list %>% str()

# Reshuffle elements
list.shuffled <- list[sample(1:length(list), size = length(list), replace = F)] # nolint

list <- list.shuffled
rm(list.shuffled)

# ------------------------------ Work with Lists: pluck() ------------------------------
# Get list elements at index 1
pluck(list, 1)

# Get elements from "vec.l1"
pluck(list, "vec.l1")

# ------------------------------ Work with Lists: keep() ------------------------------
list %>% map(class) %>% unlist()

# Keep only character objects
keep(list, is.character)

# Keep only logical objects
keep(list, is.logical)

# ------------------------------ Work with Lists: discard() ------------------------------
# Discard logical objects
discard(list, is.logical)

# ------------------------------ Work with Lists: flatten() ------------------------------
# Flatten our list
list.f <- flatten(list)

# ------------------------------ Work with Lists: transpose() ------------------------------
list.t <- transpose(list)
list.t %>% str()

# ------------------------------ Summarise & Join Lists: every() and sum() ------------------------------
# Every list is logical object
every(list, is.logical)

# Some lists are logical object
some(list, is.logical)

# ------------------------------ Summarise & Join Lists: has_element() ------------------------------
# Does the list have an element
has_element(list, mpg)

# ------------------------------ Summarise & Join Lists: detect() and detect_index() ------------------------------
# Find indices with numeric objects
detect(list, is.numeric)

# Find the first index with numeric object
detect_index(list, is.numeric)

# ------------------------------ Summarise & Join Lists: vec_depth() ------------------------------
# Check the depth of your list
vec_depth(list1)

# ------------------------------ Summarise & Join Lists: append() and prepend() ------------------------------
# Create smaller lists
sublist1 <- list(pluck(list, 1), pluck(list, 2))
sublist2 <- list(pluck(list, 3), pluck(list, 4))

# Append list
append(sublist1, sublist2)

# Pre-pend list
prepend(sublist1, sublist2)

# ------------------------------ Summarise & Join Lists: splice() ------------------------------
splice(l1, vec.l1, list1)

# ------------------------------ Transform Lists: modify() ------------------------------
modify(list, class)

modify_if(list, is.data.frame, nrow)

modify_at(list, "vec.s2", length)

# ------------------------------ Transform Lists: reduce() ------------------------------
list.c <- list(sample(letters, 17),
               sample(letters, 17),
               sample(letters, 17),
               sample(letters, 17))

list.n <- list(runif(n=10),
               runif(n=10),
               runif(n=10),
               runif(n=10))

# Intersection of letters
reduce(list.c, intersect)

# Calculate a cumulative sum numeric vectors
reduce(list.n, sum)

# ------------------------------ Transform Lists: accumulate ------------------------------
accumulate(list.c, intersect)

# ------------------------------ Nested Data ------------------------------
# Make a df
df <- mpg %>% 
    filter(manufacturer %in% c("jeep", "land rover", "lincoln")) %>%
    select(manufacturer, model, displ, cyl, hwy)

# Nesting df
df.n <- df %>%
    group_by(manufacturer) %>%
    nest()

                # # A tibble: 3 × 2
                # # Groups:   manufacturer [3]
                #   manufacturer data            
                #   <chr>        <list>
                # 1 jeep         <tibble [8 × 4]>
                # 2 land rover   <tibble [4 × 4]>
                # 3 lincoln      <tibble [3 × 4]>

# Unnesting df back to original df
df1 <- df.n %>%
    unnest(cols = c("data")) %>%
    ungroup()

# Operations that go with nesting
df.n$data

                # # A tibble: 3 × 4
                #   model         displ   cyl   hwy
                #   <chr>         <dbl> <int> <int>
                # 1 navigator 2wd   5.4     8    17
                # 2 navigator 2wd   5.4     8    16
                # 3 navigator 2wd   5.4     8    18

# Get length of model stored in df.n
df.n$data %>% map(.x = ., .f = ~length(.$model))
                # [[1]]
                # [1] 8

                # [[2]]
                # [1] 4

                # [[3]]
                # [1] 3

# Get average highway mpg for nested dfs
df.n$data %>% map(.x = ., .f = ~mean(.$hwy))

# Nesting mpg by manufacturer
mpg %>%
    group_by(manufacturer) %>%
    nest()

                # # A tibble: 15 × 2
                # # Groups:   manufacturer [15]
                #    manufacturer data
                #    <chr>        <list>
                #  1 audi         <tibble [18 × 10]>
                #  2 chevrolet    <tibble [19 × 10]>
                #  3 dodge        <tibble [37 × 10]>
                #  4 ford         <tibble [25 × 10]>
                #  5 honda        <tibble [9 × 10]>
                #  6 hyundai      <tibble [14 × 10]>
                #  7 jeep         <tibble [8 × 10]>
                #  8 land rover   <tibble [4 × 10]>
                #  9 lincoln      <tibble [3 × 10]>
                # 10 mercury      <tibble [4 × 10]>
                # 11 nissan       <tibble [13 × 10]>
                # 12 pontiac      <tibble [5 × 10]>
                # 13 subaru       <tibble [14 × 10]>
                # 14 toyota       <tibble [34 × 10]>
                # 15 volkswagen   <tibble [27 × 10]>

diamonds %>%
    # Nest by cut and color
    group_by(cut, color) %>%
    nest() %>%
    # Get average price and length for each tibble of data stored in a list
    mutate(`avg price` = map(data, ~mean(.$price)),
           `nr diamonds` = map(data, ~length(.$price))) %>%
    # Unlist the average price and length to get the actual result
    mutate(`avg price` = unlist(`avg price`),
           `nr diamonds` = unlist(`nr diamonds`))

# ------------------------------ Nested Data Workflow ------------------------------
df.models <- mpg %>%
    # Nest by manufacturer
    group_by(manufacturer) %>%
    nest() %>%
    # Get linear model 
    mutate(model = map(.x = data, .f = ~lm(hwy ~ displ + cyl, data = .)))

# Get audi's estimated coefficient
model <- df.models %>% 
    filter(manufacturer == "audi") %>%
    pull(model)

                # Coefficients:
                # (Intercept)        displ          cyl  
                #      34.357        3.076       -3.014

# Get the actual values of Intercept, displ, and cyl as a list
model %>% 
    # Get all data for hwy, displ, cyl
    flatten() %>%
    # Get the Intercept, displ, and cyl
    pluck(coefficients) %>%
    # Get the Intercept, displ, and cyl in a tibble
    enframe() %>%
    # Get the actual values of Intercept, displ, and cyl as a list
    .[[2]]

                # [1] 34.356580  3.076490 -3.014061

# Get the r squared value
model %>% 
    # Get model summary
    map(summary) %>%
    # Get the r squared value
    map_dbl("r.squared")

                # [1] 0.6018324

# Coefficent function 
extract_coef <- function(model, id_coef){
    coefficients(model)[[id_coef]]
}

# Get estimated coefficient for all models
df.models <- df.models %>%
    # Get the model summary each manufacturer 
    mutate(summary      = map(.x = model, .f = summary), 
           # Get the r squared value for each manufacturer
           `r squared`   = map_dbl(.x = summary, .f = "r.squared"),
           # Get the coefficient for each manufacturer using our extract_coef() function
           `coef a0`    = map_dbl(.x = summary, .f = extract_coef, 1),
           `coef a1`    = map_dbl(.x = summary, .f = extract_coef, 2),
           `coef a2`    = map_dbl(.x = summary, .f = extract_coef, 3),
           )

# Get a closer look at models where there r squared is 0
df.models %>%
    # Filter where r squared = 0
    filter(`r squared` == 0) %>%
    # Do further inspection on these manufacturers by getting their data into a tibble nested into a df
    select(manufacturer, data) %>%
    # Unnest into a dataframe
    unnest(cols = c(data)) %>%
    # Ungroup
    ungroup() %>%
    # Plot to inspect further
    ggplot(aes(x = displ,
               y = hwy,
               color = as.factor(cyl))) + geom_point() + facet_wrap(. ~manufacturer)
    # The data points are not linear like and so there was not a good model for the plot

# Get the manufacturer with the highest "r squared"
df.models %>%
    # Arrange by highest "r squared"
    arrange(desc(`r squared`)) %>%
    head(1) # Toyota



