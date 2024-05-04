# Remove previous objects:
rm(list=ls())
graphics.off()

# ------------------------------ Libraries ------------------------------
install.packages("tidyverse")
install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2")

# ------------------------------ Load Libraries ------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)

# ------------------------------ Inspect Data ------------------------------
help("mpg")
df <- mpg # Assign data frame "mpg" to df variable
# View(df)
print(df)
str(df)
nrow(df); ncol(df) # 234, 11

# ------------------------------ Manipulate Variables (Columns) ------------------------------
# Extract: manufacturer, model, year                                            select()
select(df, manufacturer, model, year)
df.car.info <- select(df, manufacturer, model, year)

# Extract: columns that begin with the letter:                                  select()
select(df, starts_with(match="m"))

# Extract: columns that contain the letter: "r"                                 select()
select(df, contains("r"))

# Extract: columns that end with the letter: "y"                                select()
select(df, ends_with("y"))

# Extract: columns by index                                                     select()
select(df, 1:3)
select(df, 2)
select(df, c(2,5,7)) # columns 2,5,7
select(df, ncol(df) - 2:ncol(df)) # columns: last 2 to end

# Rename columns                                                                rename()
df1 <- rename(df,
              mnfc=manufacturer,
              mod=model)

# Rename only 2 of the columns in one call                                      select()
df1 = select(df, 
             mnfc=manufacturer,
             mod=model,
             everything()
             )

# Add a column                                                                  mutate()
df <- mutate(df, 'avg miles per gallon'=(cty + hwy)/2)

# Add a column with a space in between the entries                              mutate()
df <- mutate(df,
             car=paste(manufacturer, model, sep=" "),
                 'cyl / trans'=paste(cyl, " cylinders", " / ", trans, " transmission", sep=""))

# Create a new variable and drop other variables                                transmute()
df1 <- transmute(df
                 'avg miles per gallon'=(cty + hwy)/2)
                

# ------------------------------ Manipulate Variables (Rows) ------------------------------
# Reset data                                                                    rm()
rm(df1, df)
df <- mpg

# Filter rows by conditions                                                     filter()
filter(df, manufacturer == "audi")
filter(df, manufacturer == "audi", year == 1999)

# Filter rows with or operator, "|"
filter(df, manufacturer == "audi" | manufacturer == "dodge")

# Filter rows by conditional, ">="
filter(df, hwy >= 30)

# Filter rows by not equal operator, "!="
filter(df, year !=1999)

# Filter rows by position                                                       slice()
slice(df, 1:5)
slice(df, 20:30)

# Filter rows by last 10 rows
slice(df, (nrow(df)-9):nrow(df))

# Filter rows by specific rows                                                  slice()
slice(df, c(1, 3, 10, 11))

# Sort rows by ascending order for a column                                     arrange()
arrange(df, year)

# Sort rows by descending order for a column                                    arrange()
arrange(df, desc(year))

# Sort rows by ascending order for multiple columns                             arrange()
df.sort <- arrange(df, year, cyl, displ)

# Create table                                                                  data.frame()
df.example <- data.frame(id = 1:3,
                         name = c("John", "Max", "Julia"))

# Appending a row to table                                                      bind_rows()
df.example <- bind_rows(df.example, slice(df.example, 2))

# Sort rows by column                                                           arrange()
df.example <- arrange(df.example, id)

# Remove duplicate rows                                                         distinct()
distinct(df.example)

# Create table with many duplicate rows
df.dupl <- select(df, manufacturer, model)

# Keep only original rows                                                       distinct()
df.nodupl <- distinct(df.dupl)

# Set seed
set.seed(567)

# Randomly select a number of rows (size) where each row can only be 
# represented 1x                                                                sample_n()
sample_n(df, size=10, replace=F)

# Randomly select a number of rows (size) where each row can be
# represented multiple times                                                    sample_n()
sample_n(df, size=10, replace=T)

# Randomly select a percentage of rows (size) where each row can only be
# represented 1x                                                                sample_fract() 
sample_frac(df, size=0.10, replace=F)

# Apply summary functions on our table and create summaries                     summarise()
summarise(df,
          'mean hwy'=mean(hwy))

# Count table rows, count distinct car models
summarise(df,
          rows=n(),
          'nr models'=n_distinct(model)
)

# Calculate min/max hwy and cty values
summarise(df,
          'min hwy'=min(hwy),
          'min cty'=min(cty),
          'max hwy'=max(hwy),
          'max cty'=max(cty)
          )

# Group column by similar rows                                                  group_by()
grouped_df = group_by(df, manufacturer)

# Combine summary statistics for grouped data                                   summarise()                  
summarise(grouped_df,
          cars=n()
          )

# Calculate min/max hwy and cty values for grouped data 
df.group_model <- group_by(df, model)
summarise(df.group_model,
          'min hwy'=min(hwy),
          'min cty'=min(cty),
          'max hwy'=max(hwy),
          'max cty'=max(cty)
          )

# Count number of table rows                                                    count()
count(df)

# Count number on a column                                                      count()
count(df, model)

# ------------------------------ Pipe Operator %>% ------------------------------
# Forward pipe operator %>% is used for "piping" (chaining) functions in a pipeline.
# The operator creates an individual object for each function.
# It enables the code to be more clean

# Pipe operator - Filter rows by condition
df %>% filter(manufacturer == "audi")

# Pipe operator - Filter rows by condition &
# get the count
df %>% 
    filter(manufacturer == "audi") %>%
    count()

# Pipe operator - Filter rows by not equal operator, "!=" &
# extract certain columns 
df %>% 
    filter(manufacturer == "dodge" | manufacturer == "chevrolet") %>%
    select(manufacturer, model, year, class)

# Pipe operator - For each manufacturer, model, class, and transsmission type get the
# 'mean hwy' and count number of cars and 
# filter results where the 'mean hwy'is greater than 30 and
# show the results in descending order based on 'mean hwy'
df %>%
    group_by(manufacturer, model, class, trans) %>%
    summarise('mean hwy' = mean(hwy), car=n()) %>%
    ungroup() %>%
    filter('mean hwy' > 30) %>%
    arrange(desc('mean hwy'))

# ------------------------------ Pivoting Tables ------------------------------
# Create table                                                                  data.frame()
table.long <- data.frame(id = 1:6,
                         type = c("a", "b", "c", "c", "b", "a"),
                         count = c(20, 40, 11, 1, 13, 3)
)

#   id type count
# 1  1    a    20
# 2  2    b    40
# 3  3    c    11
# 4  4    c     1
# 5  5    b    13
# 6  6    a     3

# Pivot the table to wide                                                       pivot_wider()
table.wide <- pivot_wider(table.long, 
                          names_from=type,
                          values_from=count
                          )

#      id     a     b     c
#   <int> <dbl> <dbl> <dbl>
# 1     1    20    NA    NA
# 2     2    NA    40    NA
# 3     3    NA    NA    11
# 4     4    NA    NA     1
# 5     5    NA    13    NA
# 6     6     3    NA    NA

# Pivot the table to long                                                       pivot_longer()
table.long1 <- pivot_longer(table.wide,
                            cols=c("a", "b", "c"),
                            names_to="type",
                            values_to="count",
                            values_drop_na=T
                            )

#      id type  count
#   <int> <chr> <dbl>
# 1     1 a        20
# 2     2 b        40
# 3     3 c        11
# 4     4 c         1
# 5     5 b        13
# 6     6 a         3

# Filter rows where manufacturer is "jeep", "land rover", hyundai
# Select model, trans., hwy
# Calculate avg. hwy for each model and trans.
# In a long table format
df.long <- df %>%
    filter(manufacturer %in% c("jeep", "land rover", "hyundai")) %>%
    select(model, trans, hwy) %>%
    group_by(model, trans) %>%
    summarise('mean hwy' = mean(hwy)) %>%
    ungroup()
df.long

# Convert long to wide format where trans. type is transformed into columns     pivot_wider()
df.wide <- df.long %>%
    pivot_wider(names_from = trans, 
                values_from = 'mean hwy')

# Convert back to long format                                                   pivot_longer()
df.long1 <- df.wide %>%
    pivot_longer(-model,    # exclude the 'model' column
                 names_to = "trans",
                 values_to = "mean hwy",
                 values_drop_na = T
                 )
    
# ------------------------------ Separating and Uniting Columns ------------------------------
# Create a table for 1 year
dates <- seq.Date(from = as.Date("2021-01-01"),
                  to = as.Date("2021-12-31"), 
                  by = "day"
                  )
table <- data.frame(date = dates)
table %>% head()
table %>% tail()

# Split one column into multiple columns                                        separate()
table.sep <- table %>%
    separate(data = .,
             col = date,
             into = c("year", "month", "dayofmonth"),
             sep = "-"
             ) %>%
    mutate(month = as.numeric(month),
           dayofmonth = as.numeric(dayofmonth)
           ) %>%
    arrange(year, month, dayofmonth)

# Combine multiple columns                                                      unite()
install.packages("stringr")
library(stringr)

table.unite <- table.sep %>%
    # Add leading zeroes back to dayofmonth and month
    mutate(dayofmonth = str_pad(dayofmonth, width = 2, side = "left", pad = "0"), 
           month = str_pad(month, width = 2, side = "left", pad = "0"), 
           ) %>%
    unite(data = ., 
          col = 'date',
          year, month, dayofmonth,
          sep = '-'
          ) %>%
    arrange()

# ------------------------------ dplyr and tidyr ------------------------------
# Extract a column as a vector                                                  pull()
df %>% pull(hwy)

# Calculate average hwy per car manufacturer and car model
df <- df %>%
    group_by(manufacturer, model) %>%
    mutate('mean hwy' = mean(hwy)) %>%
    ungroup()

# Add variable "transmission type": automatic or manual
df %>% count(trans)
df <- df %>%
    mutate(trans_ = str_sub(string = trans, 
                            start = 1, 
                            end = 1)) %>%
    mutate('transmission type' = case_when(trans_ == "a" ~ "automatic",
                                           trans_ == "m" ~ "manual",
                                           TRUE ~ "NA")) %>%
    select(-trans_)

df %>% count('transmission type', trans)
# Add a row number                                                              row_number()
df <- df %>%
    mutate('car id' = row_number())

# Add a row number after grouping                                               row_number()
df <- df %>%
    group_by(manufacturer) %>%
    mutate('car id 1' = row_number()) %>%
    ungroup()

# Remove all tables
rm(list = ls())

# Flights data
install.packages("hflights")
library(hflights)
df <- hflights

# Number of rows and columns
nrow(hflights); ncol(hflights)

df %>%
    count(UniqueCarrier, FlightNum, TailNum, Year, Month, DayofMonth)

# How many columns begin with the word "Taxi"
df %>% >
    select(starts_with("Taxi"))

# How many flights were flown with < 1000 miles and >= 1000 miles
df %>%
    mutate(dist1000 = case_when(Distance < 1000 ~ "< 1000 miles",
                                Distance >= 1000 ~ ">= 1000 miles")) %>%
    count(dist1000)

# Flights per carrier - sorted top to bottom
df %>%
    group_by(UniqueCarrier) %>%
    count() %>%
    ungroup() %>%
    arrange(desc(n))

# Number of cancelled flights for each carrier
df %>%
    filter(Cancelled == 1) %>%
    group_by(UniqueCarrier) %>%
    count() %>%
    ungroup() %>%
    arrange(desc(n))

# Percentage of cancelled flights for each carrier
df %>%
    # Number of cancelled flights per carrier
    group_by(UniqueCarrier, Cancelled) %>%
    count() %>%
    ungroup() %>%
    # Total number of flights per carrier
    group_by(UniqueCarrier) %>%
    mutate('n tot' = sum(n)) %>%
    ungroup() %>%
    # Calculate percentage of cancelled flights for each carrier
    mutate('n percent %' = (n / `n tot` ) * 100) %>%
    # Keep only cancelled flights, arranged from top to bottom
    filter(Cancelled == 1) %>%
    arrange(desc(`n percent %`))

# Create a date column combining the year + month + dayofmonth
df <- df %>%
    # Add leading zeroes
    mutate_at(.vars = c("Month", "DayofMonth"),
              .funs = str_pad, 2, "left", "0") %>%
    unite(col = "Date",
          Year, Month, DayofMonth,
          sep = "-")

# Count flights per cancelled codes and per carriers
df %>% count(CancellationCode)
df %>% mutate(CancellationCode = case_when(CancellationCode == "" ~ "0",
                                           TRUE ~ CancellationCode)) %>%
    group_by(UniqueCarrier, CancellationCode) %>%
    count() %>%
    ungroup() %>%
    pivot_wider(names_from = CancellationCode,
                values_from = n,
                values_fill = 0
                )

View(df)
