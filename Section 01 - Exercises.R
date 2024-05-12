# Remove previous objects:
rm(list=ls())
graphics.off()

# ------------------------------ Libraries ------------------------------
install.packages("tidyverse")
install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("stringr")
install.packages("lubridate")
# ------------------------------ Load Libraries ------------------------------
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(lubridate)
# ------------------------------ Exercise 1 ------------------------------
# Flights data
install.packages("hflights")
library(hflights)
df <- hflights

# How many rows and columns are in table hflights?
nrow(hflights); ncol(hflights)

# How many different carriers are listed in the table (print a table with distinct carrier names)?
n_distinct(df$UniqueCarrier)

# Which and how many airports were involved? Consider both origin and destination
# airports!
df1 <- df %>%
    # Get only the 'Origin' and 'Dest' columns
    select(Origin, Dest) %>%
    # Combine 'Origin' and 'Dest' columns into "orig/dest"
    pivot_longer(cols = everything(), 
                 names_to = "orig/dest",
                 values_to = "airport") %>%
    # Keep only the unique airports
    distinct(airport) %>%
    # Sort by airport in ascending order 
    arrange(airport) 

View(df1)

# How many flights were cancelled?
df %>%
    filter(Cancelled == 1) %>%
    count()

# ------------------------------ Exercise 2 ------------------------------
# First, produce a table where statistics for each carrier is shown:
# * number of flights per carrier
# * total distance flown in miles per carrier
# * total actual elapsed time in hours per carrier
# * total air time in hours per carrier
# * mean distance per flight for each carrier
# * mean actual elapsed time in hours per flight for each carrier
# * mean air time in hours per flight for each carrier

df2 <- df %>%
    group_by(UniqueCarrier) %>%
    summarise('number of flights'=n(),
              'total distance flown (mi)'=sum(Distance),
              'total actual elapsed time'=round(sum(ActualElapsedTime, na.rm=T)/60, 1),
              'total air time (hrs)'=round(sum(AirTime, na.rm=T)/60, 1),
              'mean distance'=mean(Distance),
              'mean actual elapsed time (hrs)'=round(mean(ActualElapsedTime, na.rm=T)/60, 1),
              'mean air time (hrs)'=round(mean(AirTime, na.rm=T)/60, 1)
              )

View(df2)

# Second, calculate the percentage of total distance flown by top 3 performing carriers VS total
# distance flown by remaining carriers. Execute steps:
# * first rank carriers by total distance flown
# * top 3 performers are in one group, remaining carriers are in second group
# * for each group calculate total distance flown
# * for each group calculate %: (total distance flown per group/# total distance all carriers)

df3 <- df2 %>%
    # Only look at 'Unique Carrier' and 'total distance flown (mi)' column
    select(UniqueCarrier, 
           distance = 'total distance flown (mi)') %>%
    # Sort by 'total distance flown (mi)' from highest to lowest
    arrange(desc(distance)) %>%
    # Split the groups by: top 3 performers vs. the rest
    mutate(rank = row_number(),
           group = case_when(rank <= 3 ~ "top performer",
                             TRUE ~ "the rest")) %>%
    group_by(group) %>%
    summarise(carriers = n(),
              distance = sum(distance)) %>%
    # Ungroup 
    ungroup() %>%
    # Get total distance flown per group/# total distance all carriers percentage
    mutate(`distance %` = distance / sum(distance) * 100)

View(df3)

# ------------------------------ Exercise 3 ------------------------------
# Modify your main flights table:
# * create date column by uniting columns: year, month, day of month
# * when uniting columns do not lose source columns (mutate each column - with slightly
#   different name, before unite operation is executed)
# * you will need to parse date column after unite operation
# * also you should add leading zeros to month and day of month column before date is
#   created
# * create columns: quarter, week

df4 <- df %>%
    # Make new columns 
    mutate(Mon = as.numeric(Month),
           Day = as.numeric(DayofMonth),
           Yea = as.numeric(Year)
           ) %>%
    # Add leading zeroes
    mutate_at(.vars = c("Mon", "Day"),
              .funs = str_pad, 2, "left", "0") %>%
    # Unite into date and remove the columns (Mon, Day, Yea)
    unite(col = "Date",
          Yea, Mon, Day,
          sep = "-") %>%
    # Add week column
    mutate('week' = case_when(DayofMonth < 8 ~ 1,
                              DayofMonth < 15 ~ 2,
                              DayofMonth < 22 ~ 3,
                              TRUE ~ 4)) %>%
    # Add quarter column
    mutate('quarter' = case_when(Month < 4 ~ 1,
                                 Month < 7 ~ 2,
                                 Month < 10~ 3,
                                 TRUE ~ 4
                              ))

# * Is total number of flights increasing or decreasing quarterly?
df4 %>%
    count(quarter)

# * Is total distance increasing or decreasing monthly?
df5 <- df %>%
    group_by(Month) %>%
    mutate(`total distance` = sum(Distance)) %>%
    distinct(Month, `total distance`)

View(df5)


