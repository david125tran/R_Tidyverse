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
library(readr)

# ------------------------------ Exercise 1 ------------------------------
continents <- tibble("Date" = as.Date(c("2017-11-10", "2017-11-10", "2017-11-10", "2017-11-10", "2017-11-10", "2017-11-10", "2017-11-10")),
                     "Continent" = c("Africa", "Antartica", "Asia", "Europe", "North America", "South America", "Australia"),
                     "Area (km2)" = c(30370000, 14000000, 44579000, 10180000, 24709000, 17840000, 8600000),
                     "Percent total landmass" = c(20.4, 9.2, 29.5, 6.8, 16.5, 12.0, 5.9),
                     "Population" = c(1287920000, 4490, 4545133000, 742648000, 587615000, 428240000, 41264000),
                     "Percent total population" = c(16.9, 0.1, 59.5, 9.7, 7.7, 5.6, 0.5)
                     )

                    # # A tibble: 7 × 6
                    #   Date       Continent     `Area (km2)` `Percent total landmass` Population
                    #   <date>     <chr>                <dbl>                    <dbl>      <dbl>
                    # 1 2017-11-10 Africa            30370000                     20.4 1287920000
                    # 2 2017-11-10 Antartica         14000000                      9.2       4490
                    # 3 2017-11-10 Asia              44579000                     29.5 4545133000
                    # 4 2017-11-10 Europe            10180000                      6.8  742648000
                    # 5 2017-11-10 North America     24709000                     16.5  587615000
                    # 6 2017-11-10 South America     17840000                     12    428240000
                    # 7 2017-11-10 Australia          8600000                      5.9   41264000
                    # # ℹ 1 more variable: `Percent total population` <dbl>

continents_summary <- continents %>%
    summarise(`total area` = sum(`Area (km2)`),
              `total population` = sum(`Population`),
              `sum of percentage - total landmass` = sum(`Percent total landmass`),
              `sum of percentage - total population` = sum(`Percent total population`)
              ) 

                    # # A tibble: 1 × 4
                    #   `total area` `total population` sum of percentage - t…¹ sum of percentage - …²
                    #          <dbl>              <dbl>                   <dbl>                  <dbl>
                    # 1    150278000         7632824490                    100.                    100
                    # # ℹ abbreviated names: ¹​`sum of percentage - total landmass`,
                    # #   ²​`sum of percentage - total population`

# ------------------------------ Exercise 2 ------------------------------
setwd("C:/Users/Laptop/Desktop/R - Tidyverse/Section 02 - Data import (readr & tibble)/data/Exercise")
df2 <- read_csv(file = "./flights_02.csv")

# Check structure of csvv
str(df2)

# ------------------------------ Exercise 3 ------------------------------
df3 <- read_delim(file = "./flights_03.csv", 
                  col_names = F,
                  col_types = cols(.default = "c"),
                  comment = "#",
                  skip = 12,    
                  delim = "|") %>% # For the delimiter to get picked up correctly, you have to skip and land on the very first row where the data starts.
                  # Assign column names
                  rename(`UniqueCarrier` = 1,
                         `FlightNum` = 2,
                         `Date` = 3,
                         `Origin` = 4,
                         `Dest` = 5,
                         `Distance` = 6)
                  
str(df3)
View(df3)

# ------------------------------ Exercise 4 ------------------------------
df4 <- read_csv(file = "./big_table04")