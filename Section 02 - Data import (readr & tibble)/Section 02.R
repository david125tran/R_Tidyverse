# Remove previous objects:
rm(list = ls())
graphics.off()

# ------------------------------ Libraries ------------------------------
install.packages("languageserver")
install.packages("lintr")
install.packages("tidyverse")
install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("tibble")
install.packages("readr")
install.packages("readxl")
install.packages("rio")
install.packages("data.table")
install.packages("feather")

# ------------------------------ Load Libraries ------------------------------
library(lintr)
library(languageserver)
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(tibble)
library(readr)
library(readxl)
library(rio)
library(data.table)
library(feather)

# ------------------------------ Tibble ------------------------------
# tibble:
#   * "New generation" class for storing tabular data.
#   * Similar to old table but look alike type called "data.frame", but are enhanced in some ways.
#
# tibble vs data.frame:
#   * display - when printed to the console, R gives very compact view of the data (fitting on the PC screen)
#   * subsetting - "[" always returns a new tibble, "[[" and "$" always returns a vector
#   * no partial matching - full column names must be used for subsetting
#
# Some older R libraries and functions don't work with tibbles. If this happens, convert the tibble
# back to a data.frame

ggplot2::diamonds
class(ggplot2::diamonds)    # "tbl_df"     "tbl"        "data.frame"

ggplot2::economics
class(ggplot2::economics)   # "spec_tbl_df" "tbl_df"      "tbl"         "data.frame"

hflights::hflights
class(hflights::hflights)   # "data.frame"

# ------------------------------ Create a Tibble ------------------------------
# Adding non-syntatic column names
#   * You are able to use not-valid R variable names.  Ex. `*`, `3.14`, `;`
#   * You just have to wrap these column names with backticks, ``
#
# tibble::tribble
#   * A transposed tibble.
#   * Function that enables customized data entry - table like data input.
#   * Column headings are referred with tilde ~

# Convert hflights data frame to tibble
library(hflights)
class(hflights)             # "data.frame"

dft <- as_tibble(hflights)
class(dft)                  # "tbl_df"     "tbl"        "data.frame"

# Convert a custom data frame to tibble
df <- data.frame(x = 1:10,
                 y = seq.Date(from = as.Date("2021-01-01"),
                              to = as.Date("2021-01-10"),
                              by = "day"))

                                #     x          y
                                # 1   1 2021-01-01
                                # 2   2 2021-01-02
                                # 3   3 2021-01-03
                                # 4   4 2021-01-04
                                # 5   5 2021-01-05
                                # 6   6 2021-01-06
                                # 7   7 2021-01-07
                                # 8   8 2021-01-08
                                # 9   9 2021-01-09
                                # 10 10 2021-01-10

class(df)                   # "data.frame"

dft <- as_tibble(df)
class(dft)                  # "tbl_df"     "tbl"        "data.frame"

# Create a custom tibble
tibble(v1 = seq(from = 1, to = 100, by = 1),
        v2 = pi,
        v3 = sqrt(v1),
        v4 = seq.Date(from = as.Date("2021-01-01"),
                      length.out = 100,
                      by = "day"))

                                # # A tibble: 100 × 4
                                #       v1    v2    v3 v4
                                #    <dbl> <dbl> <dbl> <date>
                                #  1     1  3.14  1    2021-01-01
                                #  2     2  3.14  1.41 2021-01-02
                                #  3     3  3.14  1.73 2021-01-03
                                #  4     4  3.14  2    2021-01-04
                                #  5     5  3.14  2.24 2021-01-05
                                #  6     6  3.14  2.45 2021-01-06
                                #  7     7  3.14  2.65 2021-01-07
                                #  8     8  3.14  2.83 2021-01-08
                                #  9     9  3.14  3    2021-01-09
                                # 10    10  3.14  3.16 2021-01-10
                                # # ℹ 90 more rows
            
# Create a custom tibble with strange names
tibble(`123` = 123,
       `.` = "period",
       `,` = "comma",
       `,*/-+?!` = "strange name")
                                    
                                # # A tibble: 1 × 4
                                #   `123` .      `,`   `,*/-+?!`
                                #   <dbl> <chr>  <chr> <chr>
                                # 1   123 period comma strange name

# Create a custom tribble
tribble(
    ~name, ~surname, ~male, ~age,   # header
    #---------------------------#
    "Max", "Smith", TRUE, 35,
    "Liz", "Brown", FALSE, 24
)

                                # # A tibble: 2 × 4
                                #   name  surname male    age
                                #   <chr> <chr>   <lgl> <dbl>
                                # 1 Max   Smith   TRUE     35
                                # 2 Liz   Brown   FALSE    24

# ------------------------------ Extracting Variables (Subsetting) ------------------------------
# By name:      df$var_name
# By name:      df[["var name"]]
# By index:     df[[2]]
#
# When extracting variables in a pipe, a special placeholder is used, "."
# Examples:
# <data frame> %>%
#   .$var_name
#
# <data frame> %>%
#   .[[var_name]]
#
# <data frame> %>%
#   .[[2]]

# ------------------------------ data.frame vs. tibble ------------------------------
# hflights %>%
#    as_tibble()

# hflights

# Extract by name
mpg$model %>%
    class()                 # "character"

mpg[["model"]] %>%
    class()                 # "character"

# Extract by index
mpg[[2]] %>%
    class()                 # "character"

# Extract using pipe operator by name
mpg %>%
    .$model %>%
        class()             # "character"

mpg %>%
    .[["model"]] %>%
        class()             # "character"

# Extract using pipe operator by index
mpg %>%
    .[[2]] %>%
        class()             # "character"

# ------------------------------ readr ------------------------------
# readr - Tools for importing rectangular data faster
#
# readr vs. base R functions
#   * Speed - readr's functions are up to 10x faster than base R functions like read.csv()
#   * tibbles - They produce tibbles at import
#   * Reproducibility - Base R functions inherit some OS behavior from your PC and might
#     not work on other PCs, readrs tools try to avoid that behavior
#
# read_csv()    comma separated (CSV) flat files
# read_tsv()    tab (tabulator) separted flat files
# read_delim()  general delimited files
# read_fwf()    fixed with files
# read_table()  tabular files where columns are separated by white space
# read_log()    web log files
#
# readr::read_csv       ->  for "," separated files
# readr::read_csv2      ->  for ";" separated files

# ------------------------------ Read Files ------------------------------

# Read inline csv file
read_csv("c1, c2, c3
         1, a, T
         2, b, T
         3, c, F"
         )

                                # # A tibble: 3 × 3
                                #      c1 c2    c3
                                #   <dbl> <chr> <chr>
                                # 1     1 a     T
                                # 2     2 b     T
                                # 3     3 c     F

# Inline files with meta header lines
read_csv("First meta line
         Second meta line
         c1, c2, c3
         1, a, T
         2, b, T
         3, c, F", skip = 2
         )

                                # # A tibble: 3 × 3
                                #      c1 c2    c3
                                #   <dbl> <chr> <lgl>
                                # 1     1 a     TRUE
                                # 2     2 b     TRUE
                                # 3     3 c     FALSE

# Inline files with comments
read_csv("c1, c2, c3    # Comment
         1, a, T        # Comment
         2, b, T
         3, c, F",
         comment = "#")

                                # # A tibble: 3 × 3
                                #      c1 c2    c3
                                #   <dbl> <chr> <lgl>
                                # 1     1 a     TRUE
                                # 2     2 b     TRUE
                                # 3     3 c     FALSE

# Read csv from computer's disk
getwd()
setwd("C:/Users/Laptop/Desktop/R - Tidyverse/Section 02 - Data import (readr & tibble)")

list.files(path = "C:/Users/Laptop/Desktop/R - Tidyverse/Section 02 - Data import (readr & tibble)/data/02_05_read_files")
list.files(path = "./data/02_05_read_files")

# Read csv file 
df <- read_csv(file = "./data/02_05_read_files/mpg_mini.csv")

# Read csv file with column separator ";"
df <- read_csv2(file = "./data/02_05_read_files/mpg_mini2.csv")

# Read tsv file (tab delimited file)
df <- read_tsv("./data/02_05_read_files/mpg.tsv")

# Read files with a custom delimiter
df <- read_delim(file = "./data/02_05_read_files/mpg_delim.txt", delim = "~")

# Read txt files
df <- read_delim(file = "./data/02_05_read_files/mpg.txt", col_names = T,
                                                           skip = 3,                # Skip the first 3 rows
                                                           skip_empty_rows = T,
                                                           delim = " ",             
                                                           quote = "\"")            # Escape quotes

# Read log file
df <- read_log(file = "./data/02_05_read_files/example.log")

# Check execution time of reading a file
system.time(
    df <- read_csv(file = "./data/02_05_read_files/mpg_maxi.csv")
)

# ------------------------------ Vector Parsing ------------------------------

# Parse a character vector
parse_character(c("Hannah", "Lexi", "Tyrone", "Peter"))

# Parse a logical vector
parse_logical(c("T", "TRUE", "F", "FALSE", "NA"))

# Parse an integer vector
parse_integer(c("10", "15", "20"))

# Parse a factor vector
parse_factor(c("a", "b", "c"), levels = c("b", "a"))

# Parse a double vector
parse_double(c("3.14", "1.72"))

# Parse a different decimal mark vector
parse_double(c("3,14", "1,72"), locale = locale(decimal_mark = ","))

# Parse a bunch of different numbers vector
parse_number(c("1", "2.2", "$100", "5%", "0.05", "1,000"))

# Parse specific grouping marks vector
parse_number(c("100,000.25"), locale = locale(decimal_mark = ","))

# Parse date vector
parse_date(c("2021-01-31", "2024-05-19"))

# Parse a date vector with a specific format
parse_date(c("5-19-24"), format = "%m-%d-%y")
parse_date(c("5/19/24"), format = "%m/%d/%y")
parse_date(c("19/5/24"), format = "%d/%m/%y")

# Parse a time vector
parse_time(c("00:01", "01:05:03"))

# Parse a datetime vector
parse_datetime("2024-05-19 09:03:30")

# ------------------------------ File Parsing ------------------------------

# guess parser heuristic - Used to guess column data types 
guess_parser(c("T", "False"))           # "logical"
guess_parser(c("T", "False", "dog"))    # "character"

# Read csv file and declare data types for columns
df <- read_tsv(file = "./data/02_05_read_files/mpg.tsv",
               col_types = cols(manufacturer = col_factor(),
                                model = col_factor(),
                                displ = col_double(),
                                year = col_integer(),
                                cyl = col_integer(),
                                trans = col_character(),
                                drv = col_character(),
                                cty = col_number(),
                                hwy = col_number(),
                                fl = col_character(),
                                class = col_character()
                                ))

# Import table without specifying the column types and then change the column type
df <- read_tsv(file = "./data/02_05_read_files/mpg.tsv") %>%
    mutate_at(.vars = c("year", "cyl"), .funs = as.integer) %>%         # convert columns to integer
    mutate_at(.vars = c("manufacturer", "model"), .funs = as.factor)    # convert columns to factor

# ------------------------------ Other Useful Libraries ------------------------------

# Read Excel Files (.xlsx) with read_excel()
read_excel(path = "./data/02_08_other_useful_import_libraries/mpg.xlsx")

# Read Excel Files (.xlsx) - Specific Sheet with read_excel()
read_excel(path = "./data/02_08_other_useful_import_libraries/mpg.xlsx", sheet = "Sheet 1")

# Read Excel Files (.xlsx) - Specific Cells with read_excel()
read_excel(path = "./data/02_08_other_useful_import_libraries/mpg.xlsx", range = "A1:C10")

# Read Excel Files (.xlsx) with rio
rio::import(file = "./data/02_08_other_useful_import_libraries/mpg.xlsx")

# Read Excel Files (.xlsx) - Specific Sheet with rio
rio::import(file = "./data/02_08_other_useful_import_libraries/mpg.xlsx", sheet = "Sheet 1")

# Import Larger Files with fread
df.f <- fread(file = "./data/02_05_read_files/mpg_maxi.csv", sep = ",")

# ------------------------------ Write Files ------------------------------

# Write a csv File - Comma Separated
write_csv(x = mpg,
          file = "./data/write files/readr comma separated.csv",
          col_names = T)

# Write a csv File - Semicolon Separated
write_csv2(x = mpg,
          file = "./data/write files/readr semicolon separated.csv",
          col_names = T)

# Write an xlsx File 
rio::export(x = mpg, file = "./data/write files/rio excel.xlsx")

# Write/Read an rds File
write_rds(x = mpg, file = "./data/write files/rio rds.rds")
read_rds(file = "./data/write files/rio rds.rds")

# Write/Read Feather File
write_feather(x = mpg, path = "./data/write files/feather.feather")
read_feather(path = "./data/write files/feather.feather")
