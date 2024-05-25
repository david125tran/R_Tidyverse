# Remove previous objects:
rm(list = ls())
graphics.off()

# ------------------------------ Libraries ------------------------------
# install.packages("data.table")
# install.packages("dplyr")
# install.packages("feather")
# install.packages("ggplot2")
# install.packages("languageserver")
# install.packages("lintr")
# install.packages("readr")
# install.packages("readxl")
# install.packages("rio")
# install.packages("stringr")
# install.packages("tibble")
# install.packages("tidyr")
# install.packages("tidyverse")

# ------------------------------ Load Libraries ------------------------------
library(dplyr)
library(ggplot2)
library(stringr)
library(tibble)
library(tidyr)

# ------------------------------ Strings Inside Tidyverse ------------------------------
s1 <- "I am a string"
s2 <- 'I am also a string'
vec <- c("a", "b", "c")

# Character vector inside tibble (as column)
df <- tibble(letters =  vec)

                    # # A tibble: 3 × 1
                    #   letters
                    #   <chr>
                    # 1 a
                    # 2 b
                    # 3 c

# Special characters (How to escape special characters)
s3 <- "\""

# New line
"\n"

# Tabulator
"t"

# Unicode - Non-English letters
"\u0381"

# See raw content of your string
s3 <- "I\nLove\nPizza"
writeLines(s3)

# ------------------------------ Strings Matching ------------------------------
setwd("C:/Users/Laptop/Desktop/R - Tidyverse/Section 03 - Data wrangle - strings - factors (stringr & forcats)")

load("./data/strings.RData")
ls()

                    #  [1] "countries"    "countries.df" "df"           "fruit"        "fruit.df"
                    #  [6] "ind"          "letters"      "Letters"      "letters.df"   "Letters.df"
                    # [11] "new_line"     "s1"           "s2"           "s3"           "sentences"
                    # [16] "sentences.df" "vec"          "words"        "words.df"

# ------------------------------ str_detect() - Detect a String ------------------------------
fruit
ind <- str_detect(string = fruit, pattern = "a")
fruit[ind]

                    #  [1] "apple"             "apricot"           "avocado"
                    #  [4] "banana"            "blackberry"        "blackcurrant"
                    #  [7] "blood orange"      "breadfruit"        "canary melon"
                    # [10] "cantaloupe"        "cherimoya"         "cranberry"
                    # [13] "currant"           "damson"            "date"
                    # [16] "dragonfruit"       "durian"            "eggplant"
                    # [19] "feijoa"            "grape"             "grapefruit"
                    # [22] "guava"             "jackfruit"         "jambul"
                    # [25] "kumquat"           "loquat"            "mandarine"
                    # [28] "mango"             "nectarine"         "orange"
                    # [31] "pamelo"            "papaya"            "passionfruit"     
                    # [34] "peach"             "pear"              "physalis"
                    # [37] "pineapple"         "pomegranate"       "purple mangosteen"
                    # [40] "raisin"            "rambutan"          "raspberry"
                    # [43] "redcurrant"        "salal berry"       "satsuma"
                    # [46] "star fruit"        "strawberry"        "tamarillo"        
                    # [49] "tangerine"         "watermelon"

# Old way of detecting a string in a string
grepl(pattern = "a", x = fruit)

# Find a fruit not containing any letter "a"
str_detect(fruit, "a", negate = T)  

# Inside of tibble, add a flag if the fruit doesnt contain the letter 'a'
fruit.df %>%
    mutate(flag = case_when(str_detect(string = fruit, pattern = "a") ~ "contains 'a'",
                            T ~ "does not contain 'a'"))

# ------------------------------ str_which() - Returns Index Position of a String ------------------------------
str_which(string = fruit, pattern = "a")

                    #  [1]  1  2  3  4  7  8  9 12 13 14 15 21 23 24 25 26 27 28 30 34 35 36 39 40 43
                    # [26] 46 48 49 51 54 55 56 57 58 59 61 62 64 66 68 69 70 71 73 74 75 76 77 78 80

# Get all the fruits with the letter 'a'
ind <- str_which(string = fruit, pattern = "a")
fruit[ind]

                    #  [1] "apple"             "apricot"           "avocado"
                    #  [4] "banana"            "blackberry"        "blackcurrant"
                    #  [7] "blood orange"      "breadfruit"        "canary melon"
                    # [10] "cantaloupe"        "cherimoya"         "cranberry"
                    # [13] "currant"           "damson"            "date"
                    # [16] "dragonfruit"       "durian"            "eggplant"
                    # [19] "feijoa"            "grape"             "grapefruit"       
                    # [22] "guava"             "jackfruit"         "jambul"
                    # [25] "kumquat"           "loquat"            "mandarine"
                    # [28] "mango"             "nectarine"         "orange"
                    # [31] "pamelo"            "papaya"            "passionfruit"     
                    # [34] "peach"             "pear"              "physalis"
                    # [37] "pineapple"         "pomegranate"       "purple mangosteen"
                    # [40] "raisin"            "rambutan"          "raspberry"
                    # [43] "redcurrant"        "salal berry"       "satsuma"
                    # [46] "star fruit"        "strawberry"        "tamarillo"
                    # [49] "tangerine"         "watermelon"

# ------------------------------ str_count() - Count the number of string occurences in a string ------------------------------
fruit.df1 <- fruit.df %>%
    mutate(`count a` = str_count(string = fruit, pattern = "a"))

                    # # A tibble: 80 × 2
                    #    fruit        `count a`
                    #    <chr>            <int>
                    #  1 apple                1
                    #  2 apricot              1
                    #  3 avocado              2
                    #  4 banana               3
                    #  5 bell pepper          0
                    #  6 bilberry             0
                    #  7 blackberry           1
                    #  8 blackcurrant         2
                    #  9 blood orange         1
                    # 10 blueberry            0
                    # # ℹ 70 more rows

# Show all the fruits containing the letter "a" 3 times
fruit.df1 %>%
    filter(`count a` == 3)

                    # # A tibble: 2 × 2
                    #   fruit  `count a`
                    #   <chr>      <int>
                    # 1 banana         3
                    # 2 papaya         3

# ------------------------------ str_locate() / str_locate_all() - Locate the positions of a string in a string ------------------------------
str_locate(fruit, pattern = "a")

fruit.df1 <- str_locate(fruit, pattern = "a") %>%
    as_tibble() %>%
    mutate(fruit = fruit) %>%
    select(fruit, start, end)

# Locate positions of all letters "a" in each fruit
str_locate_all(fruit, pattern = "a")

# ------------------------------ str_sub() - Extract part of a string from a character vector ------------------------------
# Extract the 1st three letters of a fruit
str_sub(fruit, start = 1, end = 3)

# Extract the 1st letter of a common word and count the word frequency
words.df %>%
    mutate(`first letter` = str_sub(word, 1, 1)) %>%
    count(`first letter`) %>%
    arrange(desc(n))

                    # # A tibble: 42 × 2
                    #    `first letter`     n
                    #    <chr>          <int>
                    #  1 s                334
                    #  2 c                297
                    #  3 p                246
                    #  4 a                214
                    #  5 t                174
                    #  6 e                164
                    #  7 r                163
                    #  8 d                161
                    #  9 b                143
                    # 10 f                141
                    # # ℹ 32 more rows

# Extract the last 3 letters of a fruit
str_sub(fruit, start = -3, end = -1)

# ------------------------------ str_subset() - Get the strings that have a match ------------------------------
# Find fruits that contain the letter "c"
str_subset(fruit, pattern = "c")

# Find fruits that contain the letter "c"
fruit[str_detect(fruit, pattern = "c")]

# ------------------------------ str_length() - Get the string length ------------------------------
str_length("pizza")

# Get all fruits with 10 or more characters
fruit[str_length(fruit) >= 10]

# ------------------------------ str_pad() - Pad a string to a minimum width ------------------------------
str_pad(fruit, width = 20, side = "left", pad = "x") # Adds an "x" to the left of the string until the string is 20 characters

# ------------------------------ str_trunc() - Truncate a string to a set width ------------------------------
str_trunc(fruit, width = 5, side = "left", ellipsis = "...")

# ------------------------------ str_trim() - Trim a string of some string ------------------------------
whitespace <- c("nospace",
                " leftspace",
                "rightspace ",
                "rightspaces     ",
                " bothspace ",
                "middle space",
                " mixed space ")

str_trim(whitespace, side = "left")
str_trim(whitespace, side = "both")
str_trim(whitespace, side = "right")

# ------------------------------ Mutating Strings ------------------------------

# Replace the first 3 letters of each fruit name with "FRU"
fruit.sub <- fruit
str_sub(fruit.sub, 1, 3) <- "FRU"
fruit.sub

# Replace only the 1st occurence of a string in a given string
str_replace(fruit, pattern = "a", replacement = "A")

# Replace all occurences of a string in a given string
str_replace_all(fruit, pattern = "a", replacement = "A")

# Convert a string to lowercase
str_to_lower("PIZZA")

# Convert a string to uppercase
str_to_upper("pizza")

# Convert a string to a "Title" case
str_to_title("the cat in the hat")

# ------------------------------ Splitting Strings ------------------------------
fruit1 <- fruit[1:20]
fruit2 <- fruit[21:40]
fruit3 <- fruit[41:60]
fruit4 <- fruit[61:80]

# ------------------------------ String Concatenation ------------------------------
str_c("I", " love ", "pizza")
str_c(fruit1, fruit2, fruit3, fruit4, sep = " ")
str_glue("What is the value of sqrt(2)?  It is {round(sqrt(2), 3)}")
name <- "David"
str_glue("Hi, my name is {name}")

# ------------------------------ Repeat a String N number of Times ------------------------------
str_dup("Cheeseburger", times = 5)

# ------------------------------ Split a String by a String Occurence ------------------------------
str_split_fixed(string = fruit, pattern = " ", n = 2)
sentences
str_split_fixed(sentences, " ", n = 2)

# ------------------------------ Sorting Strings ------------------------------
str_order(c("Melon", "Berry", "Apple", "Orange"), decreasing = FALSE)

fruit_shuffle <- sample(x = fruit, size = length(fruit), replace = F)
fruit_sorted <- fruit_shuffle[str_order(x = fruit_shuffle)]

# ------------------------------ Sorting Numbers Stored as a String ------------------------------
numbers.s <- sample(1:250, size = 20, replace = F)
numbers.s <- as.character((numbers.s))      # Convert to string

str_sort(numbers.s, numeric = TRUE)
                    #  [1] "4"   "53"  "66"  "76"  "78"  "105" "107" "114" "117" "140" "158" "183"
                    # [13] "199" "213" "217" "225" "226" "236" "239" "242"

# ------------------------------ View String Occurences ------------------------------
str_view("Pizza", "z")
                    # [1] │ Pi<z><z>a

# ------------------------------ Regular Expressions (regex) ------------------------------
string <- c("string", "word", "letter", "word.letter", "character/letter")

# Match "tr"
str_view(string, "tr")

# Match "t" - Any character before or after "t"
str_view(string, ".t.")

# Match "." and not as a special character 
str_view(string, "\\.")

# Match "/" and not as a special character
str_view(string, "/")

# Special characters
#   .           Match any character
#   \           Escape special character
#   ()          Define groups
#   {}          Define quantifiers
#   ^           Match end of a string
#   |           Or (operator)
#   ?           Quantifier: zero or one
#   *           Quantifier: zero or more
#   +           Quantifier: one or more
#   \n          New line (return)
#   \t          Tab 
#   \s \S       Any white space / non whitespace
#   \d \D       Any digit / non digit
#   \w \W       Any word character / non word character
#   \b          Word boundaries

# Classes
#   [:digit:]   Digits
#   [:alpha:]   Letters
#   [:lower:]   Lowercase letters
#   [:upper:]   Uppercase letters
#   [:alnum:]   Letters & numbers
#   [:punct:]   Punctuation
#   [:graph:]   Letters, numbers, & punctuation
#   [:space:]   Space characters (Example: \s)
#   [:blank:]   Space & tab (but not new line)

# ------------------------------ Regular Expressions (regex) - Special Characters & Classes ------------------------------
# Different Characters
string <- c(letters, "123", "1-5-6", "598642")
string
                    #  [1] "a"      "b"      "c"      "d"      "e"      "f"      "g"      "h"
                    #  [9] "i"      "j"      "k"      "l"      "m"      "n"      "o"      "p"
                    # [17] "q"      "r"      "s"      "t"      "u"      "v"      "w"      "x"
                    # [25] "y"      "z"      "123"    "1-5-6"  "598642"

# Match a string with digits
str_subset(string, pattern = "\\d")

# Match strings without digits
str_subset(string, pattern = "\\D")

# Match strings with pattern: "digit-digit-digit"
str_subset(string, pattern = "\\d-\\d-\\d")

set.seed(123)
string <- c(sample(sentences, 5),
            sample(fruit, 5),
            sample(words, 5),
            "This is \nnew line",
            "String with a tab  "
            )

string
                    #  [1] "Fasten two pins on each side."
                    #  [2] "The bunch of grapes was pressed into wine."
                    #  [3] "They felt gay when the ship arrived in port."
                    #  [4] "Shake hands with this friendly child."
                    #  [5] "Sell your gift to a buyer at a good gain."
                    #  [6] "kiwi fruit"
                    #  [7] "mulberry"
                    #  [8] "kumquat"
                    #  [9] "cantaloupe"
                    # [10] "date"
                    # [11] "team"
                    # [12] "measure"
                    # [13] "trace"
                    # [14] "fairly"
                    # [15] "freedom"
                    # [16] "This is \nnew line"
                    # [17] "String with a tab  "

writeLines(string)

# Match white spaces
str_subset(string, "\\s")

# Match string with new lines
str_subset(string, "\\n")

# Different Classes
string <- c("123abc", "123", ".,?", "ABC", "\nABC", "\tabc")


# Match digits
str_subset(string, "[:digit:]")

# Match letters
str_subset(string, "[:alpha:]")

# Match lower case letters
str_subset(string, "[:lower:]")

# Match upper case letters
str_subset(string, "[:upper:]")

# Match strings with letters, numbers, or punctuation
str_subset(string, "[:graph:]")

# Match strings with space characters
str_subset(string, "[:blank:]")




