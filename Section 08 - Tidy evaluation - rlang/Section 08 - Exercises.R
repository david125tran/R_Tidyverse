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

# ------------------------------ Exercise 1 ------------------------------
count_freq <- function(df, x) {
    # This function takes in a dataframe (as "df") and a grouping variable (as "x").
    # It returns the count of the grouping variable. 

    # Output error if package is not found
    require(dplyr)

    # Capture user's argument that will be quoted
    x <- rlang::enquo(x)


    df %>%
        # Unquote user's arguments into the quoting function
        group_by(!!x) %>%
        summarise("Frequency" = n()) %>%
        ungroup()
}

count_freq(mpg, manufacturer)

# ------------------------------ Exercise 2 ------------------------------
draw_bar_plot <- function(df, x, y) {
    # This function takes in a dataframe (as "df"), an x-axis (as "x"), and a y-axis (as "y").
    # It then draws a bar plot of x vs y.  

    # Output error if package is not found
    require(dplyr)
    require(ggplot2)
    require(rlang)

    # Capture user's arguments that will be quoted 
    x_var <- enquo(x)
    y_var <- enquo(y)

    df %>%
        # Unquote user's arguments into the quoting function
        ggplot(aes(x = !!x_var,
                   y = !!y_var)) +
        geom_col()
}

draw_bar_plot(df = count_freq(mpg, manufacturer), x = manufacturer, y = Frequency)

# ------------------------------ Exercise 3 ------------------------------
prepare_diamonds_data <- function(df, n) {
    # This function takes in the diamonds data frame (as "df") and a random population size
    # (as "n").  It then returns a tibble of n population size with an added volume column 
    # which is the result of columns (x * y * z).    

    # Output error if package is not found
    require(dplyr)
    require(rlang)

    df %>%
        # Unquote user's arguments into the quoting function
        sample_n(size = n, replace = T) %>%
        mutate(volume = x * y * z)
}

prepare_diamonds_data(diamonds, n = 10000)

# ------------------------------ Exercise 4 ------------------------------
# Generic scatter plot with a custom theme:
theme_fonts <- theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 16, face = "italic", hjust = 0.5),
    axis.text = element_text(size = 14, face = "bold", hjust = 0.5)
)

explore_diamonds <- function(df, 
                             x, 
                             y, 
                             color,
                             size,
                             title = "", 
                             x_title = "",
                             y_title = "") {
    # This function takes in a dataframe (as "df"), a variable (as "x"), and a variable (as "y").
    # It also takes in custom theme variables: (1) a color theme (as "color"), (2) a point size variable (as "size"),
    # (3) a custom title (as "title"), (4) a custom x-axis title (as "x_title"), and (5) a custom y-axis title 
    # (as "y_title"). It returns a scatter plot of x (x-axis) vs. y (y-axis) with the added custom themes.

    # Output error if package is not found                                    
    require(ggplot2)
    require(rlang)
    col_var <- enquo(color)

    # Capture user's arguments that will be quoted 
    x_var <- enquo(x)
    y_var <- enquo(y)
    size <- enquo(size)

    df %>%
        # Unquote user's arguments into the quoting function
        ggplot(aes(x = !!x_var,
                   y = !!y_var,
                   size = !!size,
                   color = !!col_var)) +
        geom_point() +
        # Add custom theme to scatter plot
        ggtitle(title) +
        xlab(x_title) +
        ylab(y_title) +
        theme_minimal() +
        theme_fonts
}

explore_diamonds(df = prepare_diamonds_data(diamonds, n = 10000), 
                 x = carat, 
                 y = price, 
                 color = color,
                 size = volume,
                 title = "Diamonds Price by Carat (USD)",
                 x_title = "Carat",
                 y_title = "Price (USD)")


