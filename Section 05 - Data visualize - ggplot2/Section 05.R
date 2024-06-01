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
# install.packages("readr")
# install.packages("readxl")
# install.packages("rio")
# install.packages("stringr")
# install.packages("tibble")
# install.packages("tidyr")
# install.packages("tidyverse")

# ------------------------------ Load Libraries ------------------------------
library(cowplot)
library(ggplot2)
library(tidyverse)

# ------------------------------ Data Visualization & ggplot2 ------------------------------
# Building Blocks - Plots Lyaers
# 1 - Data          -   The data to be plotted 
# 2 - Aesthetics    -   The scales on which the data is mapped (variables mapping: x, y, axis, color, shape, ...)
# 3 - Geometries    -   Shapes used to represent your data on the plot (line, plot, ...)
# 4 - Facets        -   Facets for creating subplots (splitting rows and columns)
# 5 - Statistics    -   Statistical models and summaries (to be added to the plot)
# 6 - Coordinates   -   Space used for plotting (which type of coordinate system)
# 7 - Theme         -   Design Elements, describing non-data ink of the plot

# ------------------------------ Histogram Plot ------------------------------
# Generate some random data with a uniform continuous distribution
set.seed(1235)
df.unif <- runif(n = 100000, min = 0, max = 1) %>%
    tibble(x = .)

                    # # A tibble: 100,000 × 1
                    #         x
                    #     <dbl>
                    #  1 0.243
                    #  2 0.515
                    #  3 0.0994
                    #  4 0.902
                    #  5 0.839
                    #  6 0.424
                    #  7 0.544
                    #  8 0.745
                    #  9 0.545
                    # 10 0.212
                    # # ℹ 99,990 more rows

ggplot(data = df.unif,
       mapping = aes(x = x)) + 
       geom_histogram()

df.unif %>%
    ggplot(aes(x = x)) +
    geom_histogram(bins = 45, 
                   color = "black",
                   fill = "deepskyblue")

# Normal continuous distribution (random numbers)
df.norm <- rnorm(n = 100000, mean = 0, sd = 1) %>%
    tibble(x = .)

df.norm %>%
    ggplot(aes(x = x)) +
    geom_histogram(bins = 45, 
                   color = "black",
                   fill = "deepskyblue")

# Cars dataset
mpg %>% 
    ggplot(aes(x = hwy)) +
    geom_histogram(bins = 45, 
                   color = "black",
                   fill = "deepskyblue") +
    xlab("Highway miles per gallon") + 
    ylab("Number of cars (Frequency)") +
    ggtitle("Distribution of variables - hwy")

# ------------------------------ Density Plot ------------------------------
df.norm %>%
    ggplot(aes(x = x)) +
    geom_density(adjust = 0.5)

# Line parameter
df.norm %>%
    ggplot(aes(x = x)) +
    geom_density(adjust = 0.5, 
                 size = 1.2,
                 linetype = "dashed",
                 color = "black",
                 fill = "brown")

# Visualize normally distributed variables (diff. distr. params.)
df.norm1 <- rnorm(n = 100000, mean = 0, sd = 1) %>%
    tibble(x = .)
df.norm2 <- rnorm(n = 100000, mean = 1, sd = 2) %>%
    tibble(x = .)
df.norm3 <- rnorm(n = 100000, mean = -2, sd = 3) %>%
    tibble(x = .)

ggplot() + 
    # First density
    geom_density(data = df.norm1, 
                 aes(x = x),
                 color = "black",
                 fill = "blue",
                 alpha = 0.2) +
    # Second density
    geom_density(data = df.norm2, 
                 aes(x = x),
                 color = "black",
                 fill = "red",
                 alpha = 0.2) +
    # Third density
    geom_density(data = df.norm3, 
                 aes(x = x),
                 color = "black",
                 fill = "green",
                 alpha = 0.2)

# Cars dataset
ggplot() + 
    # highway density
    geom_density(data = mpg, 
                 aes(x = hwy),
                 color = "black",
                 fill = "blue",
                 alpha = 0.2) +
    # city density 
    geom_density(data = mpg, 
                 aes(x = cty),
                 color = "black",
                 fill = "green",
                 alpha = 0.2) +
    xlab("Miles per gallon") +
    ylab("Density")

# How to export a figure
setwd("C:/Users/Laptop/Desktop/R - Tidyverse/Section 05 - Data visualize - ggplot2")
ggsave(filename = "./data/density_plot_mpg.png",
       plot = last_plot(),
       units = "cm", 
       width = 29, 
       height = 21, 
       dpi = 300)

# ------------------------------ Area Plot ------------------------------
# Bins - Statistics
df.norm %>%
    ggplot(aes(x = x)) +
    geom_area(stat = "bin",
              binwidth = 0.1)

# Cumulative distribution
df.norm <- rnorm(n = 1000,
                 mean = 0,
                 sd = 1) %>%
    tibble(x = .)

df.norm <- df.norm %>%
    arrange(x) %>%
    mutate(count = 1,
           y = cumsum(count),
           y = y / sum(count))

df.norm %>%
    ggplot(aes(x = x,
           y = y)) +
    geom_area(fill = "blue",
              color = "black")


# Add some standard deviations
df.norm %>%
    ggplot(aes(x = x,
           y = y)) +
    geom_area(fill = "blue",
              color = "black") +
    # Add some dashed lines to the area plot
    geom_vline(xintercept = -3,
               linetype = "dashed",
               size = 1) +
    geom_vline(xintercept = -2,
               linetype = "dashed",
               size = 1) +
    geom_vline(xintercept = -1,
               linetype = "dashed",
               size = 1) +
    geom_vline(xintercept = -0,
               linetype = "dashed",
               size = 1) +     
    geom_vline(xintercept = 1,
               linetype = "dashed",
               size = 1) +
    geom_vline(xintercept = 2,
               linetype = "dashed",
               size = 1) +
    geom_vline(xintercept = 3,
               linetype = "dashed",
               size = 1)

# Theoretical probabilities
tibble(x = seq(-3,3,1),
       probs = pnorm(q = x,
                     mean = 0,
                     sd = 1,
                     lower.tail = T))

# ------------------------------ Bar Plot ------------------------------
# Make some data
groups <- paste("groups", 1:4, sep = " ")
probs <- c(.2, .3, .4, .1)
sum(probs)
set.seed(123)
df.data <- sample(groups,
                  size = 1000,
                  replace = T,
                  prob = probs) %>%
    tibble(group = .)

# Bar plot - statistics = "count"
df.data %>%
    ggplot(aes(x = group)) +
    geom_bar(stat = "count")

# Bar plot - statistics = "identity"
df.data %>%
    # Count frequency
    group_by(group) %>%
    summarise(freq = n()) %>%
    ungroup() %>%
    # Plot
    ggplot(aes(x = group,
               y = freq)) +
    geom_bar(stat = "identity")

# Bar plot - statistics = "count" - with colors
df.data %>%
    ggplot(aes(x = group,
               fill = group)) +
    geom_bar(stat = "count")

# Bar plot - statistics = "count" - with manually selected colors
df.data %>%
    ggplot(aes(x = group,
               fill = group)) +
    geom_bar(stat = "count",
             color = "black") + 
    scale_fill_manual(values = c("red", "green", "blue", "gray"))

# Bar plot - statistics = "count" - with pallete colors
df.data %>%
    ggplot(aes(x = group,
               fill = group)) +
    geom_bar(stat = "count",
             color = "black") + 
    scale_fill_brewer(palette = 3)

# Bar plot - statistics = "count" - with viridis pallete colors
df.data %>%
    ggplot(aes(x = group,
               fill = group)) +
    geom_bar(stat = "count",
             color = "black") + 
    scale_fill_viridis_d(option = "magma")

# Bar plot - statistics = "identity" with viridis pallete colors and labels on top 
df.data %>%
    # Count frequency
    group_by(group) %>%
    summarise(freq = n()) %>%
    ungroup() %>%
    # Plot
    ggplot(aes(x = group,
               y = freq,
               fill = group)) +
    geom_bar(stat = "identity") + 
    # Viridis pallete colors
    scale_fill_viridis_d(option = "magma") +
    # Labels
    geom_text(aes(label = freq,
                  y = freq + 10),
              size = 10)

# Cars dataset 
mpg %>%
    # Count of cars
    group_by(manufacturer) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    # Add percentages
    mutate(percentage = round(n / sum(n) * 100, 1),
           label = paste(n, " | ", percentage, "%", sep = "")) %>%
    # Order
    arrange(desc(n)) %>%
    # Add manufacturer as factor
    mutate(manufacturer = as.factor(manufacturer),
           manufactuer = fct_inorder(manufacturer)) %>%
    # Plot
    ggplot(aes(x = manufactuer,
               y = n,
               fill = manufactuer)) + 
    geom_bar(stat = "identity",
             show.legend = F,
             color = "black") +
    # Labels
    geom_text(aes(label = label,
                  y = n + 1),
              size = 4) +
    scale_fill_viridis_d(option = "inferno", direction = -1) + # Viridis pallete colors
    xlab("Car manufacturer") +
    ylab("Car count") + 
    ggtitle("Number of cars per each manufactuer")

# Save plot
ggsave(filename = "./data/cars_per_manufacturer.png",
       plot = last_plot(),
       units = "cm", 
       width = 29, 
       height = 21, 
       dpi = 500)

# ------------------------------ Scatter Plot ------------------------------
mpg %>%
    ggplot(aes(x = cty,
               y = hwy)) +
    geom_point(color = "red",
               size = 5,
               shape = 17) +   # Triangle points
    # Add regression line
    geom_smooth(method = "lm",
                se = T)

mpg %>%
    ggplot(aes(x = cty,
               y = hwy)) +
    geom_point() +
    # Add scales for the axis
    scale_x_continuous(breaks = seq(0,50,2.5),
                       limits = c(0,50)) +
    scale_y_continuous(breaks = seq(0,50,2.5),
                       limits = c(0,50))

# Diamonds dataset
set.seed(123)
df.diamonds <- diamonds %>%
    sample_n(size = 10000,
             replace = F)

df.diamonds %>%
    ggplot(aes(x = carat,
               y = price)) +
    geom_point()

# Include transparency
df.diamonds %>%
    ggplot(aes(x = carat,
               y = price)) +
    geom_point(size = 3,
               alpha = 1/5)

# Alter y-axis - Transform nonlinear trend to linear trend - Option 1: Square root transformation
df.diamonds %>%
    ggplot(aes(x = carat,
               y = price)) +
    geom_point() + 
    scale_y_sqrt()

# Alter y-axis - Transform nonlinear trend to linear trend - Option 2: log10() transformation
df.diamonds %>%
    ggplot(aes(x = carat,
               y = price)) +
    geom_point() + 
    scale_y_log10() +
    scale_x_log10()

# Add smoothing line - Auto detect 
df.diamonds %>%
    ggplot(aes(x = carat,
               y = price)) +
    geom_point() + 
    geom_smooth()

# ------------------------------ Bar Plot & Scatter Plot ------------------------------
# Stacked bar plot
mpg %>%
    ggplot(aes(x = manufacturer,
               fill = class)) +
    geom_bar(position = "stack",
             color = "black") +
    scale_fill_viridis_d()

# Dodge
mpg %>%
    ggplot(aes(x = manufacturer,
               fill = class)) +
    geom_bar(position = "dodge",
             color = "black") +
    scale_fill_viridis_d()

# Fill
mpg %>%
    ggplot(aes(x = manufacturer,
               fill = class)) +
    geom_bar(position = "fill",
             color = "black") +
    scale_fill_viridis_d()

# Scatter plot
mpg %>%
    ggplot(aes(x = manufacturer,
               y = class)) +
    geom_point(position = "jitter")

# Scatter plot - with colors
mpg %>%
    ggplot(aes(x = manufacturer,
               y = class,
               color = class)) +
    geom_point(position = "jitter") +
    scale_color_viridis_d()

# Diamonds plots
df.diamonds %>%
    ggplot(aes(x = color,
               fill = cut)) +
    geom_bar(position = "stack",
             color = "black") +
    scale_fill_viridis_d()

df.diamonds %>%
    ggplot(aes(x = color,
               y = cut,
               color = cut)) +
    geom_jitter(size = 2) +
    scale_fill_viridis_d()

# ------------------------------ Box Plot------------------------------
mpg %>%
    ggplot(aes(x = manufacturer,
               y = hwy)) + 
    geom_boxplot()

df.diamonds %>%
    ggplot(aes(x = color,
               y = price,
               fill = color)) +
    geom_boxplot() +
    scale_fill_viridis_d() +
    scale_y_log10()

# Tweak parameters of boxplot
mpg %>%
    ggplot(aes(x = class,
               y = hwy)) + 
    geom_boxplot(fill = "brown1",
                 outlier.colour = "blue",
                 outlier.size = 5,
                 size = 1.3)

# Modify theme layer
mpg %>%
    ggplot(aes(x = class,
               y = hwy)) + 
    geom_boxplot() + 
    # theme_bw()
    # theme_gray()
    # theme_classic()
    theme_classic()

# Custom theme
mpg %>%
    ggplot(aes(x = class,
               y = hwy,
               fill = class)) + 
    geom_boxplot() + 
    # Labels
    xlab("Class") + 
    ylab("Highway consumption") + 
    ggtitle("Car fuel consumption by class") + 
    # Theme layer
    theme(axis.text = element_text(size = 14),
          axis.title = element_text(size = 14),
          axis.title.x = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
          plot.title = element_text(size = 25, face = "bold", hjust = 0.5),
          legend.background = element_rect(fill = "grey",
                                           colour = "black",
                                           linetype = "dashed"))

# Save plot
ggsave(filename = "./data/cars_box_plot.png",
       plot = last_plot(),
       units = "cm", 
       width = 29, 
       height = 21, 
       dpi = 500)

# ------------------------------ Violin Plot------------------------------
mpg %>%
    ggplot(aes(x = manufacturer,
               y = hwy)) + 
    geom_violin(fill = "green",
                scale = "count")

# ------------------------------ Visualize Multiple Variables Using Only One Plot ------------------------------
# Facets divide a plot into subplots based on the values of one or more discrete (categorial) variables
# Facet is an object that creates many little graphics that are variations of a single graphic - facets are frames of frames
# Data is splitted into groups (facets) and data is plotted in their own plots using the same coordinate system.

# Facet Options:
# facet_wrap()          -   Wrap subplots into columns
# facet_grid()          -   Split plot according to rows and columns

# facet_wrap()
mpg %>%
    ggplot(aes(x = cty,
               y = hwy)) + 
    geom_jitter() +
    facet_wrap(vars(class),
               scales = "free")

# facet_grid()
mpg %>%
    ggplot(aes(x = cty,
               y = hwy)) + 
    geom_jitter() +
    facet_grid(rows = vars(class),
               scales = "free")

mpg %>%
    ggplot(aes(x = cty,
               y = hwy)) + 
    geom_jitter() +
    facet_grid(class ~ drv,
               scales = "free")

# Add multiple features
mpg %>%
    ggplot(aes(x = cty,
               y = hwy,
               size = cyl)) +
    geom_jitter() +
    scale_size(range = c(1,10))

mpg %>%
    ggplot(aes(x = cty,
               y = hwy,
               color = cyl)) +
    geom_jitter()

mpg %>%
    ggplot(aes(x = cty,
               y = hwy,
               color = class)) +
    geom_jitter()

mpg %>%
    ggplot(aes(x = cty,
               y = hwy,
               shape = class)) +
    geom_jitter()

# Final plot
df.diamonds %>%
    ggplot(aes(x = carat,
               y = price,
               color = cut)) +
    geom_jitter() +
    facet_grid(color ~ clarity,
               scales = "free",
               labeller = "label_both")

ggsave(filename = "./data/diamonds_facet_plot.png",
       plot = last_plot(),
       units = "cm", 
       width = 29, 
       height = 21, 
       dpi = 300)

# ------------------------------ Visualize Time Series ------------------------------
economics %>%
    ggplot(aes(x = date,
               y = unemploy)) +
    geom_line()

# Multiple time series - Wide table format
economics %>%
    ggplot(aes(x = date)) +
    # umemploy vs time
    geom_line(aes(y = unemploy), color = "red") + 
    # pce vs time
    geom_line(aes(y = pce), color = "blue") + 
    # psavert vs time
    geom_line(aes(y = psavert), color = "black") +
    scale_y_log10() 

# Multiple time series - Long table format
economics_long %>%
    ggplot(aes(x = date,
               y = value,
               group = variable,
               color = variable)) +
    geom_line()

economics_long %>%
    filter(variable != "pop") %>%
    ggplot(aes(x = date,
               y = value,
               group = variable,
               color = variable)) +
    geom_line() +
    scale_x_date(date_breaks = "5 years", date_labels = "%Y-%m")

# ------------------------------ Heatmaps ------------------------------
mpg %>%
    group_by(manufacturer, class) %>%
    summarise(cars = n()) %>%
    ungroup() %>%
    ggplot(aes(x = class,
               y = manufacturer,
               fill = cars)) +
    geom_tile() + 
    scale_fill_viridis_c(option = "magma")

mpg %>%
    group_by(manufacturer, class) %>%
    summarise(`hwy mean` = mean(hwy)) %>%
    ungroup() %>%
    ggplot(aes(x = class,
               y = manufacturer,
               fill = `hwy mean`)) +
    geom_tile() + 
    scale_fill_viridis_c(option = "magma")

# ------------------------------ Maps ------------------------------
# Crime map data
df.crime <- USArrests %>%
    mutate(region = str_to_lower(rownames(.))) %>%
    left_join(x = .,
              y = map_data("state"),
              by = "region")
# Map
df.crime %>%
    ggplot(aes(x = long,
               y = lat,
               group = group)) +
    geom_polygon(aes(fill = Assault),
                 color = "white") +
    scale_fill_viridis_c(option = "magma") +
    theme_minimal()

# ------------------------------ Sub Plots ------------------------------
p1 <- ggplot(mpg, aes(x = cty, y = hwy)) + geom_jitter()
p2 <- ggplot(mpg, aes(x = displ, y = hwy)) + geom_jitter()
p3 <- ggplot(mpg, aes(x = cyl, y = hwy)) + geom_jitter()
p4 <- ggplot(mpg, aes(x = drv, y = hwy)) + geom_jitter()
p5 <- ggplot(mpg, aes(x = trans, y = hwy)) + geom_jitter()
p6 <- ggplot(mpg, aes(x = class, y = hwy)) + geom_jitter()

# Create sub plots
plot_grid(p1, p2, p3, p4, labels = "AUTO")
plot_grid(p1, p2, p3, p4, labels = c("p1", "p2", "p3", "p4"))

# Alter configuration
plot_grid(p1, p2, p3, p4, p5, p6, nrow = 3, ncol = 2)
plot_grid(p1, p2, p3, p4, p5, p6, nrow = 1, ncol = 6)
