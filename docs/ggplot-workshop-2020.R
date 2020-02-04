# Data visualization with R 
# Spring 2020
# UVa StatLab
# Clay Ford


# Helpful RStudio commands ------------------------------------------------

# Description       Windows & Linux       Mac 
#
# Run current line  Ctrl+Enter            Command+Enter
# Previous plot 	  Ctrl+Alt+F11 	        Command+Option+F11
#                   Ctrl+Shift+PageUp
# Next plot 	      Ctrl+Alt+F12 	        Command+Option+F12
#                   Ctrl+Shift+PageDown

# Packages ----------------------------------------------------------------

library(tidyverse)
library(scales)
library(gridExtra)
library(plotly)

# Data --------------------------------------------------------------------

# Albemarle County real estate data.

# Downloaded from Office of Geographic Data Services by Michele Claibourn.

# Read in the data:

# The readRDS() function allows us to read in an R object that was saved with saveRDS().
# saveRDS(homes, file = "albemarle_homes_2020.rds")

# We need to use the url() function to open the connection to the web site
# hosting the file. 
URL <- "https://github.com/clayford/DataVisualizationR/raw/master/albemarle_homes_2020.rds"
homes <- readRDS(file = url(URL))

# Let's review what we have.
str(homes) 
glimpse(homes)
names(homes)


# Map of census tracts
# https://www2.census.gov/geo/maps/dc10map/tract/st51_va/c51003_albemarle/DC10CT_C51003_001.pdf


# One Variable - Continuous -----------------------------------------------


# The histogram helps us see how a continous variable is distributed
ggplot(homes, aes(x=finsqft)) + geom_histogram()

# note the message regarding "stat_bin".
# "stat_bin" is the stat for geom_histogram.

# Says the documentation: "You should always override this value, exploring
# multiple widths to find the best way to illustrate the stories in your data."

# Try a new binwidth or bins
ggplot(homes, aes(x=finsqft)) + geom_histogram(bins = 40)
ggplot(homes, aes(x=finsqft)) + geom_histogram(binwidth=50)
# try some others! Beware, setting small binwidths can tax your computer.

# A frequency polygon is like a histogram, but counts are presented with lines
# instead of bars.
ggplot(homes, aes(x=finsqft)) + geom_freqpoly(bins = 40)
ggplot(homes, aes(x=finsqft)) + geom_freqpoly(binwidth = 50)

# We can map high school district to color to get freq polygon for all high
# school districts
ggplot(homes, aes(x=finsqft, color = hsdistrict)) + geom_freqpoly(bins = 40)


# To create a "true" histogram with density instead of count, (ie, the area
# under the histogram is 1), set y = stat(density) in the aes() function.

ggplot(homes, aes(x=finsqft, y=stat(density))) + 
  geom_histogram(binwidth=250)
ggplot(homes, aes(x=finsqft, y=stat(density))) + 
  geom_freqpoly(binwidth=250)


# The reason we might want a "True" histogram is to compare distributions with
# different counts.

# finsqft by condition of home
ggplot(homes, aes(x=finsqft, color = condition)) + 
  geom_freqpoly(binwidth=500) 

# finsqft by condition of home - True histogram
ggplot(homes, aes(x=finsqft, y=stat(density), color = condition)) + 
  geom_freqpoly(binwidth=500) 

# This data might be better presented in separate plots
ggplot(homes, aes(x=finsqft, y=stat(density))) + 
  geom_freqpoly(binwidth=500) +
  facet_wrap(~condition)

# And we might want to zoom in on the x-axis
ggplot(homes, aes(x=finsqft, y=stat(density))) + 
  geom_freqpoly(binwidth=500) +
  facet_wrap(~condition) +
  coord_cartesian(xlim = c(0,3000))

# Finally, we can make these plots interactive with the ggplotly() function.
# Just save the plot and use it as an argument to ggplotly()
p <- ggplot(homes, aes(x=finsqft)) + geom_histogram(bins = 40)
ggplotly(p)


# YOUR TURN #1 ------------------------------------------------------------

# (1) Create a histogram of totalvalue, the total value of the home. 
# Set bins = 100.


# (2) repeat #1 but now also facet by hsdistrict


# (3) repeat #3 but now also zoom in on x-axis from $0 to $1,000,000


# (4) save (3) and investigate with ggplotly()



# One variable - Discrete -------------------------------------------------


# Bar Plots help us visualize how discrete values are distributed:
ggplot(homes, aes(x=condition)) + geom_bar()

# Notice the geom_bar() function did the counting for us. If we already had a
# data frame with counts, we could use geom_col() with a y aesthetic

# Example: create a data frame of condition counts
hc <- homes %>% 
  group_by(condition) %>% 
  count()
hc

ggplot(hc, aes(x = condition, y = n)) + geom_col()

# A nice benefit of a data frame with counts is that we can sort the categories
# on the x-axis using reorder()
ggplot(hc, aes(x = reorder(condition, n), y = n)) + geom_col()

# Or the other way...
ggplot(hc, aes(x = reorder(condition, -n), y = n)) + geom_col()

# The x-axis now has a funny label. We can fix with labs()
ggplot(hc, aes(x=reorder(condition, -n), y = n)) + geom_col() +
  labs(x = "Condition")

# back to the homes data frame...

# the coord_flip() function allows us to flip the coordinate axis; notice there
# is also a width argument we can use to adjust size of bars.
ggplot(homes, aes(x=condition)) + 
  geom_bar(width = 0.75) +
  coord_flip() 

# Let's map the fp indicator to fill to get counts of homes with and without
# fireplaces by condition. Notice we had to wrap fp in factor()
ggplot(homes, aes(x=condition, fill = factor(fp))) + geom_bar()

# Note the "stacked" position. We can change that by setting position = "dodge"
# in geom_bar():
ggplot(homes, aes(x=condition, fill=factor(fp))) + 
  geom_bar(position = "dodge")

# Setting position = "fill" shows relative proportions at each category
ggplot(homes, aes(x=condition, fill=factor(fp))) + 
  geom_bar(position = "fill")

# what are those proportions exactly?
table(homes$fp, homes$condition) %>% 
  prop.table(margin = 2) %>%           # margin = 2 means "column proportions"
  round(2)

# Or use ggplotly to make an interactive plot
p <- ggplot(homes, aes(x=condition, fill=factor(fp))) + 
  geom_bar(position = "fill") 
ggplotly(p)


# How can we fix the legend title?
# Recall that is a by-product of the fill scale. So we need to use a scale
# function.
ggplot(homes, aes(x=condition, fill=factor(fp))) + 
  geom_bar(position = "fill") +
  scale_fill_discrete(name="Fireplaces", labels = c("None","At least one"))

# We could also modify the data
# homes <- homes %>% 
#   mutate(fp = factor(fp, labels = c("None","At least one")))

# An example with manually defined colors
ggplot(homes, aes(x=condition, fill=factor(fp))) + 
  geom_bar(position = "fill") +
  scale_fill_manual("Fireplaces",
                    values = c("Blue","Red"),
                    labels = c("None", "At least one"))

ggplot(homes, aes(x=condition, fill=factor(fp))) + 
  geom_bar(position = "fill") +
  scale_fill_manual("Fireplaces", 
                    values = c("grey80","grey40"), 
                    labels = c("None", "At least one"))


# enter colors() at console to see all available colors
# run demo("colors") to see all colors
# see Appendices for defining your own color palettes

# YOUR TURN #2 ------------------------------------------------------------

# (1) Create a barplot of counts for middle school district. 



# (2) Create a stacked proportional bar chart (ie, set position = "fill") for
# msdistrict using cooling (No Central Air vs. Central Air) as the fill
# variable: fill = cooling



# (3) The following code attempts to show a proportional bar plot of number of
# bedrooms (1-5) by msdistrict, but it doesn't work. Can you fix it?

ggplot(filter(homes, bedroom %in% 1:5), 
       aes(x=msdistrict, fill = factor(bedroom))) + 
  geom_bar(position = fill) +
  scale_fill_discrete("Bedrooms")



# Two numeric variables ---------------------------------------------------

# The scatterplot allows you to visualize the relationship between two
# numeric variables.

# plot finsqft vs totalvalue:
ggplot(homes, aes(x=finsqft, y=totalvalue)) + 
  geom_point()

# Lots of overplotting!
# What can we do about that? One solution is the alpha aesthetic. "alpha=1/10"
# means 10 points overplotted adds to a solid color
ggplot(homes, aes(x=finsqft, y=totalvalue)) + 
  geom_point(alpha=1/10)

# Can also try smaller points using the shape aesthetic
ggplot(homes, aes(x=finsqft, y=totalvalue)) + 
  geom_point(shape=".")

# open circles ala Base R
ggplot(homes, aes(x=finsqft, y=totalvalue)) + 
  geom_point(shape = 1)

# see ?points for shape codes

# Another approach is to use facets: break the data into subsets 
ggplot(homes, aes(x=finsqft, y=totalvalue)) + 
  geom_point() +
  facet_wrap(~ hsdistrict)

# We can also zoom in on plot
ggplot(homes, aes(x=finsqft, y=totalvalue)) + 
  geom_point(shape = 1) +
  facet_wrap(~ hsdistrict) +
  coord_cartesian(xlim = c(0,3000),ylim = c(0,1e6))

# We can also map the color of points to a variable in our data.
ggplot(homes, aes(x=finsqft, y=totalvalue, color=cooling)) + 
  geom_point(shape = 1) +
  facet_wrap(~ hsdistrict) +
  coord_cartesian(xlim = c(0,3000),ylim = c(0,1e6)) 

# This is another good application for ggplotly()
p <- ggplot(homes, aes(x=finsqft, y=totalvalue)) + 
  geom_point(shape = 1) +
  facet_wrap(~ hsdistrict) 
ggplotly(p)

# YOUR TURN #3 ------------------------------------------------------------

# (1) Plot age vs. finsqft with geom_point(). Put finsqft on the y axis. 



# (2) Repeat 1 but now also zoom in on the x and y axis. Look at the last 100
# years for houses with less than 5000 finsqft. Also, set shape = 1.





# Saving ggplot objects ---------------------------------------------------

# ggplot2 objects can be saved in our global environment
p <- ggplot(homes, aes(x=finsqft, y=totalvalue)) +
  geom_point()

# nothing is plotted. To see plot, call p
p

# We can work with ggplot objects as if they were ggplot2 code
p + coord_cartesian(xlim = c(0,2000), ylim = c(0,5e5))
p + facet_wrap(~condition)

# This can also help us save some typing. For example, instead of always typing
# ggplot(homes) we could do this:
h <- ggplot(homes)

# Now just use h instead of ggplot(homes) and call aes() by itself
h + aes(x=finsqft, y=totalvalue) + 
  geom_point()

h + aes(x = condition) + 
  geom_bar()


# Scales and smooths ------------------------------------------------------


# Let's look again at totalvalue vs finsqft:
ggplot(homes, aes(x=finsqft, y=totalvalue)) + 
  geom_point() +
  facet_wrap(~ hsdistrict)

# The y-axis scale would look better formatted as dollar amounts. The scales
# package can help with this. It has functions designed for this type of
# situation. When you install ggplot2, scales is installed as well.

# Three functions that are very handy: percent, comma, dollar;
# typically used in a scale function with the labels argument.

p2 <- ggplot(homes, aes(x=finsqft, y=totalvalue)) + 
  geom_point(alpha=1/6) +
  facet_wrap(~ hsdistrict) + 
  scale_y_continuous(labels=dollar) + 
  scale_x_continuous(labels=comma)
p2

# We can add a smooth line through our scatterplots with geom_smooth()
p2 + geom_smooth()

# We can fit a straight linear regression line setting method="lm"
p2 + geom_smooth(method="lm")

# We can add both smooth and straight lines
p2 + 
  geom_smooth(se=FALSE, color="red") +
  geom_smooth(method="lm", se=FALSE)


# Let's zoom in on the plots to get a closer look at what's going on in the 
# lower left corner. We can zoom in using coord_cartesian(). Below we set the 
# x-axis limits between 0 and 5000, and the y-axis limits between 0 and
# 1,000,000.
p2 + 
  geom_smooth(se=FALSE, color="red") +
  geom_smooth(method="lm", se=FALSE) +
  coord_cartesian(xlim = c(0, 5000), ylim = c(1,1e6))



# To add a title we can use labs()
p2 + 
  geom_smooth(se=FALSE, color="red") +
  geom_smooth(method="lm", se=FALSE) +
  coord_cartesian(xlim = c(0, 5000), ylim = c(1,1e6)) +
  labs(title = "Total Values vs Finished Sq Ft by HS District") 

# Notice the title is left-aligned by default. Here's how we can center it using
# the theme() function.
p2 + 
  geom_smooth(se=FALSE, color="red") +
  geom_smooth(method="lm", se=FALSE) +
  coord_cartesian(xlim = c(0, 5000), ylim = c(1,1e6)) +
  labs(title = "Total Values vs Finished Sq Ft by HS District") +
  theme(plot.title = element_text(hjust = 0.5))



# YOUR TURN #4 ------------------------------------------------------------

# Create a scatterplot of totalvalue vs lotsize, with totalvalue on y axis. 
# - add a smooth
# - zoom in on plot: x-axis (0, 10) y-axis (0, 1e6)
# - Fix the y-axis to show the amount in dollars. 





# Boxplots and stripcharts ------------------------------------------------


# Boxplots are good for visualizing a numeric variable conditional on a
# categorical variable. Let's look at totalvalue by number of fullbaths:

ggplot(homes, aes(x=fullbath, y=totalvalue)) + geom_boxplot()

# Not what we wanted! fullbath is not categorical Can fix by setting group =
# fullbath
ggplot(homes, aes(x = fullbath, y = totalvalue, group = fullbath)) + 
  geom_boxplot()

# and so does this (convert fullbath to factor):
ggplot(homes, aes(x=factor(fullbath), y=totalvalue)) + 
  geom_boxplot() + 
  scale_y_continuous(labels=dollar)

# The expensive homes have rendered this boxplot practically useless. 


# Another option is to create a stripchart, which is basically a 
# scatterplot in one dimension. We can do that with geom_jitter() which is 
# basically geom_point() but with random noise added to the coordinates. Below
# we jitter left and right, but not up and down.

# Notice also I am zooming in
ggplot(homes, aes(x=fullbath, y=totalvalue, group = fullbath)) + 
  geom_jitter(width = 0.3, height = 0, alpha = 1/5) + 
  scale_y_continuous(labels=dollar) +
  coord_cartesian(ylim = c(0,1e6), xlim = c(1,5))



# YOUR TURN #5 ------------------------------------------------------------


# (1) Make a boxplot totalvalue by msdistrict. 



# (2) Make a stripchart of totalvalue by totalvalue.
# - in geom_jitter() set width = 0.4 and alpha = 1/5
# - format y axis as dollar 
# - zoom in on y axis: 0 - $1,000,000

  
  


# Plotting two discrete integer variables ---------------------------------


# Bedroom vs. fullbath
ggplot(homes, aes(x=bedroom,y=fullbath)) + geom_point()

# geom_jitter() can help with this
ggplot(homes, aes(x=bedroom,y=fullbath)) + geom_jitter()

# scales could be better; minor_breaks=NULL turns off the minor grid lines 
ggplot(homes, aes(x=bedroom,y=fullbath)) + geom_jitter(alpha=1/5) +
  scale_x_continuous(breaks=0:12, minor_breaks=NULL) +
  scale_y_continuous(breaks=0:9, minor_breaks=NULL)


# Line Graphs -------------------------------------------------------------


# line graphs are nice for connecting dots and showing a trend over time.

# plot number of houses built per year; 
# need to count up number of homes by YearBuilt
years <- homes %>% 
  group_by(yearbuilt) %>% 
  tally() %>% 
  filter(yearbuilt > 0)
years


# now use geom_line()
ggplot(years, aes(x=yearbuilt, y=n)) + 
  geom_line()

# Let's change the x-axis to show years in increments of 50 years
ggplot(years, aes(x=yearbuilt, y=n)) + 
  geom_line() +
  scale_x_continuous(breaks=seq(1700,2000,50)) +
  labs(x="Year", y="Number of Homes")

# Great time to use ggpl0tly()
p <- ggplot(years, aes(x=yearbuilt, y=n)) + 
  geom_line() +
  scale_x_continuous(breaks=seq(1700,2000,50)) +
  labs(x="Year", y="Number of Homes")
ggplotly(p)


# Plotting with two data frames -------------------------------------------


# ggplot allows us to use multiple data frames when creating a plot. For 
# example, consider the following plot totalvalue vs YearBuilt for the years 
# 1950 - 2016 for homes valued at $1,000,000 or less. Notice we are zoomed in.

ggplot(homes, aes(x=yearbuilt, y=totalvalue)) + 
  geom_point(position=position_jitter(w=0.2,h=0), shape=".") +
  coord_cartesian(xlim=c(1950,2016), ylim=c(0,1e6))

# Let's say we wanted to plot on top of that graph the median totalvalue by
# year. First we need to calculate the median by year. Here's one way
# to do it using dplyr:

homeValues <- homes %>% 
  group_by(yearbuilt) %>% 
  summarize(median_totalvalue = median(totalvalue))
homeValues

# Each geom_xxx() function allows you to supply them with a separate data frame
# and mappings. Below notice the geom_line() function references the homeValues
# data frame and provide a different mapping to the y aesthetic. Also notice we
# have swap the order of data and aesthesics.

ggplot(homes, aes(x=yearbuilt, y=totalvalue)) + 
  geom_point(position=position_jitter(w=0.2,h=0), shape=".") +
  # Notice the next geom uses a new data frame
  geom_line(aes(y = median_totalvalue), homeValues, color = "red", size = 2) +
  coord_cartesian(xlim=c(1950,2016), ylim=c(0,1e6)) 


# Another example

# Boxplot of totalvalues by middle school district, zoomed in on homes
# $1,000,000 or less
ggplot(homes, aes(x = msdistrict, y = totalvalue)) +
  geom_boxplot() +
  coord_cartesian(ylim = c(0,1e6))

# Let's say we wanted to label the medians with the actual amount.

# First we calculate it. Notice we create two versions, one numeric and one 
# formatted in dollar amounts using the dollar function from the scales package.
homeMedians <- homes %>% 
  group_by(msdistrict) %>% 
  summarize(medianValue = median(totalvalue)) %>% 
  mutate(medianValueD = dollar(medianValue))

homeMedians

# Now we use the geom_text() function to add the amount to the boxplot. Notice 
# we use alpha = 1/4 to lighten the boxplot, that the label aesthetic says which
# data will be mapped to the text label, and that we add 25,000 to the 
# positioning so the label is above the median line. (that took some trial and
# error)
ggplot(homes, aes(x = msdistrict, y = totalvalue)) +
  geom_boxplot(alpha = 1/4) +
  # Notice the next geom uses a new data frame
  geom_text(aes(label = medianValueD, y = medianValue + 25000), homeMedians) +
  coord_cartesian(ylim = c(0,1e6))



# Multiple plots in one window --------------------------------------------


# In base R, we usually use par(mfrow=c(i,j)), like so:
par(mfrow=c(1,2))
hist(homes$finsqft)
plot(totalvalue ~ finsqft, data=homes)
par(mfrow=c(1,1))

# We cannot use this approach for ggplot2. The easiest solution is to use the 
# grid.arrange() function in the gridExtra package. To use it, you have to save
# your plots and then call them using grid.arrange().

p1 <- ggplot(homes, aes(x=finsqft)) + geom_histogram() 
p2 <- ggplot(homes, aes(x=finsqft, y=totalvalue)) + geom_point()
grid.arrange(p1, p2, nrow=1) # can also use ncol



# Themes ------------------------------------------------------------------


# Using built-in themes such as theme_bw()
# Before
ggplot(homes, aes(x=finsqft)) + geom_histogram(bins = 40)
# After
ggplot(homes, aes(x=finsqft)) + geom_histogram(bins = 40) + 
  theme_bw()
# another: theme_light
ggplot(homes, aes(x=finsqft)) + geom_histogram(bins = 40) + 
  theme_light()

# To permanently change the theme, use theme_set:
prevTheme <- theme_set(theme_bw())

ggplot(homes, aes(x=finsqft)) + geom_histogram(bins = 40)

ggplot(homes, aes(x=msdistrict, y = finsqft)) + geom_boxplot()


# To restore
theme_set(prevTheme)

# verify
ggplot(homes, aes(x=msdistrict, y = finsqft)) + geom_boxplot()

# see package ggthemes for more themes!
# install.packages("ggthemes")
library(ggthemes)

# Wall Street Journal theme
ggplot(homes, aes(x=msdistrict, y = finsqft)) + 
  geom_boxplot() + 
  theme_wsj()

# fivethirtyeight.com theme
ggplot(homes, aes(x=msdistrict, y = finsqft)) + 
  geom_boxplot() + 
  theme_fivethirtyeight()

# Tufte's theme (based on The Visual Display of Quantitative Information)
ggplot(homes, aes(x=msdistrict, y = finsqft)) + 
  geom_boxplot() + 
  theme_tufte()

# base R theme
ggplot(homes, aes(x=msdistrict, y = finsqft)) + 
  geom_boxplot() + 
  theme_base()



# Appendix: Labelling facets ----------------------------------------------


# We can use the labeller argument and associated labeller functions to modify
# the labels of facets.

# Let's create a 3 x 6 grid of finsqft vs Total Value by hsdistrict and
# Condition. First we'll subset the data.
homes2 <- homes %>% filter(totalvalue < 1e6 & 
                             condition %in% c("Average","Good","Excellent"))

ggplot(homes2, aes(x = finsqft, y = totalvalue)) +
  geom_point(alpha = 1/5) +
  facet_grid(condition ~ hsdistrict)

# labeller = label_both adds the variable name and value
ggplot(homes2, aes(x = finsqft, y = totalvalue)) +
  geom_point(alpha = 1/5) +
  facet_grid(condition ~ hsdistrict, labeller = label_both)

# We use the labeller function with the labeller argument to control labelling
# for each facet. For example, wrap high school name after 10 characters and show both
# variable and value label for Condition.
ggplot(homes2, aes(x = finsqft, y = totalvalue)) +
  geom_point(alpha = 1/5) +
  facet_grid(condition ~ hsdistrict, labeller = labeller(hsdistrict = label_wrap_gen(10),
                                                   condition = label_both))


# Appendix: Using RColorBrewer for color palettes -------------------------


# The RColorBrewer package provides color schemes for maps (and other graphics)
# designed by Cynthia Brewer as described at http://colorbrewer2.org

# ggplot2 provides functions for easily incorporating these color schemes into
# your plots.

# There are three types of color schemes:

# 1. sequential - suited to ordered data that progress from low to high

# 2. qualitative - suited for nominal or categorical data

# 3. diverging -  put equal emphasis on mid-range critical values and extremes
#                 at both ends of the data range

# See all color schemes
# sequential, qualitative, diverging
# Use the names on the left of the colors to specify a color scheme
library(RColorBrewer)
# sequential color palettes
display.brewer.all(type = "seq")
# qualitative color palettes
display.brewer.all(type = "qual")
# divergent color palettes
display.brewer.all(type = "div")

# To modify color or fill aesthetic, use scale_color_brewer or
# scale_fill_brewer, respectively, and specify the palette

# Example 1: sequential color scheme using Blues palette
# View the palette (optional)
display.brewer.all(type = "seq", select = "Blues")
# Use the palette in a plot
ggplot(filter(homes, totalvalue < 1e6), 
       aes(x = totalvalue, fill = condition)) + 
  geom_histogram() +
  scale_fill_brewer(palette = "Blues")

# Example 2: qualitative color scheme using Accent palette
# View the palette (optional)
display.brewer.all(type = "qual", select = "Accent")
# Use the palette in a plot
ggplot(filter(homes, totalvalue < 1e6 & yearbuilt > 0), 
       aes(x = yearbuilt, fill = hsdistrict)) + 
  geom_histogram() +
  scale_fill_brewer(palette = "Accent")

# Example 3: diverging color scheme using PiYG palette
# View the palette (optional)
display.brewer.all(type = "div", select = "PiYG")
# Create a divergent variable: scaled finsqft
# first scale then cut into categories
homes <- mutate(homes, finsqftZ = scale(finsqft)[,1]) %>% 
  mutate(finsqftZ = cut(finsqftZ, quantile(finsqftZ,
                                           probs = seq(0, 1, length.out = 6)),
                        labels = c("Very Small", "Below Average", "Average", "Above Average", "Very Large"),
                        include.lowest = TRUE))
# Use the palette in a plot
ggplot(subset(homes, totalvalue < 1e6), 
       aes(x = hsdistrict, fill = finsqftZ)) +
  geom_bar(position = "fill") +
  scale_fill_brewer("Size of House", type = "div", palette = "PiYG")


# Appendix: defining your own color palette -------------------------------

# There are many ways to define your own color palettes in R. Here are a few
# relatively easy methods.

# Below we use HEX values, but you can also use color names. Enter colors() at
# the console to see all 657 available.

# Example 1: Using scale_color_manual with specific values for a categorical
# variable
ggplot(filter(homes, totalvalue < 1e6 & 
                condition %in% c("Average","Good","Excellent","Fair")), 
       aes(x = finsqft, y = totalvalue, color = condition)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  # 2016 color palettes: https://www.w3schools.com/colors/colors_palettes.asp
  scale_color_manual(values = c("#92a8d1", "#034f84", "#f7cac9", "#f7786b"))

# Example 2: Using scale_color_gradientn with continuous data to create a custom
# 4-color gradient.
ggplot(filter(homes, totalvalue < 1e6 & 
                condition %in% c("Average","Good","Excellent","Fair")), 
       aes(x = condition, y = totalvalue, color = finsqft)) +
  geom_jitter(height = 0) +
  scale_y_log10(labels = scales::dollar) +
  # 2016 color palettes: https://www.w3schools.com/colors/colors_palettes.asp
  scale_color_gradientn(colours = c("#92a8d1", "#034f84", "#f7cac9", "#f7786b"))

# Example 3: Using scale_color_gradient with continuous data to create a custom
# 2-color, low-high gradient
ggplot(filter(homes, totalvalue < 1e6 & 
                condition %in% c("Average","Good","Excellent","Fair")), 
       aes(x = condition, y = totalvalue, color = finsqft)) +
  geom_jitter(height = 0) +
  scale_y_log10(labels = scales::dollar) +
  # 2016 color palettes: https://www.w3schools.com/colors/colors_palettes.asp
  scale_color_gradient(low = "#d5f4e6", high = "#618685")


# Example 4: Using scale_color_gradient2 with continuous data to create a custom
# diverging color gradient. Note we scale finsqft so average is 0.
ggplot(filter(homes, totalvalue < 1e6 & 
                condition %in% c("Average","Good","Excellent","Fair")), 
       aes(x = condition, y = totalvalue, color = scale(finsqft))) +
  geom_jitter(height = 0) +
  scale_y_log10(labels = scales::dollar) +
  # 2016 color palettes: https://www.w3schools.com/colors/colors_palettes.asp
  scale_color_gradient2("Scaled\nfinsqft", low = "#4040a1", mid = "white", high = "#bc5a45")


# Appendix: adding uncertainty to graphs ----------------------------------


# Example: add means and CIs to strip chart 
# Data: chickwts (An experiment was conducted to measure and compare the
# effectiveness of various feed supplements on the growth rate of chickens.)
str(chickwts)

sc <- ggplot(chickwts, aes(x=feed, y=weight)) + 
  geom_jitter(width = 0.1, height = 0)
sc

# now to calculate means and standard errors
chick2 <- chickwts %>% 
  group_by(feed) %>% 
  summarise(fMean = mean(weight),
            fSE = sd(weight)/sqrt(length(weight)))

# now add mean and error bars
sc + geom_point(data=chick2, aes(x=feed, y=fMean), color="red", size=3) +
  geom_errorbar(data=chick2, aes(x=feed, y=fMean,
                                 ymin=fMean - 2*fSE, 
                                 ymax=fMean + 2*fSE), 
                width=0.1, color="red") +
  labs(title="Mean Weight by Feed Type with 2*SE error bars")

# another way using stat_summary; fun.data="mean_cl_normal" actually calls the 
# function smean.cl.normal() from the Hmisc package. It uses the t distribution
# to determine the multiplier of the standard error.
ggplot(chickwts, aes(x=feed, y=weight)) + 
  geom_point(position = position_jitter(w = 0.1, h = 0)) +
  stat_summary(fun.data = "mean_cl_normal", color="red", geom="errorbar", width=0.1) +
  stat_summary(fun.y = "mean", geom="point", color="red", size=3) +
  labs(title="Mean Weight by Feed Type with 95% error bars")



# single line graph of means at each time point with SE bars

# Indometh data (comes with R)
# Six subjects were given an intravenous injection of indomethacin at 11 times,
# and each time plasma concentrations of indometacin was measured.

# Here we make a line plot for each subject:
ggplot(Indometh, aes(x=time,y=conc, group=Subject)) +
  geom_line()

# Let's say we wanted to create a single line graph of means at each time point
# with SE bars.

# first calculate means and SEs
Indo2 <- Indometh %>% 
  group_by(time) %>% 
  summarise(tMean = mean(conc),
            tSE = sd(conc)/sqrt(length(conc)))

# now ready to create plot
ggplot(Indo2, aes(x=time,y=tMean)) +
  geom_line() +
  geom_errorbar(aes(ymin=tMean-2*tSE, ymax=tMean+2*tSE), width=0.1)

# or again we can use stat_summary()
ggplot(Indometh, aes(x=time,y=conc)) +
  stat_summary(fun.data="mean_cl_normal", geom="errorbar", width=0.1) +
  stat_summary(fun.y = "mean", geom = "line")


# Appendix: Log Transformations in scatter plots --------------------------


# Skewed positive data are often log transformed. It helps "squeeze together"
# the large values and "spread out" the small values.

# before log transformation:
ggplot(homes, aes(x=finsqft, y=totalvalue)) + geom_point()

# Maybe try log base 10 transformation directly:
ggplot(homes, aes(x=log10(finsqft), y=log10(totalvalue))) + geom_point(alpha = 1/5)

# The scale of the axes is on the log10 scale because the data has been
# transformed.

# We can use scale functions to both transform the data and map the scale of the
# axes to the original data.
ggplot(homes, aes(x=finsqft, y=totalvalue)) + 
  geom_point(alpha=1/5) +
  scale_x_log10(labels=comma) + 
  scale_y_log10(labels=dollar)

# We can see the non-linear nature of the scales by manually defining the breaks
ggplot(homes, aes(x=finsqft, y=totalvalue)) + 
  geom_point(alpha=1/5) +
  scale_x_log10(labels=comma, breaks = seq(0, 1e4, 2e3)) +  # 0 to 10,000 in steps of 2,000
  scale_y_log10(labels=dollar, breaks = seq(0, 10e6, 2e6))  # 0 to 10,000,000 in steps of 2,000,000

# To do the same with the natural log scale, we can use the log_trans() function
# from the scales package. I prepended scales:: to the function to indicate the
# function is from the scales package, though technically we don't need it since
# we loaded the scales package at the top of the script.
ggplot(homes, aes(x=finsqft, y=totalvalue)) + 
  geom_point(alpha=1/5) +
  scale_x_continuous(labels=comma, trans = scales::log_trans()) + 
  scale_y_continuous(labels=dollar, trans = scales::log_trans())




# Appendix: plotting dates and date-times ---------------------------------

# The scale_*_date and scale_*_datetime functions provide extra control over
# scales that involve dates and date-times.

# Read in stock price data and format date column
stock_prices <- read.csv("http://people.virginia.edu/~jcf2d/data/foxa.csv", 
                         col.names = c("date","open","high","low","close","volume"),
                         stringsAsFactors = FALSE)
stock_prices$date <- as.Date(stock_prices$date, format = "%d-%b-%y")

# default plot of closing price over time
ggplot(stock_prices, aes(x = date, y = close)) + geom_line()

# Examples of using scale_x_date to change scale of x axis

# Every 2 months
ggplot(stock_prices, aes(x = date, y = close)) + 
  geom_line() +
  scale_x_date(date_breaks = "2 months")

# Every 2 months with formatted labels as Month Year
# see ?strptime for date codes such as %B and %Y
ggplot(stock_prices, aes(x = date, y = close)) + 
  geom_line() +
  scale_x_date(date_breaks = "2 months", 
               date_labels = "%B %Y")

# Every 6 weeks with minor breaks set to 1 week and labels formatted as
# Abbreviated Month
ggplot(stock_prices, aes(x = date, y = close)) + 
  geom_line() +
  scale_x_date(date_breaks = "6 weeks", 
               minor_breaks = "1 week", 
               date_labels = "%b")




# Appendix: GGally --------------------------------------------------------

# 'GGally' extends 'ggplot2' by adding several functions, including a pairwise
# plot matrix. Here's a quick demo.

# install.packages("GGally")
library(GGally)

# Learn more: http://ggobi.github.io/ggally/
# Beware: these functions can be slow

# the ggpairs function produces a pairwise comparison of multivariate data. 
homes %>% select(finsqft, lotsize, totalvalue, bedroom, fp) %>% 
  ggpairs()

# ggscatmat is similar to ggpairs but only works for purely numeric multivariate
# data.
homes %>% select(finsqft, lotsize, totalvalue, bedroom, fp) %>% 
  ggscatmat(columns = 1:3, color = "fp")




# Appendix: maps with leaflet ---------------------------------------------

# The following data are occurences of cougars in the US. It was downloaded from
# Biodiversity Information Serving Our Nation (BISON),
# http://bison.usgs.ornl.gov.

URL2 <- "http://people.virginia.edu/~jcf2d/workshops/ggplot2/bison-Cougar-20150520-172801.csv"
cougar <- read_csv(URL2)

# drop records if they're missing data for Longitude.
cougar <- filter(cougar, !is.na(decimalLongitude))  

# Load the leaflet package
library(leaflet)

# quickly make a map of the world
leaflet() %>% 
  addProviderTiles("CartoDB")

# Make a map with markers labeling cougar occurences. Notice we use the tilde
# (~) to refer to columns in the cougar data frame
leaflet() %>% 
  addProviderTiles("CartoDB") %>% 
  addMarkers(data = cougar, 
             lng = ~decimalLongitude, 
             lat = ~decimalLatitude,
             popup = ~paste0(basisOfRecord, "<br>", ITIScommonName))

# Scroll or double-click to zoom in on map. Click a marker to view information