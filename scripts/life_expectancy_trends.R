# Life expectancy trends through the years maybe?
# For Rmd Intro, could use an analysis of life expectancy trends around the world
# Then propose to investigate its link to food

# Import libraries and dataset
library(ggplot2)
library(dplyr)
df <- read.csv("clean_datasets/final_df.csv")
head(df)
dim(df)

# Next task: look through all unique "Unit" values, decide which to keep.
# e.g.: kcal/cap/d, g/cap/d, etc.

## Looking at the resulting data from data cleaning:
# Possible Questions:
# - Top 10 vs mid 10 vs bottom 10
# - How does caloric intake relate to life expectancy?
# - How does the macronutrient balance relate to caloric intake?
#   - High protein vs. high carb diets and caloric intake vs food volume
# - Are plant-based diets conducive to longer lives vs including animal foods?