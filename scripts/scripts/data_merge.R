# Importing packages
library(readxl)
library(tidyverse)
library(dplyr)

food_consumption <- read.csv("datasets/FAOSTAT_data_en_8-26-2024.csv")
life_expectancy <- read.csv("datasets/Life Expectancy.csv")
countries <- read.csv("datasets/country-capital-lat-long-population.csv")

# merge food consumption per country
combined_data <- left_join(food_consumption, countries, by = c("Area" = "Country"))
# verify data
head(combined_data)

#Check columns to drop
colnames(combined_data)
# Drop: Domain.Code, Domain, Area.Code..M49, Element.Code, Item.Code..FBS., Year.Code, Flag, Flag Description
drop_list <-c("Domain.Code", "Domain", "Area.Code..M49.","Element.Code",
              "Item.Code..FBS.", "Year.Code", "Flag", "Flag.Description")

life_expectancy <- life_expectancy[,c("Countries, territories and areas", "Life.expectancy.at.birth..years.")]

df <- combined_data[, !(names(combined_data) %in% drop_list)]
df <- left_join(df, life_expectancy$)
head(df)
