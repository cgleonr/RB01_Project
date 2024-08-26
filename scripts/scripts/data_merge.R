# get regional data or capital cities to include coordinate data
# could try using 'merge' or 'left/right/inner/outer_join commands

# Importing packages
library(readxl)
library(dplyr)

food_consumption <- read.csv("datasets/FAOSTAT_data_en_8-26-2024.csv")
deathrates_by_country <- read_xlsx("datasets/ghe2021_deathrates_bycountry_asdr.xlsx")
deaths_by_country <- read_xlsx("datasets/ghe2021_deaths_bycountry_2021.xlsx")
life_expectancy <- read.csv("datasets/Life Expectancy.csv")