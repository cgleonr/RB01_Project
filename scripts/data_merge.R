# Importing packages
library(readxl)
library(tidyverse)
library(dplyr)

food_consumption <- read.csv("datasets/FAOSTAT_data_en_8-28-2024.csv")
life_expectancy <- read.csv("datasets/Life_expectancy_2000_to_2021.csv")
countries <- read.csv("datasets/country-capital-lat-long-population.csv")
head(food_consumption)
head(life_expectancy)
head(countries)
# merge food consumption per country
combined_data <- left_join(food_consumption, countries, by = c("Area" = "Country"))
# verify data
head(combined_data)

#Check columns to drop
colnames(combined_data)
# Drop: Domain.Code, Domain, Area.Code..M49, Element.Code, Item.Code..FBS., Year.Code, Flag, Flag Description, Note
drop_list <-c("X...Domain.Code", "Domain", "Area.Code..M49.","Element.Code",
              "Item.Code..FBS.", "Year.Code", "Flag", "Flag.Description", "Note")

df <- combined_data[, !(names(combined_data) %in% drop_list)]
head(df)
# clean up life_expectancy and merge to df
colnames(life_expectancy)
head(life_expectancy)
le_df <- life_expectancy[, c("Location", "Period", "FactValueNumericHigh", "Dim1")]
colnames(le_df) <- c("Country", "Year", "Life.Expectancy", "Sex")
head(le_df)
le_df$Year <- as.integer(le_df$Year)
rownames(le_df) <- NULL

# Keep highest value for life expectancy for each country
le_df <- le_df %>%
  group_by(Country, Year) %>%
  slice_max(Life.Expectancy, n = 1) %>%
  ungroup() %>% 
  arrange(Country)

# keep the same years in both datasets to have complete data
df <- left_join(df, le_df, by = c("Area"="Country", "Year"))
df <- df[, !names(df) %in% c("Note")]
head(df)
head(le_df)

# Life.Expectancy column has datatype "character" when it should be numeric
summary(df)
class(df$Life.Expectancy)
# Convert to numeric
df$Life.Expectancy <- as.numeric(df$Life.Expectancy)

# Drop NA values
df <- na.omit(df)
names(df)[names(df) == "Area"] <- "Country"

summary(df)
dim(df)

# Save data of interest to new .csv file
write.csv(df, "clean_datasets/final_df.csv")