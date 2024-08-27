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

df <- combined_data[, !(names(combined_data) %in% drop_list)]

# clean up life_expectancy and merge to df
le_df <- life_expectancy[, c("X", "X.1", "Life.expectancy.at.birth..years.")]
colnames(le_df) <- c("Country", "Year", "Life.Expectancy")
le_df <- le_df[-1,]
le_df$Year <- as.integer(le_df$Year)
rownames(le_df) <- NULL
head(le_df)

# keep the same years in both datasets to have complete data
df <- inner_join(df, le_df, by = c("Area"="Country", "Year"))
df <- df[, !names(df) %in% c("Note")]

# Life.Expectancy column has datatype "character" when it should be numeric
summary(df)
class(df$Life.Expectancy)
# Convert to numeric
df$Life.Expectancy <- as.numeric(df$Life.Expectancy)

# Drop NA values
df <- na.omit(df)

summary(df)
dim(df)


# Save data of interest to new .csv file
write.csv(df, "clean_datasets/final_df.csv")