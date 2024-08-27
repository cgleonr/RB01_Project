# Exploring relationship between caloric intake and longevity
library(dplyr)
library(ggplot2)

df <- read.csv("clean_datasets/final_df.csv", row.names = NULL)
df <- df[ , !names(df) %in% c("X")]

str(df)
summary(df)

# Filter the dataset for total caloric intake
caloric_data <- df %>%
  filter(Element == "Food supply (kcal/capita/day)" & Unit == "kcal/cap/d") %>%
  group_by(Country, Year) %>%
  summarize(Total_Calories = sum(Value, na.rm = TRUE)/2) #2 because total was both sexes

# View the first few rows to ensure it was done correctly
head(caloric_data)

# Merge caloric data with life expectancy data
merged_data <- df %>%
  filter(Element == "Life expectancy") %>%
  select(Country, Year, Life_Expectancy = Value) %>%
  inner_join(caloric_data, by = c("Country", "Year"))

# View the first few rows to ensure it was done correctly
head(merged_data)
