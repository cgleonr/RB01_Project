# Life expectancy trends through the years maybe?
# For Rmd Intro, could use an analysis of life expectancy trends around the world
# Then propose to investigate its link to food

# Import libraries and dataset

# Next task: look through all unique "Unit" values, decide which to keep.
# e.g.: kcal/cap/d, g/cap/d, etc.

## Looking at the resulting data from data cleaning:
# Possible Questions:
# - Top 10 vs mid 10 vs bottom 10
# - How does caloric intake relate to life expectancy?
# - How does the macronutrient balance relate to caloric intake?
#   - High protein vs. high carb diets and caloric intake vs food volume
# - Are plant-based diets conducive to longer lives vs including animal foods?

library(ggplot2)
library(dplyr)
library(gganimate) # animates ggplots

df <- read.csv("clean_datasets/final_df.csv")
head(df)
dim(df)

# Filter only 2019 values
df_2019 <- df %>% filter(Year == 2019)

# Remove duplicates and sort by Life.Expectancy
df_2019_unique <- df_2019 %>% 
  distinct(Area, .keep_all = TRUE) %>% 
  arrange(desc(Life.Expectancy))
df_2019_unique
# Calculate the middle 10 indices
mid_start <- round(length(df_2019_unique[,1])/2 - 5)
mid_end <- round(length(df_2019_unique[,1])/2 + 4)

# Select top 10, middle 10, and bottom 10 countries
top_10 <- df_2019_unique %>% slice(1:10) %>% mutate(Category = "Top")
mid_10 <- df_2019_unique %>% slice(mid_start:mid_end) %>% mutate(Category = "Mid")
mid_10 <- mid_10 %>% slice(-9) %>% mutate(Category = "Mid") # grabs 11, but #9 has a char error
bot_10 <- df_2019_unique %>% slice((n() - 9):n()) %>% mutate(Category = "Bot")

# Combine them into one dataframe
subset_df <- bind_rows(top_10, mid_10, bot_10)


# Filter the original dataframe for the selected countries and years
df_filtered <- df %>%
  filter(Area %in% subset_df$Area, Year >= 2015, Year <= 2019) %>%
  left_join(subset_df %>% select(Area, Category), by = "Area")


ggplot(df_filtered, aes(x = Year, y = Life.Expectancy, color = Category, group = Area)) +
  geom_line(size = 1) +
  geom_point() +
  labs(title = "Life Expectancy (2015-2019)", 
       x = "Year", 
       y = "Life Expectancy") +
  theme_minimal() +
  scale_color_manual(values = c("Top" = "blue", "Mid" = "green", "Bot" = "red"))





