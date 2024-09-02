# Exploring relationship between caloric intake and longevity
library(dplyr)
library(ggplot2)

df <- read.csv("clean_datasets/final_df.csv", row.names = NULL)
df <- df[ , !names(df) %in% c("X")]

# Filter the dataset for total caloric intake
caloric_data <- df %>%
  filter(Element == "Food supply (kcal/capita/day)" & Unit == "kcal/cap/d") %>%
  group_by(Country, Year) %>%
  summarize(Total_Calories = sum(Value, na.rm = TRUE)/2) #2 because total was both sexes

life_exp <- df %>% 
  select(Country, Year, Life.Expectancy)

# Merge caloric data with life expectancy data
merged_df <- caloric_data %>%
  left_join(life_exp, by = c("Country", "Year"))

# select distinct
unique_data <- merged_df %>%
  distinct(Country, Year, .keep_all = TRUE)

# View the first few rows of the unique data
head(unique_data)

# Cals Consumed vs Life Expectancy
ggplot(unique_data, aes(x = Total_Calories, y = Life.Expectancy)) +
  geom_point(aes(color = as.factor(Year)), size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "Relationship Between Total Caloric Intake and Life Expectancy",
       x = "Total Calories Consumed (kcal/capita/day)",
       y = "Life Expectancy (Years)",
       color = "Year") +
  theme_minimal()
summary(lm(Life.Expectancy~Total_Calories, unique_data))



# Calculating differences in cal intake and LE
difference_data <- unique_data %>%
  group_by(Country) %>%
  reframe(
    Caloric_Intake_Diff = Total_Calories[Year == 2019] - Total_Calories[Year == 2015],
    Life_Expectancy_Diff = Life.Expectancy[Year == 2019] - Life.Expectancy[Year == 2015]
  )

difference_data

# Assuming subset_df contains the 'Country' and 'Category' columns
category_data <- subset_df %>% 
  select(Country, Category) %>% 
  distinct()

# Merge the category information into difference_data
difference_data <- difference_data %>%
  left_join(category_data, by = "Country")

# View the updated dataframe
head(difference_data)
# save to other df for visuals
difference_visual_data <- na.omit(difference_data)

# relationship between category and change in cal intake
ggplot(difference_visual_data, aes(x = Category, y = Caloric_Intake_Diff, fill = Category)) +
  geom_boxplot() +
  labs(title = "Change in Caloric Intake by Category (2015-2019)",
       x = "Category",
       y = "Change in Caloric Intake (kcal/capita/day)") +
  scale_fill_manual(values = c("Top" = "blue", "Mid" = "green", "Bot" = "red")) +
  theme_minimal()

# Change in caloric intake vs life expectancy
ggplot(difference_visual_data, aes(x = Caloric_Intake_Diff, y = Life_Expectancy_Diff, color = Category)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Changes in Caloric Intake vs. Life Expectancy by Category (2015-2019)",
       x = "Change in Caloric Intake (kcal/capita/day)",
       y = "Change in Life Expectancy (Years)") +
  scale_color_manual(values = c("Top" = "blue", "Mid" = "green", "Bot" = "red")) +
  theme_minimal()
