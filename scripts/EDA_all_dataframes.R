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

################################################################################
#Libraries
################################################################################

library(ggplot2)
library(dplyr)
library(plotly) # for interactive plots
library(gganimate) # animates ggplots
library(fmsb)
library(maptiles) # for maps
library(sf) # for maps
library(tidyterra) # for maps
library(leaflet) # for hovering over the map

# Colors
color_top <- "#009E73"
color_mid <- "#56B4E9"
color_bot <- "#D55E00"

color_protein <- "darkred"
color_fats <- "yellow3"
color_cals <- "purple1"

################################################################################

# Plot Life expectancy development of the top, mid and bottom 10 countries
ggplot(df_filtered, aes(x = Year, y = Life.Expectancy, color = Category, group = Country)) +
  geom_line(size = 1) +
  geom_point() +
  labs(title = "Life Expectancy (2010-2021)", 
       x = "Year", 
       y = "Life Expectancy") +
  theme_minimal() +
  scale_color_manual(values = c("Top" = "blue", "Mid" = "green", "Bot" = "red"))

#################################################################################
# Creating the Dataframes:
# df - raw, cleaned dataframe
head(df)
# df_2019 - only values from 2019 (subset df)
head(df_2021)
# focus_df - has top, middle and bottom 10 countries, all rows, all cols
head(focus_df)
# total_caloric_intake - total caloric intake for focus countries in 2015, 2019
head(total_caloric_intake)
# life_exp - supplementary df, contains focus countries, year, life expectancy
head(life_exp) shape(life_exp)
# total_caloric_intake - supplementary df, contains focus countries, year, cal intake
head(total_caloric_intake)
# merged_cal_longevity_df - merged df, life_exp and total_caloric_intake
head(merged_cal_longevity_df)
# unique_cal_long_df - like merged_cal_longevity_df, but one entry per year
head(unique_cal_long_df)
# difference_kcal_LE - yearly difference in cal intake and longevity (2019 - 2015)
head(difference_kcal_LE)
# caloric_sources_2015 - all caloric sources from focus group, {year} (kcal)
head(caloric_sources)
# macro_sources_2015 - protein & fat sources from focus group, {year} (grams)
head(macro_sources)
# macro_df - a calculation of calories from fats and proteins
head(macro_df)
# avg_macro_by_group - an average of calories by macro, pero country in focus group
head(avg_macro_by_group)
# model_df - a dataframe including only Country, Year and Life.Expectancy, for model training
################################################################################
# Load in DF
df <- read.csv("clean_datasets/final_df.csv")
# drop indices (column "X")
df <- df[ , !names(df) %in% c("X")]

# Filter only 2021 values
df_2021 <- df %>% filter(Year == 2021)
head(df_2021)

# Remove duplicates and sort by Life.Expectancy
df_2021_unique <- df_2021 %>% 
  distinct(Country, .keep_all = TRUE) %>% 
  arrange(desc(Life.Expectancy))
head(df_2021_unique)

# Calculate the middle indices
mid_start <- round(length(df_2021_unique[,1])/2 - 5)
mid_end <- round(length(df_2021_unique[,1])/2 + 5)

# Select top 10, middle 10, and bottom 10 countries
top_10 <- df_2021_unique %>% slice(1:10) %>% mutate(Category = "Top")
mid_10 <- df_2021_unique %>% slice(mid_start:mid_end) %>% 
  mutate(Category = "Mid") %>% slice(-9) # Picks 11, #9 has error
bot_10 <- df_2021_unique %>% slice((n() - 9):n()) %>% mutate(Category = "Bot")

# Combine them into one dataframe
subset_df <- bind_rows(top_10, mid_10, bot_10)

# Combine them into one dataframe
focus_df <- bind_rows(top_10, mid_10, bot_10) # Holds top, mid bot 10

# Filter the original dataframe for the selected countries and years
df_filtered <- df %>%
  filter(Country %in% subset_df$Country, Year >= min(df$Year), Year <= max(df$Year)) %>%
  left_join(subset_df %>% select(Country, Category), by = "Country")

# now holds all data for top, mid, bot 10
focus_df <- df %>%
  filter(Country %in% focus_df$Country, Year >= min(df$Year), Year <= max(df$Year)) %>%
  left_join(focus_df %>% select(Country, Category), by = "Country")

# Filter the dataset for total caloric intake
total_caloric_intake <- focus_df %>%
  filter(Element == "Food supply (kcal/capita/day)" & Unit == "kcal/cap/d") %>%
  group_by(Country, Year) %>%
  summarize(Total.Calories = sum(Value, na.rm = TRUE)/2) #2 because total was both sexes

life_exp <- df %>% 
  select(Country, Year, Life.Expectancy)

# Merge caloric data with life expectancy data
merged_cal_longevity_df <- total_caloric_intake %>%
  left_join(life_exp, by = c("Country", "Year"))

# select distinct
unique_cal_long_df <- merged_cal_longevity_df %>%
  distinct(Country, Year, .keep_all = TRUE)

# Calculating differences in cal intake and LE
difference_kcal_LE <- unique_cal_long_df %>%
  arrange(Country, Year) %>%  # Ensure data is ordered by Country and Year
  group_by(Country) %>%
  mutate(
    Caloric.Intake.Diff = Total.Calories - lag(Total.Calories),
    Life.Expectancy.Diff = Life.Expectancy - lag(Life.Expectancy)
  ) %>%
  ungroup()

# View the result
head(difference_kcal_LE)

# Select the 'Country' and 'Category' columns
category_data <- focus_df %>% 
  select(Country, Category) %>% 
  distinct()
head(category_data)

# deselect repeated columns
diff_kcal_LE_selected <- difference_kcal_LE %>% 
  select(-Life.Expectancy)

# Merge the category information into difference_data
focus_df <- focus_df %>%
  left_join(diff_kcal_LE_selected, by = c("Country","Year"))
head(focus_df)

# Up to this point, focus_df has all columns except total calories, since the calories are all together.
# We can try to get only the rows under Food which are:
# Food supply (kcal/capita/day), to see caloric sources, and the same for grams.

# Filter for caloric sources 
caloric_sources <- focus_df %>%
  filter(Element == "Food supply (kcal/capita/day)")

# Filter for macro grams sources 2015
macro_sources <- focus_df %>%
  filter(Element == c("Protein supply quantity (g/capita/day)",
                      "Fat supply quantity (g/capita/day)"))

# Filter the dataframe for relevant elements (Protein, Fat, and Total Calories)
macro_df <- focus_df %>%
  filter(Element %in% c("Protein supply quantity (g/capita/day)", 
                        "Fat supply quantity (g/capita/day)", 
                        "Food supply (kcal/capita/day)"))  # Total calories

# Convert protein and fat values to calories
macro_df <- macro_df %>%
  mutate(Calories = case_when(
    Element == "Protein supply quantity (g/capita/day)" ~ Value * 4,
    Element == "Fat supply quantity (g/capita/day)" ~ Value * 9,
    Element == "Food supply (kcal/capita/day)" ~ Value
  ))

head(macro_df)

avg_macro_by_group <- macro_df %>%
  group_by(Year, Category, Element) %>%
  summarize(Average_Calories = mean(Calories, na.rm = TRUE)) %>%
  ungroup()
head(avg_macro_by_group)

focus_df <- focus_df %>%
  mutate(Category = factor(Category, levels = c("Top", "Mid", "Bot"),
                           labels = c("Top", "Middle", "Bottom")))
macro_df <- macro_df %>%
  mutate(Category = factor(Category, levels = c("Top", "Mid", "Bot"),
                           labels = c("Top", "Middle", "Bottom")))
mean_life_exp <- focus_df %>%
  filter(Year == 2021) %>%
  group_by(Country, Category) %>%
  summarize(Mean_Life_Expectancy = mean(Life.Expectancy, na.rm = TRUE)) %>%
  ungroup()

# calculate the ratio of vegetal vs animal products consumed by the three country categories
head(focus_df)
filtered_df_vegetal_animal_prod <- focus_df %>%
  filter(Element == "Food supply (kcal/capita/day)" & 
           (Item == "Vegetal Products" | Item == "Animal Products" | Item == "Year"))

filtered_df_vegetal_animal_prod_ratio <- filtered_df_vegetal_animal_prod %>%
  group_by(Country, Year, Category) %>%  # Include Category in the grouping
  summarise(
    Vegetal = sum(Value[Item == "Vegetal Products"], na.rm = TRUE),
    Animal = sum(Value[Item == "Animal Products"], na.rm = TRUE),
    Ratio_Vegetal_Animal = Vegetal / Animal,
    .groups = 'drop'  # To prevent dplyr summarise warning about grouping
  )
print(filtered_df_vegetal_animal_prod_ratio)
head(filtered_df_vegetal_animal_prod_ratio)

################################################################################
# Freeform Visuals
################################################################################

# Plot 1 - Change in Life Expectancy from 2010-2019, focus countries
ggplot(focus_df, aes(x = Year, y = Life.Expectancy, color = Category, group = Country)) +
  geom_line(size = 1) +
  geom_point() +
  labs(title = "Life Expectancy (2010-2021)", 
       x = "Year", 
       y = "Life Expectancy (years)") +
  theme_minimal() +
  scale_color_manual(values = c("Top" = color_top, "Mid" = color_mid, "Bot" = color_bot))

# Plot 1.1 - Change in Life Expectancy from 2010 - 2019, focus countries - interactive
int_plot_life_exp <- ggplot(focus_df, aes(x = Year, y = Life.Expectancy, color = Category, group = Country, text = Country)) +
  geom_line(size = 1) +
  geom_point() +
  labs(title = "Life Expectancy (2010-2021)", 
       x = "Year", 
       y = "Life Expectancy (years)") +
  theme_minimal() +
  scale_color_manual(values = c("Top" = "#009E73", "Mid" = "#56B4E9", "Bot" = "#D55E00"))

int_plot_life_exp_display <- ggplotly(int_plot_life_exp, tooltip = "text")

int_plot_life_exp_display


# Plot 2 - Focus group's caloric intake trends 2010-2021
ggplot(focus_df, aes(x=Year, y=Total.Calories, colour = Category, group = Country)) +
  geom_line(size=1) +  
  geom_point() +
  labs(title = "Change in Caloric Intake (2010-2021)",
       x = "Year",
       y = "Average Daily Intake (kcal)")

# Plot 2.1 - Focus group's caloric intake trends 2010 - 2021, interactive
int_plot_caloric_intake <- ggplot(focus_df, aes(x = Year, y = Total.Calories, colour = Category, group = Country, text = Country)) +
  geom_line(size = 1) +  
  geom_point() +
  labs(title = "Change in Caloric Intake (2010-2021)",
       x = "Year",
       y = "Average Daily Intake (kcal)") +
  theme_minimal() +
  scale_color_manual(values = c("Top" = "#009E73", "Mid" = "#56B4E9", "Bot" = "#D55E00"))

int_plot_caloric_intake_display <- ggplotly(int_plot_caloric_intake, tooltip = "text")

int_plot_caloric_intake_display

# Plot 3(a) - Top group kcal Trends
focus_top <- focus_df %>% filter(Category == "Top")
ggplot(focus_top, aes(x=Year, y=Total.Calories)) +
  geom_point() +
    geom_smooth(method = "lm", se = FALSE, color = color_top)
    labs(title = "Change in Caloric Intake for Top 10 Countries(2010-2021)",
       x = "Year",
       y = "Average Daily Intake (kcal)")

# Plot 3(a.a) - Top group kcal Trends - colour coded and interactive
    # I would go with a plot with all three groups together
focus_top <- focus_df %>% filter(Category == "Top")

int_plot_caloric_intake_mid <- ggplot(focus_top, aes(x = Year, y = Total.Calories, text = Country)) +
      geom_line(aes(group = Country), color = "#009E73", size = 1) +  # Connect dots with lines, set color to green
      geom_point(color = "#009E73", size = 2) +  # Points colored with HEX code #009E73
      labs(title = "Change in Caloric Intake for Middle 10 Countries (2010 - 2021)",  # Updated title
           x = "Year",  # Updated x-axis label
           y = "Average Daily Intake (kcal)") +  # Updated y-axis label
      theme_minimal()
    
int_plot_caloric_intake_mid_display <- ggplotly(int_plot_caloric_intake_mid, tooltip = "text")
    
int_plot_caloric_intake_mid_display

# Plot 3(b) - Middle group kcal Trends
focus_mid <- focus_df %>% filter(Category == "Mid")
ggplot(focus_mid, aes(x=Year, y=Total.Calories)) +
  geom_point() +
    geom_smooth(method = "lm", se = FALSE, color = color_mid)
    labs(title = "Change in Caloric Intake for Middle 10 Countries(2010-2021)",
       x = "Year",
       y = "Average Daily Intake (kcal)")

# Plot 3(c) - Bottom group kcal Trends
focus_bot <- focus_df %>% filter(Category == "Bot")
ggplot(focus_bot, aes(x=Year, y=Total.Calories)) +
  geom_point() +
    geom_smooth(method = "lm", se = FALSE, color = color_bot)
    labs(title = "Change in Caloric Intake for Bottom 10 Countries(2010-2021)",
       x = "Year",
       y = "Average Daily Intake (kcal)")
    
# Plot 4 - All together
ggplot(focus_df, aes(x = Year, y = Total.Calories, color = Category, group = Country)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, aes(group = Category)) +  # Add regression lines for each Category
  labs(title = "Trends in Caloric Intake (2010-2021)",
       x = "Year",
       y = "Average Daily Intake (kcal)") +
  scale_color_manual(values = c("Top" = color_top, "Mid" = color_mid, "Bot" = color_bot)) +  # Optional: set custom colors
  theme_minimal()

################################################################################
# Storyline Visuals
################################################################################

# Plot 1 - Life expectancy per country in each group
ggplot(mean_life_exp, aes(x = reorder(Country, Mean_Life_Expectancy), y = Mean_Life_Expectancy, fill = Category)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Countries by Mean Life Expectancy (2021)",
       x = "Country",
       y = "Mean Life Expectancy (years)") +
  scale_fill_manual(values = c("Top" = color_top, "Middle" = color_mid, "Bottom" = color_bot)) +
  theme_minimal() +
  theme(legend.position = "top")

# Plot 2 - Average Nutritional intake by group
avg_nutritional_profile <- macro_df %>%
  group_by(Category, Element) %>%
  summarize(Average_Calories = mean(Calories, na.rm = TRUE)) %>%
  ungroup()
# Stacked bar plot showing the average intake of macronutrients by group
ggplot(avg_nutritional_profile, aes(x = Category, y = Average_Calories, fill = Element)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Average Nutritional Intake by Group (2010-2021)",
       x = "Group",
       y = "Average Daily Intake (kcal)") +
  scale_fill_manual(values = c("Protein supply quantity (g/capita/day)" = color_protein, 
                               "Fat supply quantity (g/capita/day)" = color_fats, 
                               "Food supply (kcal/capita/day)" = color_cals),
                    labels = c("Protein supply quantity (g/capita/day)" = "Protein",
                               "Fat supply quantity (g/capita/day)" = "Fat",
                               "Food supply (kcal/capita/day)" = "Carbohydrates and Others"),
                               name = "Calories from:") +
  theme_minimal()

# Plot 3 - Nutritional Profile Comparison
ggplot(avg_macro_by_group, aes(x = Year, y = Average_Calories, color = Element, group = Element)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  facet_wrap(~ Category) +
  labs(title = "Nutritional Profile Comparison (2010-2021)",
       x = "Year",
       y = "Average Daily Intake (kcal)") +
  scale_color_manual(values = c("Protein supply quantity (g/capita/day)" = color_protein, 
                                "Fat supply quantity (g/capita/day)" = color_fats, 
                                "Food supply (kcal/capita/day)" = color_cals)) +
  theme_minimal() +
  theme(legend.position = "top")

# Plot 4 - How do different macro habits affect longevity?
facet_labels <- c(
  "Protein supply quantity (g/capita/day)" = "Protein Intake",
  "Fat supply quantity (g/capita/day)" = "Fat Intake",
  "Food supply (kcal/capita/day)" = "Total Caloric Intake"
)

ggplot(macro_df, aes(x = Calories, y = Life.Expectancy, color = Category)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Relationship Between Macronutrient Intake and Life Expectancy",
       x = "Calories (kcal/capita/day)",
       y = "Life Expectancy (years)") +
  scale_color_manual(values = c("Top" = color_top, 
                                "Middle" = color_mid, 
                                "Bottom" = color_bot),
                     labels = c("Top" = "Top 10",
                                "Middle" = "Middle 10",
                                "Bottom" = "Bottom 10"),
                     name = "Group") +  # Change the legend title
  facet_wrap(~ Element, labeller = labeller(Element = facet_labels)) +
  theme_minimal()

# Plot 5 - What is the Average Intake of Proteins, Fats, and Carbohydrates Across Different Countries?
# Simplify labels for macronutrients
element_labels <- c(
  "Protein supply quantity (g/capita/day)" = "Protein",
  "Fat supply quantity (g/capita/day)" = "Fat",
  "Food supply (kcal/capita/day)" = "Total Calories"
)
# Stacked bar plot

avg_nutrient_by_country <- macro_df %>%
  group_by(Country, Element) %>%
  summarize(Average_Intake = mean(Calories, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Highlight = ifelse(Country %in% focus_df$Country[focus_df$Category == "Top"], "Top", "Other"))

# Step 2: Create a stacked bar plot with conditional coloring
ggplot(avg_nutrient_by_country, aes(x = reorder(Country, -Average_Intake), y = Average_Intake, fill = Element)) +
  geom_bar(stat = "identity", aes(alpha = Highlight)) +
  scale_alpha_manual(values = c("Top" = 1, "Other" = 0.3), guide = "none") +  # Highlight Top countries
  labs(title = "Average Macronutrient Intake by Country",
       x = "Country",
       y = "Average Intake (kcal)") +
  scale_fill_manual(values = c("Protein supply quantity (g/capita/day)" = color_protein, 
                               "Fat supply quantity (g/capita/day)" = color_fats, 
                               "Food supply (kcal/capita/day)" = color_cals),
                    labels = element_labels,
                    name = "Macronutrient") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Plot 6 - Is the Nutritional Profile the Same for the Top 10 / Middle 10 / Bottom 10?
ggplot(macro_df, aes(x = Category, y = Calories, fill = Element)) +
  geom_boxplot() +
  labs(title = "Macronutrient Intake by Group",
       x = "Group",
       y = "Calories (kcal/capita/day)") +
  scale_fill_manual(values = c("Protein supply quantity (g/capita/day)" = color_protein, 
                               "Fat supply quantity (g/capita/day)" = color_fats, 
                               "Food supply (kcal/capita/day)" = color_cals)) +
  theme_minimal()

# Plot 7 - Comparison of Their Nutritional Profile with the Suggested Macronutrient Profile


# Plot 8 - Does the Nutrition of the Top 10 Countries Align with Suggestions by the Government/WHO?
# Radar chart comparing actual vs suggested intake (for Top 10 group)

# Example radar chart structure
#top_10_avg <- comparison_df %>% filter(Category == "Top")
#radar_data <- rbind(rep(max(top_10_avg$Average_Calories), 3), rep(0, 3), top_10_avg$Average_Calories)
#radar_data <- as.data.frame(radar_data)
#colnames(radar_data) <- c("Protein", "Fat", "Carbohydrates")

#radarchart(radar_data, axistype = 1,
#           pcol = c("blue"), pfcol = c("lightblue"), plwd = 2,
#           cglcol = "grey", cglty = 1, axislabcol = "grey", caxislabels = seq(0, max(top_10_avg$Average_Calories), by = 100),
#           cglwd = 0.8,
#           vlcex = 0.8,
#           title = "Top 10 Countries: Actual vs Suggested Nutritional Profile")


# Plot 9 -  Are There Notable Regional or Cultural Differences in Macronutrient Consumption?

# Plot 10 - How Do Different Types of Macronutrients (e.g., Saturated vs. Unsaturated Fats) Correlate with Life Expectancy?
#### Needs fix
ggplot(macro_df %>% filter(Element %in% c("Saturated fat supply quantity (g/capita/day)", "Unsaturated fat supply quantity (g/capita/day)")),
       aes(x = Calories, y = Life.Expectancy, color = Element)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Correlation Between Fat Types and Life Expectancy",
       x = "Intake (kcal/capita/day)",
       y = "Life Expectancy (years)") +
  theme_minimal()



################################################################################
# Linear Model
################################################################################
# New df for model
model_df <- df %>%
  select(Country, Year, Life.Expectancy)

# Outlier Cleaning
Q1 <- quantile(model_df$Life.Expectancy, 0.25, na.rm = TRUE)
Q3 <- quantile(model_df$Life.Expectancy, 0.75, na.rm = TRUE)
IQR_value <- Q3 - Q1

low_bound <- Q1 - 1.5 * IQR_value
hi_bound <- Q1 + 1.5 * IQR_value

model_df_clean <- model_df %>%
  filter(Life.Expectancy >= low_bound & Life.Expectancy <= hi_bound)
head(model_df_clean)

model <- lm(model_df_clean$Life.Expectancy ~ model_df_clean$Year)

summary(model)

ggplot(model_df_clean, aes(x = Year, y = Life.Expectancy)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "blue", linetype = "dashed") +
  labs(title = "Global Trend in Life Expectancy Over Time",
       x = "Year",
       y = "Life Expectancy (Years)") +
  theme_minimal()


