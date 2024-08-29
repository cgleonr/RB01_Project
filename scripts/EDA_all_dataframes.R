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
install.packages(c("ggplot2", "dplyr", "sf", "leaflet"))
library(ggplot2)
library(dplyr)
library(plotly) # for interactive plots
library(gganimate) # animates ggplots
library(maptiles) # for maps
library(sf) # for maps
library(tidyterra) # for maps
library(leaflet) # for hovering over the map


df <- read.csv("clean_datasets/final_df.csv")
head(df)
dim(df)

# Filter only 2019 values
df_2021 <- df %>% filter(Year == 2021)

# Remove duplicates and sort by Life.Expectancy
df_2021_unique <- df_2021 %>% 
  distinct(Country, .keep_all = TRUE) %>% 
  arrange(desc(Life.Expectancy))
df_2021_unique
# Calculate the middle 10 indices
mid_start <- round(length(df_2021_unique[,1])/2 - 5)
mid_end <- round(length(df_2021_unique[,1])/2 + 5)

# Select top 10, middle 10, and bottom 10 countries
top_10 <- df_2021_unique %>% slice(1:10) %>% mutate(Category = "Top")
mid_10 <- df_2021_unique %>% slice(mid_start:mid_end) %>% mutate(Category = "Mid") %>% slice(-9)
#mid_10 <- mid_10 %>% slice(-9) %>% mutate(Category = "Mid") # grabs 11, but #9 has a char error
bot_10 <- df_2021_unique %>% slice((n() - 9):n()) %>% mutate(Category = "Bot")

# Combine them into one dataframe
subset_df <- bind_rows(top_10, mid_10, bot_10)

# Filter the original dataframe for the selected countries and years
df_filtered <- df %>%
  filter(Country %in% subset_df$Country, Year >= min(df$Year), Year <= max(df$Year)) %>%
  left_join(subset_df %>% select(Country, Category), by = "Country")

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
head(life_exp)
# total_caloric_intake - supplementary df, contains focus countries, year, cal intake
head(total_caloric_intake)
# merged_cal_longevity_df - merged df, life_exp and total_caloric_intake
head(merged_cal_longevity_df)
# unique_cal_long_df - like merged_cal_longevity_df, but one entry per year
head(unique_cal_long_df)
# difference_kcal_LE - yearly difference in cal intake and longevity (2019 - 2015)
head(difference_kcal_LE)
# caloric_sources_2015 - all caloric sources from focus group, {year} (kcal)
head(caloric_sources_{year})
# macro_sources_2015 - protein & fat sources from focus group, {year} (grams)
head(macro_sources_{year})
################################################################################
# Load in DF
df <- read.csv("clean_datasets/final_df.csv")
# drop indices (column "X")
df <- df[ , !names(df) %in% c("X")]

# Filter only 2019 values
df_2021 <- df %>% filter(Year == 2019)
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
focus_df <- bind_rows(top_10, mid_10, bot_10) # Holds top, mid bot 10

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
macro_sources_2015 <- focus_df %>%
  filter(Element == c("Protein supply quantity (g/capita/day)",
                      "Fat supply quantity (g/capita/day)"))


################################################################################
# Visuals
################################################################################

# Plot 1 - Change in Life Expectancy from 2015-2019, focus countries
ggplot(focus_df, aes(x = Year, y = Life.Expectancy, color = Category, group = Country)) +
  geom_line(size = 1) +
  geom_point() +
  labs(title = "Life Expectancy (2010-2021)", 
       x = "Year", 
       y = "Life Expectancy (years)") +
  theme_minimal() +
  scale_color_manual(values = c("Top" = "blue", "Mid" = "green", "Bot" = "red"))

# Plot 1.1 - Change in Life Expectancy from 2015 - 2019, focus countries - interactive
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
    geom_smooth(method = "lm", se = FALSE, color = "blue")
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
    geom_smooth(method = "lm", se = FALSE, color = "green")
    labs(title = "Change in Caloric Intake for Middle 10 Countries(2010-2021)",
       x = "Year",
       y = "Average Daily Intake (kcal)")

# Plot 3(c) - Bottom group kcal Trends
focus_bot <- focus_df %>% filter(Category == "Bot")
ggplot(focus_bot, aes(x=Year, y=Total.Calories)) +
  geom_point() +
    geom_smooth(method = "lm", se = FALSE, color = "red")
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
  scale_color_manual(values = c("Top" = "blue", "Mid" = "green", "Bot" = "red")) +  # Optional: set custom colors
  theme_minimal()

# Plot 4.1 - All together Interactive, colours adjusted
int_plot_caloric_intake <- ggplot(focus_df, aes(x = Year, y = Total.Calories, color = Category, group = Country, text = Country)) +
  geom_point(size = 1) +
  geom_smooth(method = "lm", se = FALSE, aes(group = Category), size = 1.5) +  # Thicker regression lines for each Category
  labs(title = "Trends in Caloric Intake (2010-2021)",
       x = "Year",
       y = "Average Daily Intake (kcal)") +
  scale_color_manual(values = c("Top" = "#009E73", "Mid" = "#56B4E9", "Bot" = "#D55E00")) +  # Apply specified HEX colors
  theme_minimal()

int_plot_caloric_intake_display <- ggplotly(int_plot_caloric_intake, tooltip = "text")

int_plot_caloric_intake_display

# Plot 5 - Stacked Bars for total cals from proteins, fats, other (carbs), 2015 vs 2019, groups

# Plot 6 - Stacked bars for grams proteins, fats

# Plot 7 - Stacked Bars for plant-based vs animal sources

# Plot 9 - Map with the countries top, mid, bot. colour coded for group. interactive with average caloric daily intake
  ## creating a simple feature object
sf_data <- st_as_sf(df, coords=c("Longitude", "Latitude"), crs = 4326)
  
## plotting the base map
    #Create leaflet map
map <- leaflet() %>%
  addTiles() %>%
  
    #Top countries Layer
  addCircleMarkers(data = top_10,
                   ~Longitude, ~Latitude,
                   color = '#009E73',
                   label = ~paste("Country:", Country, "<br>",
                                  "Life Expectancy:", top_10$Life.Expectancy, "years", "<br>",
                                  "Caloric Intake:", top_10$Value, "kcal"),
                   group = "Top 10") %>%
    #Mid countries layer
  addCircleMarkers(data = mid_10, 
                   ~Longitude, ~Latitude, 
                   color = '#56B4E9', 
                   label = ~paste("Country:", Country),
                   group = "Mid 10") %>%
    #Bot Countries Layer
  addCircleMarkers(data = bot_10, 
                   ~Longitude, ~Latitude, 
                   color = '#D55E00', 
                   label = ~paste("Country:", Country),
                   group = "Bottom 10") %>%
  
    #layers control
addLayersControl(
    overlayGroups = c("Top 10", "Middle 10", "Bottom 10"),
    options = layersControlOptions(collapsed = FALSE)
  )

map


