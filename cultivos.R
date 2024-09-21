library(readr)

# Import Dataset
yield_df <- read_csv("data/yield_df.csv")

#View Dataset
View(yield_df)

# Display the first few rows to understand the structure
head(yield_df)

# Display the structure of the dataset
str(yield_df)

# Summary statistics for a quick overview
summary(yield_df)

# MODIFY STRUCTURE
# Convert 'Area' and 'Item' columns to factors
yield_df$Area <- as.factor(yield_df$Area)
yield_df$Item <- as.factor(yield_df$Item)
# See changes
summary(yield_df)

# Load libraries
library(dplyr)
library(ggplot2)

# Summary statistics for numerical columns
summary_stats <- yield_df %>%
  summarise(
    avg_yield = mean(yield_df$`hg/ha_yield`, na.rm = TRUE),
    median_yield = median(yield_df$`hg/ha_yield`, na.rm = TRUE),
    sd_yield = sd(yield_df$`hg/ha_yield`, na.rm = TRUE),
    avg_rainfall = mean(average_rain_fall_mm_per_year, na.rm = TRUE),
    avg_pesticides = mean(pesticides_tonnes, na.rm = TRUE),
    avg_temp = mean(avg_temp, na.rm = TRUE)
  )

print(summary_stats)

# Average yield by Area (Top 10 Areas)
area_yield <- yield_df %>%
  group_by(Area) %>%
  summarise(avg_yield = mean(yield_df$`hg/ha_yield`, na.rm = TRUE)) %>%
  arrange(desc(avg_yield)) %>%
  head(25)

print(area_yield)

# Average yield by Item (Top 10 Items)
item_yield <- yield_df %>%
  group_by(Item) %>%
  summarise(avg_yield = mean(yield_df$`hg/ha_yield`, na.rm = TRUE)) %>%
  arrange(desc(avg_yield)) %>%
  head(10)

print(item_yield)

# Distribution of crop yield
ggplot(yield_df, aes(x = yield_df$`hg/ha_yield`)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Crop Yield (hg/ha)", x = "Yield (hg/ha)", y = "Frequency") +
  theme_minimal()

# Scatter plot of Yield vs Rainfall
ggplot(yield_df, aes(x = average_rain_fall_mm_per_year, y = yield_df$`hg/ha_yield`)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = T, color = "red") +
  labs(title = "Crop Yield vs Average Rainfall", x = "Average Rainfall (mm/year)", y = "Yield (hg/ha)") +
  theme_minimal()

# Scatter plot of Yield vs Temperature
ggplot(yield_df, aes(x = avg_temp, y = yield_df$`hg/ha_yield`)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Crop Yield vs Average Temperature", x = "Average Temperature (°C)", y = "Yield (hg/ha)") +
  theme_minimal()

# Scatter plot of Yield vs Pesticides
ggplot(yield_df, aes(x = pesticides_tonnes, y = yield_df$`hg/ha_yield`)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Crop Yield vs Pesticides Usage", x = "Pesticides (Tonnes)", y = "Yield (hg/ha)") +
  theme_minimal()

# Build the linear regression model
linear_model <- lm(yield_df$`hg/ha_yield` ~ average_rain_fall_mm_per_year + avg_temp + pesticides_tonnes, data = yield_df)

# Display the model summary
summary(linear_model)

# Include 'Item' as a factor in the model
linear_model_with_item <- lm(yield_df$`hg/ha_yield` ~ average_rain_fall_mm_per_year + avg_temp + pesticides_tonnes + Item, data = yield_df)

# Display the summary of the updated model
summary(linear_model_with_item)

