library(tidyverse)
library(ggthemes)
library(plotly)

# load data and ensure you read DATE as type character.
seoul_bike_sharing <- read_csv("Data/seoul_bike_sharing.csv", show_col_types = FALSE)
class(seoul_bike_sharing$DATE)

# Recast DATE as a date. Use the format of the data, namely "%d/%m/%Y".
seoul_bike_sharing <- seoul_bike_sharing |>
  mutate(DATE = as.Date(DATE, format = "%d/%m/%Y"))
class(seoul_bike_sharing$DATE)

# Cast HOURS as a categorical variable
seoul_bike_sharing <- seoul_bike_sharing |>
  mutate(HOUR = as.factor(HOUR))
class(seoul_bike_sharing$HOUR)
str(seoul_bike_sharing)

# Finally, ensure there are no missing values in the dataset
sum(is.na(seoul_bike_sharing))


# Dataset Summary
summary(seoul_bike_sharing)

# How many Holidays there are?
count_holiday <- seoul_bike_sharing |>
  select(HOLIDAY) |>
  filter(HOLIDAY == "Holiday") |>
  tally() |>
  rename(COUNT_OF_HOLIDAY = n)

# the percentage of records that fall on a holiday
(count_holiday$COUNT_OF_HOLIDAY / dim(seoul_bike_sharing)[1]) * 100

# Given there is exactly a full year of data, determine how many records we expect to have
expected_records <- 365 * 24

# Given the observations for the 'FUNCTIONING_DAY' how many records must there be
count_functioning_day <- seoul_bike_sharing |>
  select(FUNCTIONING_DAY) |>
  filter(FUNCTIONING_DAY == "Yes") |>
  tally() |>
  rename(COUNT_OF_FUNCTIONING_DAY = n)

# group the data by SEASONS, and use the summarize() function
# to calculate the seasonal total rainfall and snowfall
total_rainfall_snowfall <- seoul_bike_sharing |>
  group_by(SEASONS) |>
  summarize(
    TOTAL_RAINFALL = sum(RAINFALL, rm.na = TRUE),
    TOTAL_SNOWFALL = sum(SNOWFALL, rm.na = TRUE)
  )


ggplot(seoul_bike_sharing, aes(x = DATE, y = RENTED_BIKE_COUNT)) +
  geom_point(aes(color = as.factor(SEASONS)), size = 0.5, alpha = 0.5) +
  labs(color = "Season") +
  theme_bw()


plot <- ggplot(seoul_bike_sharing, aes(x = DATE, y = RENTED_BIKE_COUNT)) +
  geom_point(aes(color = as.factor(HOUR)), size = 0.5, alpha = 0.5) +
  scale_color_viridis_d(option = "plasma") +
  labs(color = "Hour") +
  guides(color = guide_legend(override.aes = list(size = 2))) +
  theme_bw()

ggplotly(plot)

# Create a histogram overlaid with a kernel density curve
ggplot(seoul_bike_sharing, aes(x = RENTED_BIKE_COUNT)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "dodgerblue3", color = "black", alpha = 0.7) +
  geom_density(color = "red", size = 1) +
  labs(
    title = "Histogram of Rented Bike Count with Density Curve",
    x = "Rented Bike Count",
    y = "Density"
  ) +
  theme_bw()

# Use a scatter plot to visualize the correlation between
# RENTED_BIKE_COUNT and TEMPERATURE by SEASONS

plot <- ggplot(seoul_bike_sharing, aes(x = TEMPERATURE, y = RENTED_BIKE_COUNT)) +
  geom_point(aes(color = as.factor(HOUR)), size = 0.5, alpha = 0.5) +
  guides(color = guide_legend(override.aes = list(size = 2))) +
  facet_wrap(~SEASONS) +
  labs(color = "Hour") +
  theme_bw()

ggplotly(plot)

# boxplots of RENTED_BIKE_COUNT vs. HOUR grouped by SEASONS
ggplot(seoul_bike_sharing, aes(x = HOUR, y = RENTED_BIKE_COUNT)) +
  geom_boxplot(aes(fill = I("dodgerblue3"), color = I("black"))) +
  facet_wrap(~SEASONS) +
  theme_bw()


# Group the data by DATE, and use the summarize() function to calculate the daily total rainfall and snowfall.
total_daily_rainfall_snowfall <- seoul_bike_sharing |>
  group_by(DATE) |>
  summarize(
    TOTAL_RAINFALL = sum(RAINFALL, na.rm = TRUE),
    TOTAL_SNOWFALL = sum(SNOWFALL, na.rm = TRUE)
  )

total_day_with_snowfall <- total_daily_rainfall_snowfall |>
  filter(TOTAL_SNOWFALL > 0) |>
  tally() |>
  rename(COUNT_OF_DAYS_WITH_SNOWFALL = n)
