library(tidyverse)
library(leaflet)
source("model_prediction.R")


test_weather_data_generation<-function(){
  city_weather_bike_df<-generate_city_weather_bike_data()
  stopifnot(length(city_weather_bike_df)>0)
  #print(city_weather_bike_df)
  return(city_weather_bike_df)
}

# Define a city list
# Test generate_city_weather_bike_data() function
city_weather_bike_df <- test_weather_data_generation()
list_city <- unique(city_weather_bike_df$CITY_ASCII)

# Define color factor
color_levels <- colorFactor(palette = c("green", "yellow", "red"), 
                            levels = c("small", "medium", "large"))

# Create another data frame called `cities_max_bike` with each row contains city location info and max bike
# prediction for the city
cities_max_bike <- city_weather_bike_df |>
  group_by(CITY_ASCII) |>
  filter(BIKE_PREDICTION == max(BIKE_PREDICTION, na.rm = TRUE)) |>
  slice_head(n = 1) |>
  summarise(
    CITIES_MAX_BIKE = first(BIKE_PREDICTION), # Ahora 'first' toma el valor máximo que ya hemos filtrado
    LABEL = first(LABEL),
    DETAILED_LABEL = first(DETAILED_LABEL),
    LAT = first(LAT),
    LNG = first(LNG),
    BIKE_PREDICTION_LEVEL = case_when(
      CITIES_MAX_BIKE <= 1000 ~ "small",
      CITIES_MAX_BIKE <= 3000 ~ "medium",
      TRUE ~ "large"
    ),
    custom_radius = case_when(
      BIKE_PREDICTION_LEVEL == "small" ~ 6,
      BIKE_PREDICTION_LEVEL == "medium" ~ 10,
      BIKE_PREDICTION_LEVEL == "large" ~ 12,
      TRUE ~ 5
    )
  )

color_levels <- colorFactor(
  palette = c("blue", "yellow", "red"), # Por ejemplo: "small" = azul, "medium" = verde, "large" = rojo
  domain = cities_max_bike$BIKE_PREDICTION_LEVEL,
  levels = c("small", "medium", "large") # Asegúrate de que estos coincidan exactamente con tus datos
)

min_date <- min(city_weather_bike_df$FORECASTDATETIME, na.rm = TRUE)
max_date <- max(city_weather_bike_df$FORECASTDATETIME, na.rm = TRUE)
