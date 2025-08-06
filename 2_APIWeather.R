library(rvest)
library(httr)

current_weather_url <- 'https://api.openweathermap.org/data/2.5/weather'
API <- "a4e86aff1815cc4ad015989ac5604ecc"
current_query <- list(q = "Seoul", appid = API, units="metric")
response <- GET(current_weather_url, query=current_query)
http_type(response)

json_result <- content(response, as="parsed")
json_result

weather <- c()
visibility <- c()
temp <- c()
temp_min <- c()
temp_max <- c()
pressure <- c()
humidity <- c()
wind_speed <- c()
wind_deg <- c()

# $weather is also a list with one element, its $main element indicates the weather status such as clear or rain
weather <- c(weather, json_result$weather[[1]]$main)
# Get Visibility
visibility <- c(visibility, json_result$visibility)
# Get current temperature 
temp <- c(temp, json_result$main$temp)
# Get min temperature 
temp_min <- c(temp_min, json_result$main$temp_min)
# Get max temperature 
temp_max <- c(temp_max, json_result$main$temp_max)
# Get pressure
pressure <- c(pressure, json_result$main$pressure)
# Get humidity
humidity <- c(humidity, json_result$main$humidity)
# Get wind speed
wind_speed <- c(wind_speed, json_result$wind$speed)
# Get wind direction
wind_deg <- c(wind_deg, json_result$wind$deg)

# Combine all vectors
weather_data_frame <- data.frame(weather=weather, 
                                 visibility=visibility, 
                                 temp=temp, 
                                 temp_min=temp_min, 
                                 temp_max=temp_max, 
                                 pressure=pressure, 
                                 humidity=humidity, 
                                 wind_speed=wind_speed, 
                                 wind_deg=wind_deg)

weather_data_frame

#Forcasting

# Get forecast data for a given city list
get_weather_forecaset_by_cities <- function(city_names){
  # Create some empty vectors to hold data temporarily
  city_list <- list()
  weather_list <- list()
  visibility_list <- list()
  temp_list <- list()
  temp_min_list <- list()
  temp_max_list <- list()
  pressure_list <- list()
  humidity_list <- list()
  wind_speed_list <- list()
  wind_deg_list <- list()
  forecast_datetime_list <- list()
  season_list <- list()
  
  for (city_name in city_names){
    # Forecast API URL
    forecast_url <- 'https://api.openweathermap.org/data/2.5/forecast'
    # Create query parameters
    forecast_query <- list(q = city_name, appid = "a4e86aff1815cc4ad015989ac5604ecc", units="metric")
    # Make HTTP GET call for the given city
    response <- GET(forecast_url, query=forecast_query)
    json_result <- content(response, as="parsed")
    # Note that the 5-day forecast JSON result is a list of lists. You can print the reponse to check the results
    #results <- json_list$list
    results <- json_result$list
    # Loop the json result
    for(result in results) {
      # Get city name
      city_list <- c(city_list, city_name)
      # Get weather status
      weather_list <- c(weather_list, result$weather[[1]]$main)
      # Get visibility
      visibility_list <- c(visibility_list, result$visibility)
      # Get current temperature
      temp_list <- c(temp_list, result$main$temp)
      # Get current min temperature
      temp_min_list <- c(temp_min_list, result$main$temp_min)
      # Get current max temperature
      temp_max_list <- c(temp_max_list, result$main$temp_max)
      # Get current pressure
      pressure_list <- c(pressure_list, result$main$pressure)
      # Get current humidity
      humidity_list <- c(humidity_list, result$main$humidity)
      # Get current wind speed
      wind_speed_list <- c(wind_speed_list, result$wind$speed)
      # Get current wind direction
      wind_deg_list <- c(wind_deg_list, result$wind$deg)
      # Get forecast timestamp
      datetime_obj <- as.POSIXct(result$dt_txt, tz="UTC")
      forecast_datetime_list <- c(forecast_datetime_list, datetime_obj)
      # Get season
      month <- as.numeric(format(datetime_obj, "%m"))
      if (month %in% c(3, 4, 5)) {
        season_list <- c(season_list, "Spring")
      } else if (month %in% c(6, 7, 8)) {
        season_list <- c(season_list, "Summer")
      } else if (month %in% c(9, 10, 11)) {
        season_list <- c(season_list, "Autumn")
      } else {
        season_list <- c(season_list, "Winter")
      }
    }
    
    # Add the R Lists into a data frame df
    df <- data.frame(city = unlist(city_list),
                     weather = unlist(weather_list),
                     visibility = unlist(visibility_list),
                     temp = unlist(temp_list),
                     temp_min = unlist(temp_min_list),
                     temp_max = unlist(temp_max_list),
                     pressure = unlist(pressure_list),
                     humidity = unlist(humidity_list),
                     wind_speed = unlist(wind_speed_list),
                     wind_deg = unlist(wind_deg_list),
                     # Aquí se asegura que forecast_datetime_list se convierta en un vector POSIXct
                     forecast_datetime = as.POSIXct(unlist(forecast_datetime_list), origin="1970-01-01", tz="UTC"),
                     season = unlist(season_list))
  }
  # Return a data frame
  return(df)
}


cities <- c("Seoul", "Washington, D.C.", "Paris", "Suzhou")
cities_weather_df <- get_weather_forecaset_by_cities(cities)

# Write cities_weather_df to `cities_weather_forecast.csv`
write.csv(cities_weather_df, "cities_weather_forecast.csv", row.names = FALSE)


# Download several datasets

# Download some general city information such as name and locations
url <- "https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-RP0321EN-SkillsNetwork/labs/datasets/raw_worldcities.csv"
# download the file
download.file(url, destfile = "raw_worldcities.csv")

# Download a specific hourly Seoul bike sharing demand dataset
url <- "https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-RP0321EN-SkillsNetwork/labs/datasets/raw_seoul_bike_sharing.csv"
# download the file
download.file(url, destfile = "raw_seoul_bike_sharing.csv")
