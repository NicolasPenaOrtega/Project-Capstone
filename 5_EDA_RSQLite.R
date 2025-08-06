library(tidyverse)
library(RSQLite)

conn <- dbConnect(SQLite(), dbname = "weather_data.db")

dataset_list <- c(
  "seoul_bike_sharing.csv", "cities_weather_forecast.csv",
  "bike_sharing_systems.csv", "world_cities.csv"
)

for (dataset in dataset_list) {
  df <- read_csv(paste0("Data/", dataset), show_col_types = FALSE)
  dbWriteTable(conn, unlist(strsplit(dataset, "\\."))[1], df, overwrite = TRUE)
  rm(df)
}

dbListTables(conn)

query <- "PRAGMA table_info(bike_sharing_systems)"
tableColNames_BSS <- data.frame(dbGetQuery(conn, query)) |>
  select(name, type) |>
  slice(1:15)

query <- "PRAGMA table_info(world_cities)"
tableColNames_WC <- data.frame(dbGetQuery(conn, query)) |>
  select(name, type) |>
  slice(1:15)

colnames(tableColNames) <- c("Column Names")

# Determine how many records are in the seoul_bike_sharing dataset
query <- "SELECT COUNT(*) FROM seoul_bike_sharing"
dbGetQuery(conn, query)

# Determine how many hours had non-zero rented bike count
query <- "SELECT SUM(HOUR) as TOTAL_HOURS FROM seoul_bike_sharing
          WHERE RENTED_BIKE_COUNT != 0"
dbGetQuery(conn, query)

# Query the the weather forecast for Seoul over the next 3 hours.
query <- "SELECT WEATHER FROM cities_weather_forecast LIMIT 1"
dbGetQuery(conn, query)

# Find which seasons are included in the seoul bike sharing dataset.
query <- "SELECT DISTINCT(SEASONS) FROM seoul_bike_sharing"
dbGetQuery(conn, query)

# Find the first and last dates in the Seoul Bike Sharing dataset
query <- "SELECT MIN(DATE) AS FIRST_DATE, MAX(DATE) AS LAST_DATE FROM seoul_bike_sharing"
dbGetQuery(conn, query)

# determine which date and hour had the most bike rentals.
query <- "SELECT DATE, HOUR, MAX(RENTED_BIKE_COUNT) AS MAX_RENTED_BIKES
          FROM 'seoul_bike_sharing'
          WHERE RENTED_BIKE_COUNT == (SELECT MAX(RENTED_BIKE_COUNT) FROM 'seoul_bike_sharing')"
dbGetQuery(conn, query)


# Determine the average hourly temperature and the average number of bike rentals per hour over each season. List the top ten results by average bike count.
query <- "SELECT SEASONS, HOUR, AVG(TEMPERATURE) AS AVG_TEMPERATURE,
            AVG(RENTED_BIKE_COUNT) AS AVG_RENTED_BIKES
            FROM seoul_bike_sharing
            GROUP BY SEASONS, HOUR
            ORDER BY AVG_RENTED_BIKES DESC
            LIMIT 10"
dbGetQuery(conn, query)


# Find the average hourly bike count during each season.
# Also include the minimum, maximum, and standard deviation of the hourly bike count for each season.

query <- "SELECT SEASONS, HOUR,
          AVG(RENTED_BIKE_COUNT) AS AVG_RENTED_BIKES,
          AVG(TEMPERATURE) AS AVG_TEMPERATURE,
          MIN(RENTED_BIKE_COUNT) AS MIN_RENTED_BIKES,
          MAX(RENTED_BIKE_COUNT) AS MAX_RENTED_BIKES,
          SQRT(AVG(RENTED_BIKE_COUNT*RENTED_BIKE_COUNT) - AVG(RENTED_BIKE_COUNT)*AVG(RENTED_BIKE_COUNT)) AS STDDEV_RENTED_BIKES
          FROM seoul_bike_sharing
          GROUP BY SEASONS, HOUR
          LIMIT 10"
dbGetQuery(conn, query)

# Consider the weather over each season. On average, what were the
# TEMPERATURE, HUMIDITY, WIND_SPEED, VISIBILITY,
# DEW_POINT_TEMPERATURE, SOLAR_RADIATION, RAINFALL, and
# SNOWFALL per season?
# Include the average bike count as well , and rank the results by
# average bike count so you can see if it is correlated with the
# weather at all.

query <- "SELECT SEASONS,
          AVG(TEMPERATURE) AS AVG_TEMPERATURE,
          AVG(HUMIDITY) AS AVG_HUMIDITY,
          AVG(WIND_SPEED) AS AVG_WIND_SPEED,
          AVG(VISIBILITY) AS AVG_VISIBILITY,
          AVG(DEW_POINT_TEMPERATURE) AS AVG_DEW_POINT_TEMPERATURE,
          AVG(SOLAR_RADIATION) AS AVG_SOLAR_RADIATION,
          AVG(RAINFALL) AS AVG_RAINFALL,
          AVG(SNOWFALL) AS AVG_SNOWFALL,
          AVG(RENTED_BIKE_COUNT) AS AVG_RENTED_BIKES
          FROM seoul_bike_sharing
          GROUP BY SEASONS
          ORDER BY AVG_RENTED_BIKES DESC"
dbGetQuery(conn, query)

# Use an implicit join across the WORLD_CITIES and the
# BIKE_SHARING_SYSTEMS tables to determine the total number of
# bikes avaialble in Seoul, plus the following city information about
# Seoul: CITY, COUNTRY, LAT, LON, POPULATION, in a single view.
# Notice that in this case, the CITY column will work for the
# WORLD_CITIES table, but in general you would have to use the
# CITY_ASCII column.

query <- "SELECT wc.COUNTRY, wc.CITY_ASCII, wc.LAT, wc.LNG, wc.POPULATION, bss.BICYCLES
          FROM bike_sharing_systems bss
          JOIN world_cities wc ON bss.CITY == wc.CITY_ASCII
          WHERE wc.CITY_ASCII == 'Seoul'"

dbGetQuery(conn, query)

# Find all cities with total bike counts between 15000 and 20000. Return the city and country names,
#  plus the coordinates (LAT, LNG), population, and number of bicycles for each city.¶

query <- "SELECT wc.COUNTRY, wc.CITY_ASCII, wc.LAT, wc.LNG, wc.POPULATION, bss.BICYCLES
          FROM bike_sharing_systems bss
          JOIN world_cities wc ON bss.CITY == wc.CITY_ASCII
          WHERE bss.BICYCLES BETWEEN 15000 AND 20000"

dbGetQuery(conn, query)

dbDisconnect(conn)
