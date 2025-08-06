library(tidyverse)
library(fastDummies)

bike_sharing_df <- read_csv("raw_seoul_bike_sharing.csv", show_col_types = FALSE)
summary(bike_sharing_df)
dim(bike_sharing_df)


df_con_na_if_any <- bike_sharing_df |>
  filter(if_any(everything(), is.na))

dim(df_con_na_if_any)

bike_sharing_df <- bike_sharing_df |>
  drop_na(RENTED_BIKE_COUNT)


AVG_TMP_BY_HOUR <- bike_sharing_df |>
  filter(SEASONS == "Summer") |>
  group_by(HOUR) |>
  summarise(
    TMP_AVG = mean(TEMPERATURE, na.rm = TRUE)
  )

bike_sharing_df <- bike_sharing_df |>
  left_join(AVG_TMP_BY_HOUR, by = "HOUR") |>
  mutate(
    TEMPERATURE = if_else(
      is.na(TEMPERATURE) & SEASONS == "Summer",
      TMP_AVG,
      TEMPERATURE
    )
  ) |>
  select(-TMP_AVG)

print(summary(bike_sharing_df))

write.csv(bike_sharing_df,"seoul_bike_sharing.csv", row.name = TRUE)


bike_sharing_df <- bike_sharing_df |>
  mutate(HOUR = as.character(HOUR))

bike_sharing_df <- bike_sharing_df %>%
  dummy_cols(
    select_columns = c("HOUR", "SEASONS", "HOLIDAY", "FUNCTIONING_DAY"), # Aquí especificamos todas las columnas
    remove_first_dummy = FALSE, # Mantenemos todas las columnas dummy (0 y 1)
    remove_selected_columns = FALSE, # Mantenemos las columnas originales también
  )
write.csv(bike_sharing_df, "seoul_bike_sharing_converted.csv", row.names = FALSE)


normalization <- function(x){
  x_new = (x - min(x)) / (max(x) - min(x))
  return(x_new)
}


bike_sharing_df <- bike_sharing_df |>
  mutate(
    across(
      .cols = c(RENTED_BIKE_COUNT, TEMPERATURE, HUMIDITY, WIND_SPEED,
                VISIBILITY, DEW_POINT_TEMPERATURE, SOLAR_RADIATION,
                RAINFALL, SNOWFALL),
      .fns = normalization
    )
  )

print(summary(bike_sharing_df))
write.csv(bike_sharing_df, "seoul_bike_sharing_converted_normalized.csv", row.names = FALSE)

# Dataset list
dataset_list <- c('seoul_bike_sharing.csv', 'seoul_bike_sharing_converted.csv', 'seoul_bike_sharing_converted_normalized.csv')

for (dataset_name in dataset_list){
  # Read dataset
  dataset <- read_csv(dataset_name)
  # Standardized its columns:
  # Convert all columns names to uppercase
  names(dataset) <- toupper(names(dataset))
  # Replace any white space separators by underscore, using str_replace_all function
  names(dataset) <- str_replace_all(names(dataset), " ", "_")
  # Save the dataset back
  write.csv(dataset, dataset_name, row.names=FALSE)
}






