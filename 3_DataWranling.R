library(tidyverse)

# Download raw_bike_sharing_systems.csv
url <- "https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-RP0321EN-SkillsNetwork/labs/datasets/raw_bike_sharing_systems.csv"
download.file(url, destfile = "raw_bike_sharing_systems.csv")

# Download raw_cities_weather_forecast.csv
url <- "https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-RP0321EN-SkillsNetwork/labs/datasets/raw_cities_weather_forecast.csv"
download.file(url, destfile = "raw_cities_weather_forecast.csv")

# Download raw_worldcities.csv
url <- "https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-RP0321EN-SkillsNetwork/labs/datasets/raw_worldcities.csv"
download.file(url, destfile = "raw_worldcities.csv")

# Download raw_seoul_bike_sharing.csv
url <- "https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-RP0321EN-SkillsNetwork/labs/datasets/raw_seoul_bike_sharing.csv"
download.file(url, destfile = "raw_seoul_bike_sharing.csv")

# List of datasets to be used in the project
dataset_list <- c("raw_bike_sharing_systems.csv", "raw_seoul_bike_sharing.csv", "raw_cities_weather_forecast.csv", "raw_worldcities.csv")

for (dataset_name in dataset_list) {
  # Read dataset
  dataset <- read_csv(dataset_name, show_col_types = FALSE)
  # Standardized its columns:
  # Convert all column names to uppercase
  # Replace any white space separators by underscores, using the str_replace_all function
  colnames(dataset) <- toupper(str_replace_all(colnames(dataset), " ", "_"))
  # Save the dataset
  write.csv(dataset, dataset_name, row.names = FALSE)
}

for (dataset_name in dataset_list) {
  dataset <- read_csv(dataset_name, show_col_types = FALSE)
  print(dataset_name)
  print(summary(dataset))
}


# Analisamos los dataset
bike_sharing_df <- read_csv("raw_bike_sharing_systems.csv", show_col_types = FALSE)
head(bike_sharing_df)

sub_bike_sharing_df <- bike_sharing_df |>
  select(COUNTRY, CITY, SYSTEM, BICYCLES)

sub_bike_sharing_df |>
  summarize_all(class) |>
  gather(variable, class)

find_character <- function(strings) grepl("[^0-9]", strings)

sub_bike_sharing_df |>
  select(BICYCLES) |>
  filter(find_character(BICYCLES)) |>
  slice(0:10)

# Define a 'reference link' character class,
# `[A-z0-9]` means at least one character
# `\\[` and `\\]` means the character is wrapped by [], such as for [12] or [abc]
ref_pattern <- "\\[[A-z0-9]+\\]"
find_reference_pattern <- function(strings) grepl(ref_pattern, strings)


# Check whether the COUNTRY column has any reference links
sub_bike_sharing_df |>
  select(COUNTRY) |>
  filter(find_reference_pattern(COUNTRY)) |>
  slice(0:10)

# Check whether the CITY column has any reference links
sub_bike_sharing_df |>
  select(CITY) |>
  filter(find_reference_pattern(CITY)) |>
  slice(0:10)

# Check whether the SYSTEM column has any reference links
sub_bike_sharing_df |>
  select(SYSTEM) |>
  filter(find_reference_pattern(SYSTEM)) |>
  slice(0:10)

# remove reference link
remove_ref <- function(strings) {
  ref_pattern <- "\\[^0-9\\]"
  result <- str_replace_all(strings, ref_pattern, "")
  # Replace all matched substrings with a white space using str_replace_all()
  # Trim the reslt if you want
  return(result)
}

# Remove reference links from the CITY column
# sub_bike_sharing_df %>% mutate(column1=remove_ref(column1), ... )
result <- sub_bike_sharing_df |>
  mutate(CITY = remove_ref(CITY))


result <- sub_bike_sharing_df |>
  select(CITY, SYSTEM, BICYCLES) |>
  filter(find_reference_pattern(CITY) | find_reference_pattern(SYSTEM) | find_reference_pattern(BICYCLES))

# Extract the first number
extract_num <- function(columns) {
  # Define a digital pattern
  digitals_pattern <- "^\\d+"
  # Find the first match using str_extract
  result <- str_extract(columns, digitals_pattern)
  number <- as.numeric(result)
  return(number)
  # Convert the result to numeric using the as.numeric() function
}

# Use the mutate() function on the BICYCLES column
result <- sub_bike_sharing_df %>%
  mutate(BICYCLES = extract_num(BICYCLES))


summary(result$BICYCLES)

write.csv(result, "bike_sharing_systems.csv", row.names = FALSE)
