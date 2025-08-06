library(tidyverse)
library(tidymodels)
library(stringr)

# load data
dataset_url <- "https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-RP0321EN-SkillsNetwork/labs/datasets/seoul_bike_sharing_converted_normalized.csv"
bike_sharing_df <- read_csv(dataset_url)
readr::spec(bike_sharing_df)
# remove unnecessary columns
bike_sharing_df <- bike_sharing_df |>
  select(-DATE, -FUNCTIONING_DAY)

# Split data
set.seed(1234)
bike_split <- initial_split(bike_sharing_df, prop = 3 / 4)
bike_train <- training(bike_split)
bike_test <- testing(bike_split)

# define linear regression
lm_model <- linear_reg() |>
  set_engine("lm") |>
  set_mode("regression")

# define recipe
lm_recipe_weather <- recipe(
  RENTED_BIKE_COUNT ~ TEMPERATURE + HUMIDITY + WIND_SPEED + VISIBILITY + DEW_POINT_TEMPERATURE +
    SOLAR_RADIATION + RAINFALL + SNOWFALL,
  data = bike_train
)

# define workflow
lm_model_weather_workflow <- workflow() |>
  add_model(lm_model) |>
  add_recipe(lm_recipe_weather)

# fit the model
lm_model_weather_fit <- lm_model_weather_workflow |>
  fit(data = bike_train)

lm_model_weather_fit$fit

# `RENTED_BIKE_COUNT ~ .` means use all other variables except for the response variable
lm_recipe_all_variable <- recipe(RENTED_BIKE_COUNT ~ ., data = bike_train)
# define workflow
lm_model_all_variables_workflow <- workflow() |>
  add_model(lm_model) |>
  add_recipe(lm_recipe_all_variable)
# fit the model
lm_model_all_variables_fit <- lm_model_all_variables_workflow |>
  fit(data = bike_train)

lm_model_all_variables_fit$fit


# test_results_weather for lm_model_weather model
lm_model_weather_predictions <- predict(lm_model_weather_fit, new_data = bike_test) |>
  bind_cols(bike_test)
# test_results_all for lm_model_all
lm_model_all_variable_predictions <- predict(lm_model_all_variables_fit, new_data = bike_test) |>
  bind_cols(bike_test)

# rsq_weather <- rsq(...)
rsq_weather <- lm_model_weather_predictions |>
  rsq(truth = RENTED_BIKE_COUNT, estimate = .pred)
# rsq_all <- rsq(...)
rsq_all_variable <- lm_model_all_variable_predictions |>
  rsq(truth = RENTED_BIKE_COUNT, estimate = .pred)

# rmse_weather <- rmse(...)
rmse_weather <- lm_model_weather_predictions |>
  rmse(truth = RENTED_BIKE_COUNT, estimate = .pred)
# rmse_all <- rmse(...)
rmse_all_variable <- lm_model_all_variable_predictions |>
  rmse(truth = RENTED_BIKE_COUNT, estimate = .pred)

# visualization coefficents
lm_model_all_variables_fit |>
  extract_fit_parsnip() |>
  tidy() |>
  select(term, estimate) |>
  drop_na(estimate) |>
  ggplot(aes(y = reorder(term, abs(estimate)), x = abs(estimate))) +
  geom_bar(stat = "identity") +
  labs(
    title = "Coefficients of linear regression model",
    x = "Absolute value of coefficients",
    y = "Variables"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
  )
