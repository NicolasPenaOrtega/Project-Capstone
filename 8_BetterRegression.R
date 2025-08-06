# Load necessary libraries
library(tidymodels)
library(tidyverse)
library(stringr)

# load the dataset
dataset_url <- "https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-RP0321EN-SkillsNetwork/labs/datasets/seoul_bike_sharing_converted_normalized.csv"
bike_sharing_df <- read_csv(dataset_url)
readr::spec(bike_sharing_df)

# Delete unnecessary columns
bike_sharing_df <- bike_sharing_df |>
  select(-DATE, -FUNCTIONING_DAY)

lm_spec <- linear_reg() |>
  set_engine("lm") |>
  set_mode("regression")

# Split the data into training and testing sets
set.seed(1234)
data_split <- initial_split(bike_sharing_df, prop = 4/5)
train_data <- training(data_split)
test_data <- testing(data_split)


#is it linear the correlation between RENTED_BIKE_COUNT and TEMPERATURE?

ggplot(data=train_data, aes(RENTED_BIKE_COUNT, TEMPERATURE)) + 
  geom_point() + 
  geom_smooth(method = "lm", formula = y ~ x, color="red") + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), color="yellow") + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 4), color="green") + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 6), color="blue")

#it's not linear, so we will use polynomial regression

poly_recipe <- recipe(
  RENTED_BIKE_COUNT ~ RAINFALL + HUMIDITY + TEMPERATURE + `18` +DEW_POINT_TEMPERATURE + `19`+ `8`, data = train_data) |>
  step_poly(RAINFALL, degree = 6, options = list(raw = FALSE)) |>
  step_poly(HUMIDITY, degree = 4, options = list(raw = FALSE)) |>
  step_poly(TEMPERATURE, DEW_POINT_TEMPERATURE, degree = 2, options = list(raw = FALSE))

# create workflow and add recipe and model
poly_workflow <- workflow() |>
  add_recipe(poly_recipe) |>
  add_model(lm_spec)

# Fit the model to the train data
poly_fit <- poly_workflow |>
  fit(data = train_data)

# show the summary of the fitted model
poly_fit |>
  extract_fit_parsnip() |>
  summary()

# show the coefficients of the fitted model
poly_fit |>
  extract_fit_parsnip() |>
  tidy() |>
  select(term, estimate) |>
  arrange(desc(abs(estimate)))

# Make predictions on the training data
train_predictions <- poly_fit |>
  predict(new_data = train_data) |>
  bind_cols(train_data)

train_predictions <- train_predictions |>
  mutate(.pred = if_else(.pred < 0, 0, .pred))

# calculate the RMSE for the training data
rmse_train <- train_predictions |>
  rmse(truth = RENTED_BIKE_COUNT, estimate = .pred)

# calculate the RSQ(R^2) for the training data
rsq_train <- train_predictions |>
  rsq(truth = RENTED_BIKE_COUNT, estimate = .pred)

# Make prediction for the testing data
test_predictions <- poly_fit |>
  predict(new_data = test_data) |>
  bind_cols(test_data)

#drop negative values
test_predictions<- test_predictions |>
  mutate(.pred = if_else(.pred < 0, 0, .pred))

# Calculate RSME for the test data
rmse_test <- test_predictions |>
  rmse(truth = RENTED_BIKE_COUNT, estimate = .pred)

# Calculate RSQ(R^2) for the test data
rsq_test <- test_predictions |>
  rsq(truth = RENTED_BIKE_COUNT, estimate = .pred)


# Create interaction terms for the recipe

poly_recipe_interaction_terms <- recipe(
  RENTED_BIKE_COUNT ~ RAINFALL + HUMIDITY + TEMPERATURE + `18` +DEW_POINT_TEMPERATURE + `19`+ `8`, data = train_data) |>
  step_interact(terms = ~ RAINFALL:TEMPERATURE + RAINFALL:HUMIDITY + TEMPERATURE:HUMIDITY +
                          DEW_POINT_TEMPERATURE:TEMPERATURE) |>
  step_poly(RAINFALL, degree = 6, options = list(raw = FALSE)) |>
  step_poly(HUMIDITY, degree = 4, options = list(raw = FALSE)) |>
  step_poly(TEMPERATURE, DEW_POINT_TEMPERATURE, degree = 2, options = list(raw = FALSE))

# create workflow and add recipe and model
poly_workflow_interaction_terms <- workflow() |>
  add_recipe(poly_recipe_interaction_terms) |>
  add_model(lm_spec)

# Fit the model to the train data
poly_fit_interation_terms <- poly_workflow_interaction_terms |>
  fit(data = train_data)

poly_fit_interation_terms |>
  extract_fit_parsnip() |>
  summary()

poly_fit_interation_terms |>
  extract_fit_parsnip() |>
  tidy() |>
  select(term, estimate) |>
  arrange(desc(abs(estimate)))

# evaluate model
# TRAIN DATA
train_predictions_interaction_terms <- poly_fit_interation_terms |>
  predict(new_data = train_data) |>
  bind_cols(train_data)

#drop negative values
train_predictions_interaction_terms<- train_predictions_interaction_terms |>
  mutate(.pred = if_else(.pred < 0, 0, .pred))

# Calculate RSME for the test data
rsme_train_interaction_terms <- train_predictions_interaction_terms |>
  rmse(truth = RENTED_BIKE_COUNT, estimate = .pred)

# Calculate RSQ(R^2) for the test data
rsq_train_interaction_terms <- train_predictions_interaction_terms |>
  rsq(truth = RENTED_BIKE_COUNT, estimate = .pred)

# TEST DATA
test_predictions_interaction_terms <- poly_fit_interation_terms |>
  predict(new_data = test_data) |>
  bind_cols(test_data)

#drop negative values
test_predictions_interaction_terms<- test_predictions_interaction_terms |>
  mutate(.pred = if_else(.pred < 0, 0, .pred))

# Calculate RSME for the test data
rsme_test_interaction_terms <- test_predictions_interaction_terms |>
  rmse(truth = RENTED_BIKE_COUNT, estimate = .pred)

# Calculate RSQ(R^2) for the test data
rsq_test_interaction_terms <- test_predictions_interaction_terms |>
  rsq(truth = RENTED_BIKE_COUNT, estimate = .pred)

# Correlacion entre variables
matrix_cor <- cor(train_data, method = "pearson")

corrplot::corrplot(matrix_cor,
         method = "color", # Tipo de visualización: "circle", "square", "ellipse", "number", "shade", "color", "pie"
         type = "full",    # "full", "upper", "lower" (para mostrar toda la matriz, solo la parte superior o inferior)
         #addCoef.col = "black", # Añadir los coeficientes de correlación en negro
         tl.col = "black",    # Color de las etiquetas de texto (variables)
         tl.srt = 45,         # Ángulo de rotación de las etiquetas del texto
         tl.cex = 0.5,
         diag = FALSE,        # No mostrar la correlación de una variable consigo misma (siempre 1)
         title = "Mapa de Calor de la Correlación de Pearson", # Título del gráfico
         mar = c(0,0,1,0)     # Ajustar márgenes para el título
)



# add regularization to the model
lasso_spec <- 
  linear_reg(penalty = tune(), mixture = 1) |>
  set_engine("glmnet")
# Create a recipe for lasso regression
lasso_recipe <- recipe(RENTED_BIKE_COUNT ~ ., data = train_data) |>
  step_interact(terms = ~ TEMPERATURE:SUMMER + TEMPERATURE:WINTER + HUMIDITY:DEW_POINT_TEMPERATURE) |>
  step_poly(RAINFALL, HUMIDITY, degree = 3, options = list(raw = FALSE)) |>
  step_poly(TEMPERATURE, DEW_POINT_TEMPERATURE, degree = 2, options = list(raw = FALSE))

# Create a workflow for lasso regression
lasso_workflow <- 
  workflow() |>
  add_recipe(lasso_recipe) |>
  add_model(lasso_spec)

# perform cross-validation to find the best penalty
lambda_grid <- grid_regular(levels = 300, penalty(range = c(-7,0.5)))
lasso_cv_folds <- vfold_cv(train_data, v = 10)

# Tune the lasso model using cross-validation
lasso_tune_results <- 
  tune_grid(
  lasso_workflow,
  resamples = lasso_cv_folds,
  grid = lambda_grid,
  metrics = metric_set(rmse, rsq, mae),
  control = control_grid(save_pred = TRUE)
)

# Show and select the best results
show_best(lasso_tune_results, metric = "rsq")
best_lasso_penalty <- select_best(lasso_tune_results, metric = "rsq")

final_lasso_workflow <- 
  lasso_workflow |>
  finalize_workflow(best_lasso_penalty)

final_lasso_fit <-
  final_lasso_workflow |>
  fit(data = train_data)

df_coefficients <- final_lasso_fit |>
  extract_fit_parsnip() |>
  tidy() |>
  select(term, estimate) |>
  drop_na(estimate) |>
  arrange(desc(abs(estimate)))
  
df_coefficients |>
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

# Evaluate the performance with the train data
lasso_train_predictions <-
  final_lasso_fit |>
  predict(new_data = train_data) |>
  bind_cols(train_data)

lasso_train_predictions <- lasso_train_predictions |>
  mutate(.pred = if_else(.pred < 0, 0, .pred)) # Ensure no negative predictions

lasso_rmse_train_data <- lasso_train_predictions |>
  rmse(truth = RENTED_BIKE_COUNT, estimate = .pred)

lasso_rsq_train_data <- lasso_train_predictions |>
  rsq(truth = RENTED_BIKE_COUNT, estimate = .pred)

# Evaluate performance with test data

lasso_test_predictions <-
  final_lasso_fit |>
  predict(new_data = test_data) |>
  bind_cols(test_data)

lasso_test_predictions <- lasso_test_predictions |>
  mutate(.pred = if_else(.pred < 0, 0, .pred)) # Ensure no negative predictions

lasso_rmse_test_data <- lasso_test_predictions |>
  rmse(truth = RENTED_BIKE_COUNT, estimate = .pred)

lasso_rsq_test_data <- lasso_test_predictions |>
  rsq(truth = RENTED_BIKE_COUNT, estimate = .pred)

# save metric results in a dataframe

df_metrics <- tibble(
  MODEL = rep(c("MODEL1", "MODEL2", "MODEL3"), each = 2),
  EVALUATION = rep(c("TRAINING", "TESTING"), times = 3),
  RSQ = c(
    rsq_train$.estimate,
    rsq_test$.estimate,
    rsq_train_interaction_terms$.estimate,
    rsq_test_interaction_terms$.estimate,
    lasso_rsq_train_data$.estimate,
    lasso_rsq_test_data$.estimate
  ),
  RMSE = c(
    rmse_train$.estimate,
    rmse_test$.estimate,
    rsme_train_interaction_terms$.estimate,
    rsme_test_interaction_terms$.estimate,
    lasso_rmse_train_data$.estimate,
    lasso_rmse_test_data$.estimate
  )
) |>
  pivot_longer(
    cols = c(RSQ, RMSE),
    names_to = "METRIC",
    values_to = "VALUE"
  )


df_metrics |>
  ggplot(aes(x = MODEL, y = VALUE)) +
  geom_bar(stat = "identity", aes(fill = EVALUATION), position = "dodge") +
  facet_wrap(~ METRIC, scales = "free_y") +
  labs(
    title = "Model Performance Metrics",
    x = "Model",
    y = "Value",
    fill = "Evaluation"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
  )


ggplot(lasso_train_predictions) +
  stat_qq(aes(sample = RENTED_BIKE_COUNT), color = "green") +
  stat_qq(aes(sample = .pred), color = "brown1") +
  labs(
    title = "Q-Q Plot of Rented Bike Count vs Predictions for train data",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
  )

ggplot(lasso_test_predictions) +
  stat_qq(aes(sample = RENTED_BIKE_COUNT), color = "green") +
  stat_qq(aes(sample = .pred), color = "brown1") +
  labs(
    title = "Q-Q Plot of Rented Bike Count vs Predictions for test data",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
  )

lasso_rmse_train_data; lasso_rsq_train_data; lasso_rmse_test_data; lasso_rsq_test_data


# 1. Calcular los residuos para los datos de entrenamiento y prueba
lasso_train_predictions <- lasso_train_predictions |>
  mutate(residual = RENTED_BIKE_COUNT - .pred)

lasso_test_predictions <- lasso_test_predictions |>
  mutate(residual = RENTED_BIKE_COUNT - .pred)


# 2. Visualización de Residuos vs. Valores Predichos (Datos de Entrenamiento)
ggplot(lasso_train_predictions, aes(x = .pred, y = residual)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Residuos vs. Predicciones (Datos de Entrenamiento)",
    x = "Predicciones del Modelo",
    y = "Residuos"
  ) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))

# 3. Visualización de Residuos vs. Valores Predichos (Datos de Prueba)
ggplot(lasso_test_predictions, aes(x = .pred, y = residual)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Residuos vs. Predicciones (Datos de Prueba)",
    x = "Predicciones del Modelo",
    y = "Residuos"
  ) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))

# 4. Visualización de Residuos vs. Variables Predictoras Clave (Ejemplo con Temperatura)
# Puedes repetir este gráfico para otras variables predictoras que consideres importantes
ggplot(lasso_test_predictions, aes(x = TEMPERATURE, y = residual)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Residuos vs. Temperatura (Datos de Prueba)",
    x = "Temperatura",
    y = "Residuos"
  ) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))

# 5. Histograma de Residuos (para verificar normalidad de los errores)
ggplot(lasso_test_predictions, aes(x = residual)) +
  geom_histogram(binwidth = 50, fill = "skyblue", color = "black") +
  labs(
    title = "Histograma de Residuos (Datos de Prueba)",
    x = "Residuos",
    y = "Frecuencia"
  ) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))

# Opcional: Gráfico de densidad de Residuos
ggplot(lasso_test_predictions, aes(x = residual)) +
  geom_density(fill = "skyblue", alpha = 0.7) +
  labs(
    title = "Densidad de Residuos (Datos de Prueba)",
    x = "Residuos",
    y = "Densidad"
  ) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))

lasso_rmse_train_data; lasso_rsq_train_data; lasso_rmse_test_data; lasso_rsq_test_data


# Save the final model
#saveRDS(final_lasso_fit, "final_lasso_model.rds")
# Save the metrics dataframe
#write.csv(df_metrics, "model_performance_metrics.csv", row.names = FALSE)








