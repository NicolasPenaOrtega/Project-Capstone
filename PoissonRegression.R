# Load necessary libraries
library(tidymodels)
library(tidyverse)
library(stringr)
library(poissonreg) # Necesaria para el motor 'glmnet' con regresión de Poisson

# load the dataset
dataset_url <- "https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-RP0321EN-SkillsNetwork/labs/datasets/seoul_bike_sharing_converted_normalized.csv"
bike_sharing_df <- read_csv(dataset_url)
readr::spec(bike_sharing_df)

# Delete unnecessary columns (justifications maintained)
bike_sharing_df <- bike_sharing_df |>
  select(-DATE, -FUNCTIONING_DAY)

# Split the data into training and testing sets
set.seed(1234)
data_split <- initial_split(bike_sharing_df, prop = 4/5)
train_data <- training(data_split)
test_data <- testing(data_split)

# --- CAMBIO CLAVE 1: Especificación del Modelo de Poisson con Regularización ---
# Usamos `poisson_reg()` en lugar de `linear_reg()`
# El motor "glmnet" es compatible y permite la regularización (penalty y mixture)
poisson_spec <-
  poisson_reg(penalty = tune(), mixture = 1) |> # mixture = 1 para Lasso
  set_engine("glmnet")

# Create a recipe for Poisson regression
# Mantenemos las interacciones y términos polinomiales que ya definiste
poisson_recipe <- recipe(RENTED_BIKE_COUNT ~ ., data = train_data) |>
  step_interact(terms = ~ TEMPERATURE:SUMMER + TEMPERATURE:WINTER + HUMIDITY:DEW_POINT_TEMPERATURE) |>
  step_poly(RAINFALL, HUMIDITY, degree = 3, options = list(raw = FALSE)) |>
  step_poly(TEMPERATURE, DEW_POINT_TEMPERATURE, degree = 2, options = list(raw = FALSE)) |>
  step_dummy(all_nominal_predictors()) # Asegúrate de que las variables categóricas se conviertan en dummies si no lo están ya

# Create a workflow for Poisson regression
poisson_workflow <-
  workflow() |>
  add_recipe(poisson_recipe) |>
  add_model(poisson_spec)

# perform cross-validation to find the best penalty
lambda_grid <- grid_regular(levels = 300, penalty(range = c(-7, 0.5))) # Rango de penalización
poisson_cv_folds <- vfold_cv(train_data, v = 10) # Sin estratificación, como solicitaste

# Tune the Poisson model using cross-validation
poisson_tune_results <-
  tune_grid(
    poisson_workflow,
    resamples = poisson_cv_folds,
    grid = lambda_grid,
    metrics = metric_set(rmse, rsq, mae),
    control = control_grid(save_pred = TRUE)
  )

# Show and select the best results
show_best(poisson_tune_results, metric = "rsq")
best_poisson_penalty <- select_best(poisson_tune_results, metric = "rsq")

final_poisson_workflow <-
  poisson_workflow |>
  finalize_workflow(best_poisson_penalty)

final_poisson_fit <-
  final_poisson_workflow |>
  fit(data = train_data)

# Visualización de Coeficientes (para entender la importancia de las variables)
# Nota: Los coeficientes de Poisson son en la escala logarítmica de la media.
final_poisson_fit |>
  extract_fit_parsnip() |>
  tidy() |>
  select(term, estimate) |>
  drop_na(estimate) |>
  ggplot(aes(y = reorder(term, abs(estimate)), x = abs(estimate))) +
  geom_bar(stat = "identity") +
  labs(
    title = "Coeficientes del Modelo de Regresión de Poisson",
    x = "Valor Absoluto de los Coeficientes",
    y = "Variables"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
  )

# --- Evaluación del rendimiento con los datos de entrenamiento y prueba ---

# Las predicciones de Poisson son por defecto positivas, no necesitas if_else(.pred < 0, 0, .pred)
# Pero es una buena práctica asegurar que no haya ceros si la variable objetivo puede ser 0
# y si glmnet produce ceros (aunque con poisson_reg es menos probable).
# Sin embargo, dado que la cuenta de bicicletas no puede ser negativa, el clipping a 0 es seguro.

poisson_train_predictions <-
  final_poisson_fit |>
  predict(new_data = train_data) |>
  bind_cols(train_data) |>
  mutate(.pred = if_else(.pred < 0, 0, .pred)) # Clipping a 0 por seguridad

poisson_rmse_train_data <- poisson_train_predictions |>
  rmse(truth = RENTED_BIKE_COUNT, estimate = .pred)

poisson_rsq_train_data <- poisson_train_predictions |>
  rsq(truth = RENTED_BIKE_COUNT, estimate = .pred)

poisson_test_predictions <-
  final_poisson_fit |>
  predict(new_data = test_data) |>
  bind_cols(test_data) |>
  mutate(.pred = if_else(.pred < 0, 0, .pred)) # Clipping a 0 por seguridad

poisson_rmse_test_data <- poisson_test_predictions |>
  rmse(truth = RENTED_BIKE_COUNT, estimate = .pred)

poisson_rsq_test_data <- poisson_test_predictions |>
  rsq(truth = RENTED_BIKE_COUNT, estimate = .pred)

# Mostrar métricas finales
poisson_rmse_train_data
poisson_rsq_train_data
poisson_rmse_test_data
poisson_rsq_test_data

# --- Visualizaciones adicionales (residuos y QQ plots) ---

# Calcular los residuos para los datos de entrenamiento y prueba
poisson_train_predictions <- poisson_train_predictions |>
  mutate(residual = RENTED_BIKE_COUNT - .pred)

poisson_test_predictions <- poisson_test_predictions |>
  mutate(residual = RENTED_BIKE_COUNT - .pred)

# 1. Visualización de Residuos vs. Valores Predichos (Datos de Entrenamiento)
ggplot(poisson_train_predictions, aes(x = .pred, y = residual)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Residuos vs. Predicciones (Poisson - Entrenamiento)",
    x = "Predicciones del Modelo",
    y = "Residuos"
  ) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))

# 2. Visualización de Residuos vs. Valores Predichos (Datos de Prueba)
ggplot(poisson_test_predictions, aes(x = .pred, y = residual)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Residuos vs. Predicciones (Poisson - Prueba)",
    x = "Predicciones del Modelo",
    y = "Residuos"
  ) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))

# 3. Histograma de Residuos (para verificar la distribución de los errores)
ggplot(poisson_test_predictions, aes(x = residual)) +
  geom_histogram(binwidth = 50, fill = "skyblue", color = "black") +
  labs(
    title = "Histograma de Residuos (Poisson - Prueba)",
    x = "Residuos",
    y = "Frecuencia"
  ) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))

# 4. Q-Q Plot de RENTED_BIKE_COUNT vs Predicciones (Train)
ggplot(poisson_train_predictions) +
  stat_qq(aes(sample = RENTED_BIKE_COUNT), color = "green") +
  stat_qq(aes(sample = .pred), color = "brown1") +
  labs(
    title = "Q-Q Plot de Rented Bike Count vs Predicciones (Poisson - Train)",
    x = "Cuantiles Teóricos",
    y = "Cuantiles de Muestra"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
  )

# 5. Q-Q Plot de RENTED_BIKE_COUNT vs Predicciones (Test)
ggplot(poisson_test_predictions) +
  stat_qq(aes(sample = RENTED_BIKE_COUNT), color = "green") +
  stat_qq(aes(sample = .pred), color = "brown1") +
  labs(
    title = "Q-Q Plot de Rented Bike Count vs Predicciones (Poisson - Test)",
    x = "Cuantiles Teóricos",
    y = "Cuantiles de Muestra"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
  )

# --- Actualizar el df_metrics con los resultados del modelo de Poisson ---
# Asegúrate de que los objetos rsq_train, rsq_test, etc., de modelos anteriores
# estén disponibles o renombra las filas para evitar errores.
# Para este ejemplo, solo estoy añadiendo las métricas de Poisson.

df_metrics_poisson <- tibble(
  MODEL = "Poisson_Lasso",
  EVALUATION = c("TRAINING", "TESTING"),
  RSQ = c(poisson_rsq_train_data$.estimate, poisson_rsq_test_data$.estimate),
  RMSE = c(poisson_rmse_train_data$.estimate, poisson_rmse_test_data$.estimate)
) |>
  pivot_longer(
    cols = c(RSQ, RMSE),
    names_to = "METRIC",
    values_to = "VALUE"
  )

# Si tenías un df_metrics anterior y quieres combinarlos
# df_metrics_combined <- bind_rows(df_metrics_poisson, df_metrics_old_models)
# y luego graficar df_metrics_combined

# Gráfico de métricas actualizado para incluir el modelo de Poisson (si solo quieres este, si no, combina)
ggplot(df_metrics_poisson, aes(x = MODEL, y = VALUE)) +
  geom_bar(stat = "identity", aes(fill = EVALUATION), position = "dodge") +
  facet_wrap(~ METRIC, scales = "free_y") +
  labs(
    title = "Métricas de Rendimiento del Modelo de Regresión de Poisson",
    x = "Modelo",
    y = "Valor",
    fill = "Evaluación"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
  )