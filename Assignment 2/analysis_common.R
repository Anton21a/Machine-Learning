library(caret)  
library(estimatr) 
library(Metrics)  
library(skimr)
library(glmnet)
library(dplyr)
rm(list=ls())

setwd("C:/Users/Пользователь/Desktop/MA1y/Prediction_with_ML/Machine-Learning/Assignment 2")

data2025 <- read.csv("Prague2025q1_final.csv")
data2024 <- read.csv("Prague2024q4_final.csv")

#_____________________________________PRAGUE____________________________________
#_____________________________________2024______________________________________
#_____________________________________OLS+LASSO_________________________________

# -------------------------------
# 1. One-time train/test split
# -------------------------------
set.seed(111)
split_index <- createDataPartition(data2024$ln_price, p = 0.7, list = FALSE)
train_data <- data2024[split_index, ]
test_data <- data2024[-split_index, ]

# -------------------------------
# 2. OLS models
# -------------------------------
ols_formulas <- list(
  reg1 = ln_price ~ property_type,
  reg2 = ln_price ~ property_type + accommodates + number_of_reviews_n + f_bathroom + f_minimum_nights,
  reg3 = ln_price ~ property_type + accommodates + number_of_reviews_n + f_bathroom + f_minimum_nights + refrigerator + microwave + wifi
)

ols_rmse <- numeric(3)
ols_predictions <- list()

for (j in 1:3) {
  model <- lm_robust(ols_formulas[[j]], data = train_data)
  preds <- predict(model, newdata = test_data)
  ols_rmse[j] <- RMSE(preds, test_data$ln_price)
  
  ols_predictions[[j]] <- data.frame(
    Model = paste0("OLS_Model_", j),
    Actual = test_data$ln_price,
    Predicted = preds,
    Residuals = test_data$ln_price - preds
  )
}

ols_rmse_df <- data.frame(
  Model = paste0("OLS_Model_", 1:3),
  RMSE = round(ols_rmse, 4)
)

# -------------------------------
# 3. LASSO model
# -------------------------------
lasso_formula <- as.formula(
  ln_price ~ accommodates + number_of_reviews_n + f_bathroom + f_minimum_nights +
    property_type + f_room_type + refrigerator + microwave + wifi +
    accommodates * property_type +
    f_room_type * property_type +
    property_type * refrigerator +
    property_type * microwave +
    property_type * wifi
)

# Prepare LASSO data
vars_needed <- all.vars(lasso_formula)
data_lasso <- na.omit(data2024[, c("ln_price", vars_needed)])
train_lasso <- data_lasso[split_index, ]
test_lasso <- data_lasso[-split_index, ]

X_train <- model.matrix(lasso_formula, data = train_lasso)[, -1]
y_train <- train_lasso$ln_price
X_test <- model.matrix(lasso_formula, data = test_lasso)[, -1]
y_test <- test_lasso$ln_price

lasso_cv <- cv.glmnet(X_train, y_train, alpha = 1, nfolds = 5)
best_lambda <- lasso_cv$lambda.min

lasso_preds <- predict(lasso_cv, newx = X_test, s = best_lambda)
lasso_rmse <- RMSE(lasso_preds, y_test)

lasso_prediction_df <- data.frame(
  Model = "LASSO",
  Actual = y_test,
  Predicted = as.numeric(lasso_preds),
  Residuals = y_test - as.numeric(lasso_preds)
)

lasso_rmse_df <- data.frame(
  Model = "LASSO",
  RMSE = round(lasso_rmse, 4)
)

# -------------------------------
# 4. Combine results
# -------------------------------
all_predictions <- bind_rows(ols_predictions, lasso_prediction_df)
rmse_all <- bind_rows(ols_rmse_df, lasso_rmse_df)
print(rmse_all)

# -------------------------------
# 5. Visualizations
# -------------------------------

# RMSE barplot
ggplot(rmse_all, aes(x = Model, y = RMSE, fill = Model)) +
  geom_col(width = 0.6) +
  labs(title = "RMSE Comparison: OLS vs LASSO", y = "RMSE", x = "Model") +
  theme_minimal() +
  theme(legend.position = "none")

# Residual distribution
ggplot(all_predictions, aes(x = Residuals, fill = Model)) +
  geom_density(alpha = 0.5) +
  labs(title = "Residual Distribution by Model", x = "Residual", y = "Density") +
  theme_minimal()

ggplot(all_predictions, aes(x = Model, y = Residuals, fill = Model)) +
  geom_boxplot(outlier.size = 1.5, alpha = 0.7) +
  labs(
    title = "Boxplot of Residuals: OLS vs LASSO",
    x = "Model",
    y = "Residuals (Actual - Predicted)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")


#_____________________________________PRAGUE____________________________________
#_____________________________________2025______________________________________
#_____________________________________OLS+LASSO_________________________________

# -------------------------------
# 1. One-time train/test split
# -------------------------------
set.seed(111)
split_index <- createDataPartition(data2025$ln_price, p = 0.7, list = FALSE)
train_data <- data2025[split_index, ]
test_data <- data2025[-split_index, ]

# -------------------------------
# 2. OLS models
# -------------------------------
ols_formulas <- list(
  reg1 = ln_price ~ property_type,
  reg2 = ln_price ~ property_type + accommodates + number_of_reviews_n + f_bathroom + f_minimum_nights,
  reg3 = ln_price ~ property_type + accommodates + number_of_reviews_n + f_bathroom + f_minimum_nights + refrigerator + microwave + wifi
)

ols_rmse <- numeric(3)
ols_predictions <- list()

for (j in 1:3) {
  model <- lm_robust(ols_formulas[[j]], data = train_data)
  preds <- predict(model, newdata = test_data)
  ols_rmse[j] <- RMSE(preds, test_data$ln_price)
  
  ols_predictions[[j]] <- data.frame(
    Model = paste0("OLS_Model_", j),
    Actual = test_data$ln_price,
    Predicted = preds,
    Residuals = test_data$ln_price - preds
  )
}

ols_rmse_df <- data.frame(
  Model = paste0("OLS_Model_", 1:3),
  RMSE = round(ols_rmse, 4)
)

# -------------------------------
# 3. LASSO model
# -------------------------------
lasso_formula <- as.formula(
  ln_price ~ accommodates + number_of_reviews_n + f_bathroom + f_minimum_nights +
    property_type + f_room_type + refrigerator + microwave + wifi +
    accommodates * property_type +
    f_room_type * property_type +
    property_type * refrigerator +
    property_type * microwave +
    property_type * wifi
)

# Prepare LASSO data
vars_needed <- all.vars(lasso_formula)
data_lasso <- na.omit(data2025[, c("ln_price", vars_needed)])
train_lasso <- data_lasso[split_index, ]
test_lasso <- data_lasso[-split_index, ]

X_train <- model.matrix(lasso_formula, data = train_lasso)[, -1]
y_train <- train_lasso$ln_price
X_test <- model.matrix(lasso_formula, data = test_lasso)[, -1]
y_test <- test_lasso$ln_price

lasso_cv <- cv.glmnet(X_train, y_train, alpha = 1, nfolds = 5)
best_lambda <- lasso_cv$lambda.min

lasso_preds <- predict(lasso_cv, newx = X_test, s = best_lambda)
lasso_rmse <- RMSE(lasso_preds, y_test)

lasso_prediction_df <- data.frame(
  Model = "LASSO",
  Actual = y_test,
  Predicted = as.numeric(lasso_preds),
  Residuals = y_test - as.numeric(lasso_preds)
)

lasso_rmse_df <- data.frame(
  Model = "LASSO",
  RMSE = round(lasso_rmse, 4)
)

# -------------------------------
# 4. Combine results
# -------------------------------
all_predictions <- bind_rows(ols_predictions, lasso_prediction_df)
rmse_all <- bind_rows(ols_rmse_df, lasso_rmse_df)
print(rmse_all)

# -------------------------------
# 5. Visualizations
# -------------------------------

# RMSE barplot
ggplot(rmse_all, aes(x = Model, y = RMSE, fill = Model)) +
  geom_col(width = 0.6) +
  labs(title = "RMSE Comparison: OLS vs LASSO", y = "RMSE", x = "Model") +
  theme_minimal() +
  theme(legend.position = "none")

# Residual distribution
ggplot(all_predictions, aes(x = Residuals, fill = Model)) +
  geom_density(alpha = 0.5) +
  labs(title = "Residual Distribution by Model", x = "Residual", y = "Density") +
  theme_minimal()

ggplot(all_predictions, aes(x = Model, y = Residuals, fill = Model)) +
  geom_boxplot(outlier.size = 1.5, alpha = 0.7) +
  labs(
    title = "Boxplot of Residuals: OLS vs LASSO",
    x = "Model",
    y = "Residuals (Actual - Predicted)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

#_____________________________________PRAGUE____________________________________
#_____________________________________2024______________________________________
#_____________________________________RandonForest______________________________

library(randomForest)
library(ranger)

# 1. Define formula 
# -----------------------------------------
rf_formula <- as.formula(
  ln_price ~ accommodates + number_of_reviews_n + f_bathroom + f_minimum_nights +
    property_type + f_room_type + refrigerator + microwave + wifi
)

# -----------------------------------------
# 2. Prepare data
# -----------------------------------------
set.seed(222)
split_index <- createDataPartition(data2024$ln_price, p = 0.7, list = FALSE)
train_rf <- data2024[split_index, ]
test_rf <- data2024[-split_index, ]

vars_rf <- all.vars(rf_formula)
train_rf <- na.omit(train_rf[, c("ln_price", vars_rf)])
test_rf <- na.omit(test_rf[, c("ln_price", vars_rf)])

# -----------------------------------
# 3. Manual tuning: try different mtry values
# -----------------------------------

mtry_values <- c(3, 5, 7, 9)
rf_results_list <- list()
rmse_values <- c()

for (m in mtry_values) {
  set.seed(100 + m)
  rf_model <- ranger(
    formula = rf_formula,
    data = train_rf,
    mtry = m,
    num.trees = 500,
    importance = "impurity"
  )
  
  preds <- predict(rf_model, data = test_rf)$predictions
  rmse <- RMSE(preds, test_rf$ln_price)
  rmse_values <- c(rmse_values, rmse)
  
  rf_results_list[[paste0("mtry_", m)]] <- list(model = rf_model, rmse = rmse)
  
  cat(paste0("mtry = ", m, " → RMSE: ", round(rmse, 4), "\n"))
}

# -----------------------------------
# 4. Pick the best model (lowest RMSE)
# -----------------------------------
best_idx <- which.min(rmse_values)
best_mtry <- mtry_values[best_idx]
best_rmse <- rmse_values[best_idx]
cat("\nBest mtry:", best_mtry, "| RMSE:", round(best_rmse, 4), "\n")

best_rf_model <- rf_results_list[[paste0("mtry_", best_mtry)]]$model

# -----------------------------------
# 5. Variable Importance Plot
# -----------------------------------
vip <- data.frame(
  Variable = names(best_rf_model$variable.importance),
  Importance = best_rf_model$variable.importance
)

vip <- vip %>% arrange(desc(Importance)) %>% head(10)

ggplot(vip, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_col(fill = "forestgreen") +
  coord_flip() +
  labs(title = paste("Top 10 Variable Importance (mtry =", best_mtry, ")"),
       x = "Variable", y = "Importance") +
  theme_minimal()



#_____________________________________PRAGUE____________________________________
#_____________________________________2025______________________________________
#_____________________________________RandonForest______________________________

# 1. Define formula
# -----------------------------------------
rf_formula <- as.formula(
  ln_price ~ accommodates + number_of_reviews_n + f_bathroom + f_minimum_nights +
    property_type + f_room_type + refrigerator + microwave + wifi
)

# -----------------------------------------
# 2. Prepare data
# -----------------------------------------
set.seed(222)
split_index <- createDataPartition(data2025$ln_price, p = 0.7, list = FALSE)
train_rf <- data2025[split_index, ]
test_rf <- data2025[-split_index, ]

vars_rf <- all.vars(rf_formula)
train_rf <- na.omit(train_rf[, c("ln_price", vars_rf)])
test_rf <- na.omit(test_rf[, c("ln_price", vars_rf)])

# -----------------------------------
# 3. Manual tuning: try different mtry values
# -----------------------------------

mtry_values <- c(3, 5, 7, 9)
rf_results_list <- list()
rmse_values <- c()

for (m in mtry_values) {
  set.seed(100 + m)
  rf_model <- ranger(
    formula = rf_formula,
    data = train_rf,
    mtry = m,
    num.trees = 500,
    importance = "impurity"
  )
  
  preds <- predict(rf_model, data = test_rf)$predictions
  rmse <- RMSE(preds, test_rf$ln_price)
  rmse_values <- c(rmse_values, rmse)
  
  rf_results_list[[paste0("mtry_", m)]] <- list(model = rf_model, rmse = rmse)
  
  cat(paste0("mtry = ", m, " → RMSE: ", round(rmse, 4), "\n"))
}

# -----------------------------------
# 4. Pick the best model (lowest RMSE)
# -----------------------------------
best_idx <- which.min(rmse_values)
best_mtry <- mtry_values[best_idx]
best_rmse <- rmse_values[best_idx]
cat("\nBest mtry:", best_mtry, "| RMSE:", round(best_rmse, 4), "\n")

best_rf_model <- rf_results_list[[paste0("mtry_", best_mtry)]]$model

# -----------------------------------
# 5. Variable Importance Plot
# -----------------------------------
vip <- data.frame(
  Variable = names(best_rf_model$variable.importance),
  Importance = best_rf_model$variable.importance
)

vip <- vip %>% arrange(desc(Importance)) %>% head(10)

ggplot(vip, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_col(fill = "forestgreen") +
  coord_flip() +
  labs(title = paste("Top 10 Variable Importance (mtry =", best_mtry, ")"),
       x = "Variable", y = "Importance") +
  theme_minimal()


#_____________________________________PRAGUE____________________________________
#_____________________________________2025______________________________________
#_____________________________________GradientBoosting__________________________
library(xgboost)
# -----------------------------------
# 1. Define formula and split data
# -----------------------------------
boost_formula <- ln_price ~ accommodates + number_of_reviews_n + f_bathroom + f_minimum_nights +
  property_type + f_room_type + refrigerator + microwave + wifi

set.seed(223)
split_index <- createDataPartition(data2024$ln_price, p = 0.7, list = FALSE)
train_boost <- data2024[split_index, ]
test_boost <- data2024[-split_index, ]

vars_boost <- all.vars(boost_formula)
train_boost <- na.omit(train_boost[, c("ln_price", vars_boost)])
test_boost <- na.omit(test_boost[, c("ln_price", vars_boost)])

# -----------------------------------
# 2. Create model matrix (required by xgboost)
# -----------------------------------
X_train <- model.matrix(boost_formula, data = train_boost)[, -1]
y_train <- train_boost$ln_price
X_test <- model.matrix(boost_formula, data = test_boost)[, -1]
y_test <- test_boost$ln_price

# -----------------------------------
# 3. Light tuning grid
# -----------------------------------
xgb_grid <- expand.grid(
  nrounds = 150,
  max_depth = 5,
  eta = 0.1,
  gamma = 0,
  colsample_bytree = 0.8,
  min_child_weight = 1,
  subsample = 0.8
)

# -----------------------------------
# 4. Train control (no CV)
# -----------------------------------
train_control <- trainControl(method = "none")  # just train once

# -----------------------------------
# 5. Train the model
# -----------------------------------
set.seed(125)
xgb_model <- train(
  x = X_train,
  y = y_train,
  method = "xgbTree",
  trControl = train_control,
  tuneGrid = xgb_grid,
  verbose = FALSE
)

# -----------------------------------
# 6. Predict and evaluate
# -----------------------------------
xgb_preds <- predict(xgb_model, newdata = X_test)
xgb_rmse <- RMSE(xgb_preds, y_test)
cat("XGBoost Test RMSE:", round(xgb_rmse, 4), "\n")

# -----------------------------------
# 7. Feature importance plot
# -----------------------------------
xgb_importance <- xgb.importance(model = xgb_model$finalModel)
xgb.plot.importance(xgb_importance[1:10, ], rel_to_first = TRUE, xlab = "Relative Importance")























