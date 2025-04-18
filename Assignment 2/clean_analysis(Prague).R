library(caret)
library(estimatr)
library(Metrics)
library(skimr)
library(glmnet)
library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)

rm(list = ls())

setwd("C:/Users/Пользователь/Desktop/MA1y/Prediction_with_ML/Machine-Learning/Assignment 2")

data2025 <- read.csv("Prague2025q1_final.csv")
data2024 <- read.csv("Prague2024q4_final.csv")

skim(data2024$ln_price)
skim(data2025$ln_price)

data2024$f_room_type
data2025$f_room_type

#--------------------------------------PRAGUE-----------------------------------
#-----------------------------------OLS FUNCTION--------------------------------

ols_cv_rmse <- function(data, seed_val = 111) {
  set.seed(seed_val)
  folds <- createFolds(data$ln_price, k = 5)
  
  rmse_matrix <- matrix(NA, nrow = 5, ncol = 3)
  colnames(rmse_matrix) <- c("Model1", "Model2", "Model3")
  rownames(rmse_matrix) <- paste("Fold", 1:5)
  
  formulas <- list(
    ln_price ~ property_type,
    ln_price ~ property_type + f_room_type + ln_accommodates + ln_accommodates2 +
      ln_number_of_reviews + f_bathroom + f_minimum_nights,
    ln_price ~ property_type + f_room_type + ln_accommodates + ln_accommodates2 + ln_number_of_reviews +
      f_bathroom + beds_n + f_minimum_nights + refrigerator + microwave + wifi + smoke.alarm +
      heating + hot.water + essentials + dining.table + bed.linens + shampoo + iron
  )
  
  predictions_list <- list()
  
  for (i in 1:5) {
    train <- data[-folds[[i]], ]
    test <- data[folds[[i]], ]
    
    for (j in 1:3) {
      model <- lm_robust(formulas[[j]], data = train)
      preds <- predict(model, newdata = test)
      rmse_matrix[i, j] <- RMSE(preds, test$ln_price)
      
      predictions_list[[paste0("Fold_", i, "_Model_", j)]] <- data.frame(
        Fold = i,
        Model = paste0("Model_", j),
        Actual = test$ln_price,
        Predicted = preds
      )
    }
  }
  
  rmse_df <- as.data.frame(rmse_matrix)
  mean_row <- data.frame(t(colMeans(rmse_df)))
  rownames(mean_row) <- "mean value"
  rmse_df <- rbind(rmse_df, mean_row)
  rmse_df$Fold <- rownames(rmse_df)
  
  long_df <- melt(rmse_df, id.vars = "Fold", variable.name = "Model", value.name = "RMSE")
  predictions_df <- do.call(rbind, predictions_list)
  predictions_df$Residuals <- predictions_df$Actual - predictions_df$Predicted
  
  list(rmse = rmse_df, long = long_df, residuals = predictions_df)
}

eval2024 <- ols_cv_rmse(data2024, seed_val = 113)
eval2025 <- ols_cv_rmse(data2025, seed_val = 124)

eval2024$rmse
eval2024$long
eval2024$residuals

ggplot(eval2024$long, aes(x = Fold, y = RMSE, group = Model, color = Model)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(title = "RMSE Across 5 Folds for Each Model (2024)", x = "Fold", y = "RMSE") +
  theme_minimal()

ggplot(eval2024$long, aes(x = Model, y = RMSE, fill = Model)) +
  geom_boxplot() +
  labs(title = "RMSE Distribution Across Models (2024)", x = "Model", y = "RMSE") +
  theme_minimal()

ggplot(eval2025$long, aes(x = Fold, y = RMSE, group = Model, color = Model)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(title = "RMSE Across 5 Folds for Each Model (2024)", x = "Fold", y = "RMSE") +
  theme_minimal()

ggplot(eval2025$long, aes(x = Model, y = RMSE, fill = Model)) +
  geom_boxplot() +
  labs(title = "RMSE Distribution Across Models (2024)", x = "Model", y = "RMSE") +
  theme_minimal()

#------------------------------- LASSO FUNCTION --------------------------------
lasso_cv_rmse <- function(data, formula, seed_val = 5554) {
  set.seed(seed_val)
  vars_needed <- all.vars(formula)
  data_lasso <- na.omit(data[, c("ln_price", vars_needed)])
  folds <- createFolds(data_lasso$ln_price, k = 5)
  
  rmse_list <- numeric(5)
  
  for (i in 1:5) {
    train <- data_lasso[-folds[[i]], ]
    test <- data_lasso[folds[[i]], ]
    
    X_train <- model.matrix(formula, data = train)[, -1]
    y_train <- train$ln_price
    X_test <- model.matrix(formula, data = test)[, -1]
    y_test <- test$ln_price
    
    lasso_cv <- cv.glmnet(X_train, y_train, alpha = 1, nfolds = 5)
    best_lambda <- lasso_cv$lambda.min
    
    preds <- predict(lasso_cv, newx = X_test, s = best_lambda)
    rmse_list[i] <- RMSE(preds, y_test)
  }
  
  mean_rmse <- mean(rmse_list)
  
  data.frame(
    Fold = c(paste("Fold", 1:5), "mean value"),
    Model = "LASSO (with interactions)",
    RMSE = round(c(rmse_list, mean_rmse), 4)
  )
}

lasso_formula <- ln_price ~ property_type + f_room_type + ln_accommodates + ln_accommodates2 + ln_number_of_reviews +
  f_bathroom + beds_n + f_minimum_nights + refrigerator + microwave + wifi + smoke.alarm +
  heating + hot.water + essentials + dining.table + bed.linens + shampoo + iron +
  property_type*ln_accommodates + property_type*f_room_type + property_type*f_minimum_nights +
  property_type*refrigerator + property_type*microwave + property_type*wifi +
  property_type*smoke.alarm + property_type*heating + property_type*hot.water + 
  property_type*dining.table + property_type*beds_n + property_type*f_bathroom
  

lasso2024_df <- lasso_cv_rmse(data2024, lasso_formula, seed_val = 1117)
lasso2025_df <- lasso_cv_rmse(data2025, lasso_formula, seed_val = 1118)

# Combine and plot
rmse_all_2024 <- rbind(eval2024$long, lasso2024_df)
rmse_all_2025 <- rbind(eval2025$long, lasso2025_df)

# Plot combined RMSEs
ggplot(rmse_all_2024, aes(x = Fold, y = as.numeric(RMSE), group = Model, color = Model)) +
  geom_line(size = 1) + geom_point(size = 2) +
  labs(title = "RMSE Across 5 Folds (2024)", y = "RMSE", x = "Fold") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(rmse_all_2024, aes(x = Model, y = RMSE, fill = Model)) +
  geom_boxplot() +
  labs(title = "RMSE Distribution Across Models (2024)", x = "Model", y = "RMSE") +
  theme_minimal()

ggplot(rmse_all_2025, aes(x = Fold, y = as.numeric(RMSE), group = Model, color = Model)) +
  geom_line(size = 1) + geom_point(size = 2) +
  labs(title = "RMSE Across 5 Folds (2025)", y = "RMSE", x = "Fold") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(rmse_all_2025, aes(x = Model, y = RMSE, fill = Model)) +
  geom_boxplot() +
  labs(title = "RMSE Distribution Across Models (2024)", x = "Model", y = "RMSE") +
  theme_minimal()

#------------------------------- RANDOM FOREST ---------------------------------

rf_formula <- ln_price ~ property_type + f_room_type + ln_accommodates + ln_accommodates2 + ln_number_of_reviews +
  f_bathroom + beds_n + f_minimum_nights + refrigerator + microwave + wifi + smoke.alarm +
  heating + hot.water + essentials + dining.table + bed.linens + shampoo + iron

run_rf_cv <- function(data, year_label) {
  set.seed(2223)
  vars_rf <- all.vars(rf_formula)
  data_rf <- na.omit(data[, c("ln_price", vars_rf)])
  
  train_control <- trainControl(method = "cv", number = 5, savePredictions = "all",
                                returnResamp = "all")
  tune_grid <- expand.grid(
    mtry = c(3, 5, 7, 9),
    splitrule = "variance",
    min.node.size = 5
  )
  
  rf_model_cv <- train(
    rf_formula,
    data = data_rf,
    method = "ranger",
    trControl = train_control,
    tuneGrid = tune_grid,
    num.trees = 500,
    importance = "impurity"
  )
  
  print(rf_model_cv)
  cat("\nBest mtry (", year_label, "):", rf_model_cv$bestTune$mtry,
      "| RMSE:", min(rf_model_cv$results$RMSE), "\n")
  
  vip <- varImp(rf_model_cv)$importance %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = "Variable") %>%
    arrange(desc(Overall)) %>%
    head(10)
  
  return(list(model = rf_model_cv, vip = vip))
}

rf_2024 <- run_rf_cv(data2024, "2024")
rf_2025 <- run_rf_cv(data2025, "2025")


ggplot(rf_2024$vip, aes(x = reorder(Variable, Overall), y = Overall)) +
  geom_col(fill = "forestgreen") +
  coord_flip() +
  labs(title = paste("Top 10 Variable Importance - 2024",
                     "(mtry =", rf_2024$model$bestTune$mtry, ")"),
       x = "Variable", y = "Importance") +
  theme_minimal()

ggplot(rf_2025$vip, aes(x = reorder(Variable, Overall), y = Overall)) +
  geom_col(fill = "forestgreen") +
  coord_flip() +
  labs(title = paste("Top 10 Variable Importance - 2025",
                     "(mtry =", rf_2024$model$bestTune$mtry, ")"),
       x = "Variable", y = "Importance") +
  theme_minimal()

rf_rmse_folds <- rf_2024$model$resample %>%
  filter(mtry == rf_2024$model$bestTune$mtry) %>%
  mutate(
    Fold = paste("Fold", row_number()),
    Model = "Random Forest",
    RMSE = RMSE
  ) %>%
  select(Fold, Model, RMSE)


#---------------------------------- XGB BOOSTING -------------------------------
library(xgboost)

boost_formula <- ln_price ~ property_type + f_room_type + ln_accommodates + ln_accommodates2 + ln_number_of_reviews +
  f_bathroom + beds_n + f_minimum_nights + refrigerator + microwave + wifi + smoke.alarm +
  heating + hot.water + essentials + dining.table + bed.linens + shampoo + iron

run_xgb_cv <- function(data, year_label) {
  set.seed(223)
  vars <- all.vars(boost_formula)
  data_boost <- na.omit(data[, c("ln_price", vars)])
  
  X <- model.matrix(boost_formula, data = data_boost)[, -1]
  y <- data_boost$ln_price
  
  xgb_grid <- expand.grid(
    nrounds = c(200, 300),
    max_depth = c(4, 6),
    eta = c(0.03, 0.1),
    gamma = c(0, 1),
    colsample_bytree = 0.6,
    min_child_weight = c(5, 10),
    subsample = 0.6
  )
  
  train_control <- trainControl(method = "cv", number = 5,
                                savePredictions = "all", 
                                returnResamp = "all")
  
  set.seed(125)
  xgb_model_cv <- suppressWarnings(train(
    x = X,
    y = y,
    method = "xgbTree",
    trControl = train_control,
    tuneGrid = xgb_grid,
    verbose = FALSE
  ))
  
  print(xgb_model_cv)
  cat("\nXGBoost", year_label, "| Best RMSE:", min(xgb_model_cv$results$RMSE), "\n")
  
  # Extract top features
  xgb_imp <- xgb.importance(model = xgb_model_cv$finalModel)
  xgb_top10 <- xgb_imp[1:10, ]
  
  return(list(model = xgb_model_cv, importance = xgb_top10))
}

xgb_2024 <- run_xgb_cv(data2024, "2024")
xgb_2025 <- run_xgb_cv(data2025, "2025")

# Plot feature importance
xgb.plot.importance(xgb_2024$importance, rel_to_first = TRUE,
                    xlab = "Relative Importance", main = "Top 10 Features - XGBoost 2024")
xgb.plot.importance(xgb_2025$importance, rel_to_first = TRUE,
                    xlab = "Relative Importance", main = "Top 10 Features - XGBoost 2025")

#--------------------------------- GBM BOOSTING --------------------------------
library(gbm)

run_gbm_cv <- function(data, year_label) {
  set.seed(2025)
  
  gbm_formula <- ln_price ~ property_type + f_room_type + ln_accommodates + ln_accommodates2 +
    ln_number_of_reviews + f_bathroom + beds_n + f_minimum_nights +
    refrigerator + microwave + wifi + smoke.alarm + heating + hot.water +
    essentials + dining.table + bed.linens + shampoo + iron
  
  vars <- all.vars(gbm_formula)
  data_gbm <- na.omit(data[, c("ln_price", vars)])
  
  train_control <- trainControl(
    method = "cv",
    number = 5,
    savePredictions = "all",
    returnResamp = "all"
  )
  
  gbm_grid <- expand.grid(
    interaction.depth = c(3, 5),
    n.trees = c(100, 200),
    shrinkage = c(0.05, 0.1),
    n.minobsinnode = 10
  )
  
  gbm_model_cv <- train(
    gbm_formula,
    data = data_gbm,
    method = "gbm",
    trControl = train_control,
    tuneGrid = gbm_grid,
    verbose = FALSE
  )
  
  return(gbm_model_cv)
}

gbm_2024 <- run_gbm_cv(data2024, "2024")
gbm_2025 <- run_gbm_cv(data2025, "2025")

gbm_importance <- varImp(gbm_2024)$importance %>%
  as.data.frame() %>%
  tibble::rownames_to_column(var = "Variable") %>%
  arrange(desc(Overall))

ggplot(gbm_importance[1:10, ], aes(x = reorder(Variable, Overall), y = Overall)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top 10 Variable Importance - GBM (2024)",
    x = "Variable",
    y = "Importance"
  ) +
  theme_minimal()


gbm_importance2 <- varImp(gbm_2025)$importance %>%
  as.data.frame() %>%
  tibble::rownames_to_column(var = "Variable") %>%
  arrange(desc(Overall))

ggplot(gbm_importance2[1:10, ], aes(x = reorder(Variable, Overall), y = Overall)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top 10 Variable Importance - GBM (2025)",
    x = "Variable",
    y = "Importance"
  ) +
  theme_minimal()

#-----------------HORSE RACE: Comparing OLS, LASSO, RF, and XGBoost 2024--------

# Extract RMSEs for Random Forest
best_params <- rf_2024$model$bestTune
rf_rmse_folds <- rf_2024$model$resample %>%
  filter(
    mtry == best_params$mtry,
    splitrule == best_params$splitrule,
    min.node.size == best_params$min.node.size
  ) %>%
  mutate(
    Fold = paste("Fold", row_number()),
    Model = "Random Forest",
    RMSE = RMSE
  ) %>%
  select(Fold, Model, RMSE)


# Extract RMSEs for XGBoost
xgb_preds <- xgb_2024$model$pred
best_xgb <- xgb_2024$model$bestTune

xgb_preds_filtered <- xgb_2024$model$pred %>%
  filter(
    nrounds == best_xgb$nrounds,
    max_depth == best_xgb$max_depth,
    eta == best_xgb$eta,
    gamma == best_xgb$gamma,
    min_child_weight == best_xgb$min_child_weight
  )

xgb_rmse_folds <- xgb_preds_filtered %>%
  group_by(Resample) %>%
  summarise(
    RMSE = RMSE(obs, pred),
    .groups = "drop"
  ) %>%
  mutate(
    Fold = Resample,
    Model = "XGBoost"
  ) %>%
  select(Fold, Model, RMSE)


# Extract RMSEs for GBMboost
best_gbm <- gbm_2024$bestTune

gbm_preds <- gbm_2024$pred %>%
  filter(
    interaction.depth == best_gbm$interaction.depth,
    n.trees == best_gbm$n.trees,
    shrinkage == best_gbm$shrinkage,
    n.minobsinnode == best_gbm$n.minobsinnode
  )

gbm_rmse_folds <- gbm_preds %>%
  group_by(Resample) %>%
  summarise(RMSE = RMSE(obs, pred), .groups = "drop") %>%
  mutate(
    Fold = gsub("Fold", "Fold ", Resample),
    Model = "GBM"
  ) %>%
  select(Fold, Model, RMSE)

# Extract RMSEs for OLS (Model 3 only)
ols_rmse_folds <- eval2024$long %>%
  filter(Model == "Model3" & Fold != "mean value") %>%
  mutate(Model = "OLS")

# Extract RMSEs for LASSO
lasso_rmse_folds <- lasso2024_df %>%
  filter(Fold != "mean value") %>%
  rename(RMSE = RMSE) %>%
  select(Fold, Model, RMSE)

# Combine all models
xgb_rmse_folds <- xgb_rmse_folds %>%
  mutate(Fold = gsub("Fold", "Fold ", Fold))

horserace_rmse <- bind_rows(ols_rmse_folds, lasso_rmse_folds, rf_rmse_folds,
                            xgb_rmse_folds, gbm_rmse_folds)


# Plot: RMSE per fold
ggplot(horserace_rmse, aes(x = Fold, y = as.numeric(RMSE), group = Model, color = Model)) +
  geom_line(size = 1) + geom_point(size = 2) +
  labs(title = "Horserace: Model Comparison (2024)", y = "RMSE", x = "Fold") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot: RMSE distribution across models
ggplot(horserace_rmse, aes(x = Model, y = as.numeric(RMSE), fill = Model)) +
  geom_boxplot() +
  labs(title = "RMSE Distribution by Model (2024)", y = "RMSE", x = "Model") +
  theme_minimal()


#-----------------HORSE RACE: Comparing OLS, LASSO, RF, and XGBoost 2025--------

# Extract RMSEs for Random Forest
best_params2 <- rf_2025$model$bestTune
rf_rmse_folds2 <- rf_2025$model$resample %>%
  filter(
    mtry == best_params2$mtry,
    splitrule == best_params2$splitrule,
    min.node.size == best_params2$min.node.size
  ) %>%
  mutate(
    Fold = paste("Fold", row_number()),
    Model = "Random Forest",
    RMSE = RMSE
  ) %>%
  select(Fold, Model, RMSE)


# Extract RMSEs for XGBoost
xgb_preds2 <- xgb_2025$model$pred
best_xgb2 <- xgb_2025$model$bestTune

xgb_preds_filtered2 <- xgb_2025$model$pred %>%
  filter(
    nrounds == best_xgb2$nrounds,
    max_depth == best_xgb2$max_depth,
    eta == best_xgb2$eta,
    gamma == best_xgb2$gamma,
    min_child_weight == best_xgb2$min_child_weight
  )

xgb_rmse_folds2 <- xgb_preds_filtered2 %>%
  group_by(Resample) %>%
  summarise(
    RMSE = RMSE(obs, pred),
    .groups = "drop"
  ) %>%
  mutate(
    Fold = Resample,
    Model = "XGBoost"
  ) %>%
  select(Fold, Model, RMSE)
xgb_rmse_folds2 <- xgb_rmse_folds2 %>%
  mutate(Fold = gsub("Fold", "Fold ", Fold))


# Extract RMSEs for GBMboost
best_gbm2 <- gbm_2025$bestTune

gbm_preds2 <- gbm_2025$pred %>%
  filter(
    interaction.depth == best_gbm2$interaction.depth,
    n.trees == best_gbm2$n.trees,
    shrinkage == best_gbm2$shrinkage,
    n.minobsinnode == best_gbm2$n.minobsinnode
  )

gbm_rmse_folds2 <- gbm_preds2 %>%
  group_by(Resample) %>%
  summarise(RMSE = RMSE(obs, pred), .groups = "drop") %>%
  mutate(
    Fold = gsub("Fold", "Fold ", Resample),
    Model = "GBM"
  ) %>%
  select(Fold, Model, RMSE)


# Extract RMSEs for OLS (Model 3 only)
ols_rmse_folds2 <- eval2025$long %>%
  filter(Model == "Model3" & Fold != "mean value") %>%
  mutate(Model = "OLS")

# Extract RMSEs for LASSO
lasso_rmse_folds2 <- lasso2025_df %>%
  filter(Fold != "mean value") %>%
  rename(RMSE = RMSE) %>%
  select(Fold, Model, RMSE)

# Combine all models
horserace_rmse2 <- bind_rows(ols_rmse_folds2, lasso_rmse_folds2, rf_rmse_folds2,
                             xgb_rmse_folds2, gbm_rmse_folds2)


# Plot: RMSE per fold
ggplot(horserace_rmse2, aes(x = Fold, y = as.numeric(RMSE), group = Model, color = Model)) +
  geom_line(size = 1) + geom_point(size = 2) +
  labs(title = "Horserace: Model Comparison (2025)", y = "RMSE", x = "Fold") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot: RMSE distribution across models
ggplot(horserace_rmse2, aes(x = Model, y = as.numeric(RMSE), fill = Model)) +
  geom_boxplot() +
  labs(title = "RMSE Distribution by Model (2025)", y = "RMSE", x = "Model") +
  theme_minimal()


