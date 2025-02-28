rm(list=ls())
data <- mtcars

set.seed(42)

num_splits <- 5  

rmse_values <- c()

prediction_list <- list()

for (i in 1:num_splits) {
  
  # Create 80/20 split
  train_index <- createDataPartition(mtcars$mpg, p = 0.8, list = FALSE)
  train_data <- mtcars[train_index, ]  # 80% Training Data
  test_data <- mtcars[-train_index, ]  # 20% Testing Data
  
  # Train Model: mpg ~ cyl
  model <- lm(mpg ~ cyl, data = train_data)
  
  # Make Predictions
  preds <- predict(model, newdata = test_data)
  
  # Calculate RMSE
  fold_rmse <- rmse(test_data$mpg, preds)
  rmse_values <- c(rmse_values, fold_rmse)
  
  print(paste("Split", i, "RMSE:", round(fold_rmse, 2)))
  
  prediction_list[[paste0("Split_", i)]] <- data.frame(
    Split = i,
    Actual = test_data$mpg,
    Predicted = preds
  )
}

prediction_list
