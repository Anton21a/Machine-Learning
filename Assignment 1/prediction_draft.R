library(tidyverse)
library(lspline)
library(cowplot)
library(boot)
library(estimatr)
library(huxtable)
library(stargazer)
library(modelsummary)
rm(list=ls())
setwd("C:/Users/Пользователь/Desktop/MA1y/Prediction with ML/Assignment 1")

data_in <- "clean/"

data_all <- read_csv(paste0(data_in,"Driver_or_sales_workers_and_truck_drivers.csv"),
                     col_types = cols(.default = "?", 
                                      state = "c"))

library(dplyr)
data <- data_all %>%
  mutate(
    hhid = hhid,
    weight = weight,
    week_wage = earnwke,
    working_hours_week = uhours,
    hour_wage = earnwke / uhours,
    lnhour_wage = log(earnwke / uhours),
    
    educ_hsless = ifelse(grade92 >= 31 & grade92 < 39, 1, 0),   # Not High School Graduate
    educ_hsgrad = ifelse(grade92 == 39, 1, 0),                  # High School Graduate
    educ_college = ifelse(grade92 >= 40 & grade92 < 43, 1, 0),  # College or Professional
    educ_ba_plus = ifelse(grade92 >= 43, 1, 0),                 # BA, MA, or Doctorate
    
    race_white = ifelse(race == 1, 1, 0),
    race_black = ifelse(race == 2, 1, 0),
    race_asian = ifelse(race == 4, 1, 0),
    race_other = ifelse(race %in% c(3, 5, 6, 7), 1, 0),  # Other races
    
    age2 = age^2,
    age = age,
    
    female = ifelse(sex == 2, 1, 0),  # 1 if Female, 0 if Male
    
    marital_married = ifelse(marital >= 1 & marital < 4, 1, 0), # Married
    marital_nonmarried = ifelse(marital > 4 & marital < 7, 1, 0), # Non-Married
    marital_never = ifelse(marital == 7, 1, 0), # Never Married
    
    child_1 = ifelse(ownchild == 1, 1, 0),
    child_2 = ifelse(ownchild == 2, 1, 0),
    child_3 = ifelse(ownchild == 3, 1, 0),
    child_4plus = ifelse(ownchild >= 4, 1, 0)  # More than 3 children
  )

data <- data %>%
  dplyr::select(hhid, weight, week_wage, working_hours_week, hour_wage, lnhour_wage,
                educ_hsless, educ_hsgrad, educ_college, educ_ba_plus,
                race_white, race_black, race_asian, race_other,
                age, age2, female,
                marital_married, marital_nonmarried, marital_never,
                child_1, child_2, child_3, child_4plus)
str(data)

saveRDS(data, "data_modified.rds")

data <- readRDS("data_modified.rds")

library(estimatr)
reg1 <- lm_robust(lnhour_wage ~ race_black + race_asian + race_other + female, 
                  data = data, se_type = "HC1")


reg2 <- lm_robust(lnhour_wage ~ race_black + race_asian + race_other + female + age + age2, data = data, se_type = "HC1")


reg3 <- lm_robust(lnhour_wage ~ race_black + race_asian + race_other + female + age + age2 + marital_married + marital_nonmarried, data = data, se_type = "HC1")


reg4 <- lm_robust(lnhour_wage ~ race_black + race_asian + race_other + female + age + age2 +  marital_married + marital_nonmarried + educ_hsgrad + educ_college + educ_ba_plus, data = data, se_type = "HC1")



gm <-  c('R2' = 'R-squared (%)',
         'se_type' = 'SE type')

cm <- c('R2' = 'R-squared (%)',
        'lspline(age, c(30, 40))2' = 'spline(age, c(20,30))',
        '(Intercept)' = 'Constant')
msummary(list(reg1, reg2, reg3, reg4),
         fmt="%.3f",
         gof_omit = 'DF|Deviance|Log.Lik.|F',
         stars=TRUE,
         coef_rename = cm,
         #output = paste(output,"ch09_reg2-R.tex",sep="")
)

library(caret)  
library(estimatr) 
library(Metrics)  

set.seed(42)
folds <- createFolds(data$lnhour_wage, k = 5, list = TRUE) 
folds# 5-fold CV

rmse_matrix <- matrix(NA, nrow = 5, ncol = 4)  # 5 rows (folds), 4 columns (models)
colnames(rmse_matrix) <- c("Model1", "Model2", "Model3", "Model4")
rownames(rmse_matrix) <- paste("Fold", 1:5)


for (i in 1:5) { 
  # Create train-test split for this fold
  test_indices <- folds[[i]]
  train_data <- data[-test_indices, ]  # Train set: all except current fold
  test_data <- data[test_indices, ]    # Test set: current fold
  
  # Define regression formulas
  formulas <- list(
    reg1 = lnhour_wage ~ race_black + race_asian + race_other + female,
    reg2 = lnhour_wage ~ race_black + race_asian + race_other + female + age + age2,
    reg3 = lnhour_wage ~ race_black + race_asian + race_other + female + age + age2 + marital_married + marital_nonmarried,
    reg4 = lnhour_wage ~ race_black + race_asian + race_other + female + age + age2 + marital_married + marital_nonmarried + educ_hsgrad + educ_college + educ_ba_plus
  )
  
  predictions_list <- list()
  
  for (j in 1:4) {
    # Train model on training data
    model <- lm_robust(formulas[[j]], data = train_data)
    
    # Make predictions on test data
    preds <- predict(model, newdata = test_data)
    
    # Compute RMSE for this fold and model
    rmse_matrix[i, j] <- RMSE(preds, test_data$lnhour_wage)
    
    predictions_list[[paste0("Fold_", i, "_Model_", j)]] <- data.frame(
      Fold = i,
      Model = paste0("Model_", j),
      Actual = test_data$lnhour_wage,
      Predicted = preds
    )
  }
}


# Print RMSE matrix
rmse_matrix <- as.data.frame(rmse_matrix)
rmse_matrix$`Model 1`
mean_rmse <- data.frame(Model1 = mean(rmse_matrix[, 1]),
                        Model2 = mean(rmse_matrix[, 2]),
                        Model3 = mean(rmse_matrix[, 3]),
                        Model4 = mean(rmse_matrix[, 4]))

rmse_matrix <- rbind(rmse_matrix, mean_rmse)

rownames(rmse_matrix)[6] <- 'mean value'
rmse_matrix


# Convert RMSE matrix to data frame
rmse_df <- as.data.frame(rmse_matrix)
rmse_df$Fold <- rownames(rmse_matrix)

# Reshape into long format for ggplot
library(data.table)
rmse_long <- melt(rmse_df, id.vars = "Fold", variable.name = "Model", value.name = "RMSE")

ggplot(rmse_long, aes(x = Fold, y = RMSE, group = Model, color = Model)) +
  geom_line(size = 1) + 
  geom_point(size = 3) +
  labs(title = "RMSE Across 5 Folds for Each Model", 
       x = "Fold", 
       y = "RMSE") +
  theme_minimal()

predictions_df <- do.call(rbind, predictions_list)

predictions_df <- predictions_df %>%
  filter(Model == "Model_4")

ggplot(predictions_df, aes(y = Actual, x = Predicted)) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") + 
  labs(title = "Actual vs. Predicted Values", 
       x = "Actual Lnhour Wage", 
       y = "Predicted Lnhour Wage") +
  theme_minimal()


predictions_df$Residuals <- predictions_df$Actual - predictions_df$Predicted

ggplot(predictions_df, aes(x = Residuals, fill = Model)) +
  geom_density(alpha = 0.5) +
  labs(title = "Residual Distribution", 
       x = "Residuals (Actual - Predicted)", 
       y = "Density") +
  theme_minimal()


ggplot(rmse_long, aes(x = Model, y = RMSE, fill = Model)) +
  geom_boxplot() +
  labs(title = "RMSE Distribution Across Models", 
       x = "Model", 
       y = "RMSE") +
  theme_minimal()


