rm(list = keep(dir_path, start_date_input_train, end_date_input_train, start_date_input_test, end_date_input_test))

### This script computes and stores the RMSE, MSE, MAE and MAAPE values from all ML algos for all week ahead predictions
library(dplyr)
library(lubridate)

ML_names <- c("LASSO", "SVR1", "SVR2", "SVR3", "SVR4", "DT1", "DT2", "Random Forest", "Bagging", "GBM1", "GMB2", "LSTM", "GRU", "Ensemble")
################################ Training Set ###############################################

load("All_Forecasts.RData")
load("Predictions Ensemble.RData")
All_Predictions_Master[[14]] <- Predictions_Ensemble_Results[1, , ]

n_ML = 14 # 13 indiv and ensemble
n_models = 12
n_lags = 20

# Since we will do this by week, we need to transform this All_Predictions_Master list to a format where all data can be stored week_wise.
# Need a function that extracts the actual total dengue cases data as per our requirement (i.e. based on week-ahead models)
# Ref to the Master file for summ stats

dataset_total <- read.csv("Dengue Weekly Total.csv")
dataset_total$Date <- as.Date(dataset_total$Last.Date.of.this.week, format = "%d/%m/%Y")
train_days <- nrow(All_Predictions_Master[[1]])
sd_residuals = matrix(NA, nrow = n_models ,ncol = n_ML)

actual_data_begin_index <- n_lags + 2
actual_data_end_index <- actual_data_begin_index - 1 + train_days


ape_array = aape_array = dev_sq_array = dev_abs_array <- array(NA, dim = c(train_days, n_models, n_ML))

for(ML in 1:n_ML){
  print(paste("Algorithm: ", ML_names[ML], sep = ""))
  for(weeks in 1:n_models){
    actual <- dataset_total[(actual_data_begin_index + weeks - 1) : (actual_data_end_index + weeks - 1), c("Singapore")]
    for(rows in 1:train_days){
      dev_abs_array[rows, weeks, ML] <- abs(All_Predictions_Master[[ML]][rows,weeks] - actual[rows])
      dev_sq_array[rows, weeks, ML] <- (All_Predictions_Master[[ML]][rows,weeks] - actual[rows])^2
      aape_array[rows, weeks, ML] <- atan(abs(All_Predictions_Master[[ML]][rows,weeks] - actual[rows]) /actual[rows])
      ape_array[rows, weeks, ML] <- abs(All_Predictions_Master[[ML]][rows,weeks] - actual[rows]) / actual[rows]
    }
  }
}


MSE <- apply(dev_sq_array, c(2,3), mean)
RMSE <- sqrt(MSE)

MAE <- apply(dev_abs_array, c(2,3), mean)
MAPE <- apply(ape_array, c(2,3), mean)
MAAPE <- apply(aape_array, c(2,3), mean)

colnames(MSE) = colnames(RMSE) = colnames(MAE) = colnames(MAPE) = colnames(MAAPE) = ML_names
rownames(MSE) = rownames(RMSE) = rownames(MAE) = rownames(MAPE) = rownames(MAAPE) = c("W1", "W2", "W3", "W4", "W5", "W6", "W7", "W8", "W9", "W10", "W11", "W12")

write.csv(RMSE, "RMSE_Precision.csv", sep = ",", col.names = TRUE, row.names = TRUE)
write.csv(MAE, "MAE_Precision.csv", sep = ",", col.names = TRUE, row.names = TRUE)
write.csv(MAPE, "MAPE_Precision.csv", sep = ",", col.names = TRUE, row.names = TRUE)
write.csv(MAAPE, "MAAPE_Precision.csv", sep = ",", col.names = TRUE, row.names = TRUE)

################################ Test Set ###############################################

load("All_Forecasts_OOS.RData")
load("Predictions Ensemble OOS.RData")
Predictions_Ensemble <- Predictions_Ensemble_Results[1, , ]
All_Predictions[[14]] <- Predictions_Ensemble
test_days <- nrow(All_Predictions[[1]])

ape_array = aape_array = dev_sq_array = dev_abs_array <- array(NA, dim = c(test_days, n_models, n_ML))

rm(All_Predictions_Master)
All_Predictions_Master <- All_Predictions


for(ML in 1:n_ML){
  print(paste("Algorithm: ", ML_names[ML], sep = ""))
  for(weeks in 1:n_models){
    actual <- dataset_total[(actual_data_end_index + 1 + weeks) : (actual_data_end_index + test_days + weeks), c("Singapore")]
    for(rows in 1:test_days){
      dev_abs_array[rows, weeks, ML] <- abs(All_Predictions_Master[[ML]][rows,weeks] - actual[rows])
      dev_sq_array[rows, weeks, ML] <- (All_Predictions_Master[[ML]][rows,weeks] - actual[rows])^2
      aape_array[rows, weeks, ML] <- atan(abs(All_Predictions_Master[[ML]][rows,weeks] - actual[rows]) /actual[rows])
      ape_array[rows, weeks, ML] <- abs(All_Predictions_Master[[ML]][rows,weeks] - actual[rows]) / actual[rows]
    }
  }
}


MSE_OOS <- apply(dev_sq_array, c(2,3), mean)
RMSE_OOS <- sqrt(MSE_OOS)

MAE_OOS <- apply(dev_abs_array, c(2,3), mean)
MAPE_OOS <- apply(ape_array, c(2,3), mean)
MAAPE_OOS <- apply(aape_array, c(2,3), mean)

colnames(MSE_OOS) = colnames(RMSE_OOS) = colnames(MAE_OOS) = colnames(MAPE_OOS) = colnames(MAAPE_OOS) = ML_names
rownames(MSE_OOS) = rownames(RMSE_OOS) = rownames(MAE_OOS) = rownames(MAPE_OOS) = rownames(MAAPE_OOS) = c("W1", "W2", "W3", "W4", "W5", "W6", "W7", "W8", "W9", "W10", "W11", "W12")

write.csv(RMSE_OOS, "RMSE_OOS_Precision.csv", sep = ",", col.names = TRUE, row.names = TRUE)
write.csv(MAE_OOS, "MAE_OOS_Precision.csv", sep = ",", col.names = TRUE, row.names = TRUE)
write.csv(MAPE_OOS, "MAPE_OOS_Precision.csv", sep = ",", col.names = TRUE, row.names = TRUE)
write.csv(MAAPE_OOS, "MAAPE_OOS_Precision.csv", sep = ",", col.names = TRUE, row.names = TRUE)







