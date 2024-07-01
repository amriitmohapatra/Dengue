rm(list = keep(dir_path, start_date_input_train, end_date_input_train, start_date_input_test, end_date_input_test))
library(dplyr)
library(glmnet)
library(e1071)
library(randomForest)
library(caret)
library(gbm)
library(rpart)
library(keras)
library(reticulate)
# library(tensorflow)


All_Predictions <- list()
dataset_total = read.csv("Dengue Weekly Total.csv")
dataset_total$Date <- as.Date(dataset_total$Last.Date.of.this.week, format = "%d/%m/%Y")

n_ML = 13
n_models = 12 # number of week-ahead predictions
n_lags = 20 #number of lags of feature set being considered.


all_dates <- dataset_total$Date
dataset_total <- subset(dataset_total, select = -c(1,9,10))
dataset_total_colnames <- colnames(dataset_total)

# Introducing lagged variables in the feature set
for(i in 1:length(dataset_total)){
  for(j in 1:n_lags){
    colname <- paste(dataset_total_colnames[i], ".lag_", j, sep = "")
    dataset_total <- dataset_total %>% mutate(!!colname := lag(.[[i]], j))
  }
}

# Introducing leading variables for different week-ahead forecasts.
for(i in 1:n_models){
  colname <- paste(dataset_total_colnames[1], ".lead_", i, sep = "")
  dataset_total <- dataset_total %>% mutate(!!colname := lead(.[[1]], i))
}


dataset_total <- cbind(dataset_total, all_dates)
dataset_total <- dataset_total %>%
  filter(all_dates >= as.Date(start_date_input_train) & all_dates <= as.Date(end_date_input_train))

sub_df_true = dataset_total[-1:-n_lags, -length(dataset_total)]
sub_df <- sub_df_true %>%
  mutate_all(~log1p(.))

rm(dataset_total, sub_df_true, all_dates, colname, i,j)
###############################################################
# Creating a series of matrices that will store results of stage 1 for all 12 weeks
LASSO = SVR1 = SVR2 = SVR3 = SVR4 = RF = Bagging = DT1 = DT2 = Boosting1 = Boosting2 = LSTM = GRU = matrix(0, nrow = nrow(sub_df), 12)
# Define the column names
col_names <- c("W1", "W2", "W3", "W4", "W5", "W6", "W7", "W8", "W9", "W10", "W11", "W12")


# Set column names for each matrix individually
colnames(LASSO) <- col_names
colnames(SVR1) <- col_names
colnames(SVR2) <- col_names
colnames(SVR3) <- col_names
colnames(SVR4) <- col_names
colnames(RF) <- col_names
colnames(Bagging) <- col_names
colnames(DT1) <- col_names
colnames(DT2) <- col_names
colnames(Boosting1) <- col_names
colnames(Boosting2) <- col_names
colnames(LSTM) <- col_names
colnames(GRU) <- col_names
rm(col_names)
source("All_Functions.R")
############# Loop can start from here (1:12)
for(models in 1:n_models){
  print(paste("Running all ML Models for ",models," week ahead."))
  YYY = cbind(sub_df[,1:(ncol(sub_df) - n_models)], sub_df[,paste(dataset_total_colnames[1], ".lead_", models, sep = "")])
  colnames(YYY)[length(YYY)] = "target_variable"
  print("Starting LASSO.")
  LASSO[,models] <- LASSO_Model(YYY)
  print("Starting four different types of SVRs.")
  SVR1[,models] <- SVR1_Model(YYY)
  SVR2[,models] <- SVR2_Model(YYY)
  SVR3[,models] <- SVR3_Model(YYY)
  SVR4[,models] <- SVR4_Model(YYY)
  print("Starting two variants of decision trees.")
  DT1[,models] <- DT1_Model(YYY)
  DT2[,models] <- DT2_Model(YYY)
  print("Starting Random Forest.")
  RF[,models] <- RF_Model(YYY)
  print("Starting Bagging.")
  Bagging[,models] <- Bagging_Model(YYY)
  print("Starting two types of Boosted Models.")
  Boosting1[,models] <- Boosting1_Model(YYY)
  Boosting2[,models] <- Boosting2_Model(YYY)
  print("Starting LSTM.")
  LSTM[, models] <- LSTM_Model(YYY)
  print("Starting GRU.")
  GRU[, models] <- GRU_Model(YYY)
  
  if(models == n_models){
    All_Predictions[[1]] <- LASSO
    All_Predictions[[2]] <- SVR1
    All_Predictions[[3]] <- SVR2
    All_Predictions[[4]] <- SVR3
    All_Predictions[[5]] <- SVR4
    All_Predictions[[6]] <- DT1
    All_Predictions[[7]] <- DT2
    All_Predictions[[8]] <- RF
    All_Predictions[[9]] <- Bagging
    All_Predictions[[10]] <- Boosting1
    All_Predictions[[11]] <- Boosting2
    All_Predictions[[12]] <- LSTM
    All_Predictions[[13]] <- GRU
  }
}
################################ Compiling into a Master file #######################

All_Predictions_Master = list()
for(i in 1:n_ML){
  All_Predictions_Master[[i]] <- All_Predictions[[i]]
}
All_Predictions_Master <- lapply(All_Predictions_Master, expm1)
save(All_Predictions_Master, file = "All_Forecasts.RData")
##################################
load("All_Forecasts.RData")





