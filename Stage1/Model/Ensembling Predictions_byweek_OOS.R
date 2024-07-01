rm(list = keep(dir_path, start_date_input_train, end_date_input_train, start_date_input_test, end_date_input_test))
library(stats)
library(dplyr)
library(ggplot2)
library(lubridate)
library(deSolve)
library(LaplacesDemon)
library(zoo)
library(grid)
library(abind)
dataset_total = read.csv("Dengue Weekly Total.csv")
dataset_total$Date <- as.Date(dataset_total$Last.Date.of.this.week, format = "%d/%m/%Y")

n_models <- 12 # number of week-ahead predictions being done
n_lags <- 20 #number of lags of feature set being considered.

all_dates <- dataset_total$Date
# dataset_total_dates <- dataset_total$Date
dataset_total <- subset(dataset_total, select = -c(1,9,10))
# dataset_allreg_dates <- dataset_allreg$Date
dataset_total_colnames <- colnames(dataset_total)

for(i in 1:length(dataset_total)){
  for(j in 1:n_lags){
    colname <- paste(dataset_total_colnames[i], ".lag_", j, sep = "")
    dataset_total <- dataset_total %>% mutate(!!colname := lag(.[[i]], j))
  }
}

for(i in 1:n_models){
  colname <- paste(dataset_total_colnames[1], ".lead_", i, sep = "")
  dataset_total <- dataset_total %>% mutate(!!colname := lead(.[[1]], i))
}


dataset_total <- cbind(dataset_total, all_dates)
dataset_total2 = dataset_total
dataset_total <- dataset_total %>%
  filter(all_dates <= as.Date(end_date_input_train))

dataset_total2 <- dataset_total2 %>%
  filter(all_dates >= as.Date(start_date_input_test) & all_dates < as.Date(end_date_input_test))

sub_df = dataset_total[-1:-n_lags, -length(dataset_total)]
sub_df2 = dataset_total2[, -length(dataset_total2)]

n_rows <- nrow(sub_df2)


####################################### Preprocessing #############################################
if(!file.exists("True_Data_OOS.RData")){
  print("True_Data.RData does not exist. Create the RData")
  dataset_total = read.csv("Dengue Weekly Total.csv")
  dataset_total$Date <- as.Date(dataset_total$Last.Date.of.this.week, format = "%d/%m/%Y")
  
  llim <- (n_lags + 1) + nrow(sub_df - 1) + 1
  # actual = list()
  true_data = matrix(0, nrow = n_rows, ncol = 12)
  for(rows in 1:n_rows){
    true_data[rows,] <- dataset_total[(llim + rows):((llim+ 11) +rows), 2]
  }
  
  myfilename = paste("True_Data_OOS",".RData", sep = '')
  save(true_data, file = myfilename)
}else{
  print("True_Data_OOS.RData exists. Load the RData file.")
  load("True_Data_OOS.RData")
}


load("All_Forecasts_OOS.RData")
load("Sampled Weights WS.RData")
rm(dataset_total, dataset_total2, sub_df, sub_df2, all_dates, colname, dataset_total_colnames, i, j)


All_Predictions_Master = All_Predictions
ML_models <- 13

num_lists <- length(All_Predictions_Master)
num_rows <- dim(All_Predictions_Master[[1]])[1]
num_cols <- dim(All_Predictions_Master[[1]])[2]

# Initialize a 3D array with the specified dimensions
all_predictions_array <- array(NA, dim = c(num_lists, num_rows, num_cols))

# Fill the 3D array with your data
for (i in 1:num_lists) {
  all_predictions_array[i, ,] <- All_Predictions_Master[[i]]
}

#

Predictions_Ensemble_List <- list()
for(counter in 1:length(Weights_Matrix_List)){
Predictions_Ensemble <- matrix(0, nrow = n_rows, ncol = n_models)
  for(rows in 1:n_rows){
    for(week_num in 1:12){
      for( ML_model in 1:ML_models){
        # Predictions_Ensemble_Array[counter, rows, week_num] <- Predictions_Ensemble_Array[counter, rows, week_num] + all_predictions_array[ML_model, rows, week_num] * Weights_Matrix_List[[counter]][ML_model, week_num]
        Predictions_Ensemble[rows,week_num] <- Predictions_Ensemble[rows , week_num] + all_predictions_array[ML_model, rows, week_num] * Weights_Matrix_List[[counter]][ML_model, week_num]
      }
    }
  }
Predictions_Ensemble_List[[counter]] <- Predictions_Ensemble
}


# Assuming Predictions_Ensemble_List is your list
Predictions_Ensemble_Array <- array(NA, dim = c(length(Predictions_Ensemble_List), n_rows, n_models))
for (x in 1:length(Predictions_Ensemble_List)) {
  for(y in 1:n_rows){
    for(z in 1:n_models){
      Predictions_Ensemble_Array[x,y,z] <- Predictions_Ensemble_List[[x]][y,z]
    }
  }
}


### Now, from this array, we need the following:
#1. Mean
#2. 2.5%ile
#3. 97.5%ile.
# In total, there will be three output matrices.  


Predictions_Ensemble_Results <- array(NA, dim = c(3, n_rows, n_models))
Predictions_Ensemble_Results[1, , ] <- apply(Predictions_Ensemble_Array, c(2,3), mean, na.rm = TRUE) #1
Predictions_Ensemble_Results[2, , ] <- apply(Predictions_Ensemble_Array, c(2,3), quantile, probs = 0.025, na.rm = TRUE) #2
Predictions_Ensemble_Results[3, , ] <- apply(Predictions_Ensemble_Array, c(2,3), quantile, probs = 0.975, na.rm = TRUE) #3

save(Predictions_Ensemble_Results, file = "Predictions Ensemble OOS.RData")




