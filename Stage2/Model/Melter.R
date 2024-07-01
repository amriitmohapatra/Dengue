rm(list = keep(dir_path, start_date_input_train, end_date_input_train, start_date_input_test, end_date_input_test))
setwd(paste(dir_path,"/Model/Ensemble", sep = ""))
library(dplyr)

n_lags <- 20
n_regions <- 34
load('Final_Predictions.RData')

# df <- df[-1:-(n_lags + 1),]
pred_days <- nrow(regions_forecast[[1]])
week_looper <- seq(from = 7, by = 7, length.out = 12)
## Loop begins
for(weeks in 1:length(week_looper)){
  print(paste("Now working on week: ", weeks, sep = ""))
  setwd(paste(dir_path,"/Model/Ensemble", sep = ""))
  df <- read.csv("case data.csv")
  pred_matrix <- matrix(0, nrow = pred_days, ncol = n_regions)
  
  df$Date <- as.Date(df$end.date, format = "%d/%m/%Y")
  df <- df %>%
    filter(Date < as.character(as.Date(end_date_input_train) + week_looper[weeks]))
  df <- df[-1:(-(n_lags + weeks)),]
  
  ### Create a subset of this dataframe for deviation ops
  true_matrix <- as.matrix(df[,-c(1,2,3,38)]) 
  ### Create another dataframe with predicted values
  
  for(regions in 1:n_regions){
    pred_matrix[,regions] <- regions_forecast[[regions]][,weeks]
  }
  
  dev_matrix <- true_matrix - pred_matrix
  
  colnames(pred_matrix) = colnames(dev_matrix)
  
  
  df_dev <- cbind(df[,c(1,2,3)], data.frame(dev_matrix))
  
  df_true <- cbind(df[,c(1,2,3)], data.frame(true_matrix))
  
  df_pred <- cbind(df[,c(1,2,3)], data.frame(pred_matrix))

  dir_path_2 <- paste(dir_path, "/Plots and Videos/map 2", sep = "")
  dir_path_3 <- paste(dir_path, "/Plots and Videos/map 3", sep = "")
  
  if(weeks == 1){
    setwd(paste(dir_path, "/Plots and Videos", sep = ""))
    dir.create("map 2")
    dir.create("map 3")
  }
  setwd(dir_path_2)
  write.csv(df_dev, file = paste("W_", weeks, "_Deviation_WithinSample.csv",sep = ""), row.names = FALSE)
  write.csv(df_true, file = paste("W_", weeks, "_True_WithinSample.csv",sep = ""), row.names = FALSE)
  write.csv(df_pred, file = paste("W_", weeks, "_Predictions_WithinSample.csv",sep = ""), row.names = FALSE)
  setwd(dir_path_3)
  write.csv(df_dev, file = paste("W_", weeks, "_Deviation_WithinSample.csv",sep = ""), row.names = FALSE)
  write.csv(df_true, file = paste("W_", weeks, "_True_WithinSample.csv",sep = ""), row.names = FALSE)
  write.csv(df_pred, file = paste("W_", weeks, "_Predictions_WithinSample.csv",sep = ""), row.names = FALSE)
}

setwd(paste(dir_path,"/Model", sep = ""))



