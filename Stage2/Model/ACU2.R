## To be run after MFSS_Precision.R

rm(list = keep(dir_path, start_date_input_train, end_date_input_train, start_date_input_test, end_date_input_test))
options(rstudio.help.showDataPreview = FALSE)
options(scipen =999)
setwd(dir_path)


model_names <- c("LASSO","SVR1","SVR2","SVR3","SVR4","DT1","DT2","Random Forest","Bagging","GBM1","GBM2","LSTM","GRU","Ensemble")
MAAPE_WS = MAAPE_OOS = MSE2_WS = MSE2_OOS = MAE_WS = MAE_OOS <- matrix(NA, 14, 12)# nrow = 14 because of 13 ML models and 1 Ensembled model


for(ML in 1:length(model_names)){
  setwd(paste(dir_path, "/Model/", model_names[ML], sep = ""))
  print(paste("Postprocessing MSE for algorithm: " ,model_names[ML], sep = ""  ))

  #################################################################################################################################
  
  MSE2_train <- read.csv(paste("MSE2_",model_names[ML],"_Matrix.csv", sep = "")) 
  MSE2_test <- read.csv(paste("MSE2_",model_names[ML],"_Matrix_OOS.csv", sep = ""))
  MSE2_train <- MSE2_train[,-1]
  MSE2_test <- MSE2_test[,-1]
  MSE2_WS[ML, ] <- rowMeans(MSE2_train)
  MSE2_OOS[ML, ] <- rowMeans(MSE2_test)
  #################################################################################################################################
  MAE_train <- read.csv(paste("MAE_",model_names[ML],"_Matrix.csv", sep = ""))
  MAE_test <- read.csv(paste("MAE_",model_names[ML],"_Matrix_OOS.csv", sep = ""))
  MAE_train <- MAE_train[,-1]
  MAE_test <- MAE_test[,-1]
  MAE_WS[ML, ] <- rowMeans(MAE_train)
  MAE_OOS[ML, ] <- rowMeans(MAE_test)
  #################################################################################################################################
  MAAPE_train <- read.csv(paste("MAAPE_",model_names[ML],"_Matrix.csv", sep = ""))
  MAAPE_test <- read.csv(paste("MAAPE_",model_names[ML],"_Matrix_OOS.csv", sep = ""))
  MAAPE_train <- MAAPE_train[,-1]
  MAAPE_test <- MAAPE_test[,-1]
  MAAPE_WS[ML, ] <- rowMeans(MAAPE_train)
  MAAPE_OOS[ML, ] <- rowMeans(MAAPE_test)
  
}

RMSE2_WS <- sqrt(MSE2_WS)
RMSE2_OOS <- sqrt(MSE2_OOS)
colnames(MAAPE_WS) = colnames(MAAPE_OOS) = colnames(RMSE2_WS) = colnames(RMSE2_OOS) = colnames(MSE2_WS) = colnames(MSE2_OOS) <- c("W1", "W2", "W3", "W4", "W5", "W6", "W7", "W8", "W9", "W10", "W11", "W12")
Performance_Indicators <- list(RMSE2_WS, RMSE2_OOS, MAE_WS, MAE_OOS, MAAPE_WS, MAAPE_OOS)

setwd(dir_path)

save(Performance_Indicators,file =  "Performance_by_Regions_Precision.RData")
#####################################################################################################################################
load("Performance_by_Regions_Precision.RData")
names(Performance_Indicators) <- c("RMSE2_WS", "RMSE2_OOS", "MAE_WS", "MAE_OOS", "MAAPE_WS", "MAAPE_OOS")

write.csv(Performance_Indicators[["RMSE2_OOS"]], file = "RMSE_OOS_Precision.csv")
write.csv(Performance_Indicators[["RMSE2_WS"]], file = "RMSE_WS_Precision.csv")

write.csv(Performance_Indicators[["MAE_OOS"]], file = "MAE_OOS_Precision.csv")
write.csv(Performance_Indicators[["MAE_WS"]], file = "MAE_WS_Precision.csv")

write.csv(Performance_Indicators[["MAAPE_OOS"]], file = "MAAPE_OOS_Precision.csv")
write.csv(Performance_Indicators[["MAAPE_WS"]], file = "MAAPE_WS_Precision.csv")

setwd(paste(dir_path,"/Model", sep = ""))

