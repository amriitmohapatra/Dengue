## Create folders and copy files first
rm(list = ls())
options(rstudio.help.showDataPreview = FALSE)
options(scipen =999)
library(gdata)

#Set directory to the folder where the results from Stage 1 are saved.
dir_path_s1 <- readline(prompt = "Please enter the directory path to the Stage 1 root folder: ")
dir_path_s1 <- gsub("\\\\", "/", dir_path_s1)
# Attempt to set the working directory to the user-provided path
tryCatch({
  setwd(dir_path_s1)
  cat("The working directory has been set to:", getwd(), "\n")
}, warning = function(w) {
  cat("Warning:", conditionMessage(w), "\n")
}, error = function(e) {
  cat("Error:", conditionMessage(e), "\n")
}, finally = {
  # Code to execute at the end, whether an error occurred or not
})

setwd(paste(dir_path_s1, "/Model",sep = ""))
load("All_Forecasts.RData")
load("All_Forecasts_OOS.RData")
load("Predictions Ensemble.RData")
Predictions_Ensemble_WS <- Predictions_Ensemble_Results
rm(Predictions_Ensemble_Results)
load("Predictions Ensemble OOS.RData")
Predictions_Ensemble_OOS <- Predictions_Ensemble_Results
rm(Predictions_Ensemble_Results)
# For folder named "Ensemble", Predictions_Ensemble.RData and Predictions_Ensemble.RData need to be copied and transferred.

# Set User Directory
dir_path = readline(prompt = "Please enter the directory path to set as the working directory for Stage 2: ")

dir_path <- gsub("\\\\", "/", dir_path)
# Attempt to set the working directory to the user-provided path
tryCatch({
  setwd(dir_path)
  cat("The working directory has been set to:", getwd(), "\n")
}, warning = function(w) {
  cat("Warning:", conditionMessage(w), "\n")
}, error = function(e) {
  cat("Error:", conditionMessage(e), "\n")
}, finally = {
  # Code to execute at the end, whether an error occurred or not
})



model_names <- c("LASSO","SVR1","SVR2","SVR3","SVR4","DT1","DT2","Random Forest","Bagging","GBM1","GBM2","LSTM","GRU","Ensemble")
copy1 <- read.csv("case data.csv")
copy2 <- read.csv("Dengue Weight Matrix.csv")

dir.create("Model")
setwd(paste(dir_path,"/","Model", sep = ""))

for(names in model_names){
  dir.create(names)
}

for(i in 1:length(model_names)){
  setwd(paste(dir_path,"/","Model/", model_names[i],sep = ""))
  if(model_names[i] == "Ensemble")
  {
    save(Predictions_Ensemble_WS, file = "Predictions Ensemble.RData")
    save(Predictions_Ensemble_OOS, file = "Predictions Ensemble OOS.RData")
  } else{
    save(All_Predictions_Master, file = "All_Forecasts.RData")
    save(All_Predictions, file = "All_Forecasts_OOS.RData")
  }
  write.csv(copy1, file = "case data.csv", row.names = FALSE)
  write.csv(copy2, file = "Dengue Weight Matrix.csv", row.names = FALSE)
}



start_date_input_train <- readline(prompt = "Please enter starting date for training set (YYYY-MM-DD): ")
end_date_input_train <- readline(prompt = "Please enter end date for training set (YYYY-MM-DD): ")

start_date_input_test <- readline(prompt = "Please enter starting date for test set (YYYY-MM-DD): ")
end_date_input_test <- readline(prompt = "Please enter end date for test set (YYYY-MM-DD): ")

setwd(paste(dir_path,"/Model", sep = ""))
print("Training Dirichlet Regression on all Machine Learning Algorithms using the training data.")
source("Stage 2_34R.R")
source("Stage 2_34R_Ensemble.R")
print("Fitiing Dirichlet Regression models on all Machine Learning Algorithms using the test set feature set data.")
source("Stage 2_34R_OOS.R")
source("Stage 2_34R_OOS_Ensemble.R")
print("Calculating performance metrics for all machine learning models and compiling them.")
source("MFSS_Precision.R")
source("ACU2.R")
print("Generating data for spatial-temporal plots with some preprocessing.")
source("Melter.R")
source("Melter_OOS.R")
source("modify.R")


setwd(paste(dir_path,"/Plots and Videos", sep = ""))
print("Making the accuracy plots for all week-ahead forecast models.")
source("Log10AcuPlot_v2.R")
source("Log10AcuPlot.R")
print("All processes completed successfully. Please run the rest of the plotting code in a Python IDE.")





































































