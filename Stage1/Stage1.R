rm(list = ls())
options(rstudio.help.showDataPreview = FALSE)
options(scipen =999)
library(gdata)

# Set User Directory
dir_path = readline(prompt = "Please enter the directory path to set as the working directory: ")


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

start_date_input_train <- readline(prompt = "Please enter starting date for training set (YYYY-MM-DD): ")
end_date_input_train <- readline(prompt = "Please enter end date for training set (YYYY-MM-DD): ")

start_date_input_test <- readline(prompt = "Please enter starting date for test set (YYYY-MM-DD): ")
end_date_input_test <- readline(prompt = "Please enter end date for test set (YYYY-MM-DD): ")
############################
print("Working on Stage 1.")
setwd(paste(dir_path,"/","Model", sep = ""))
print("Executing Training Set Individual forecast models for upto 12 weeks for all machine learning models.")
source("Within Sample Predictions.R")
print("Executing Test Set Individual forecast models for upto 12 weeks for all machine learning models.")
source("Out of Sample Predictions.R")
print("Executing Bayesian Combination for the Training Set.")
source("Ensembling Predictions_byweek.R")
print("Executing Bayesian Combination for the Test Set.")
source("Ensembling Predictions_byweek_OOS.R")
print("Calculating Performance Metrics for Stage1.")
source("SummaryStatsStage1.R")


### Before running this part, relevant files need to be copied and pasted elsewhere.
print("Plotting Stage 1's results and plots.")
setwd(paste(dir_path,"/","Plots", sep = ""))
source("Figure1.R") # All OK
source("Figure1_Supplementary.R") # All OK
source("Figure2.R")# All OK
source("Figure2_Supplementary.R") # Panel k too close to Week 11











