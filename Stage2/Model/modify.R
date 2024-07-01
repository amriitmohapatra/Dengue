rm(list = keep(dir_path, start_date_input_train, end_date_input_train, start_date_input_test, end_date_input_test, dir_path_2, dir_path_3))
library(readr)

# New column names
new_columns <- c("week number", "start date", "end date", "Bukit Batok", "Choa Chu Kang", "Tampines", "Yishun", 
                 "Ang Mo Kio", "Bedok", "Bishan", "Bukit Merah", "Bukit Panjang", "Bukit Timah", "Changi", 
                 "Clementi", "Geylang", "Hougang", "Jurong East", "Jurong West", "Kallang", "Mandai", 
                 "Marine Parade", "Newton", "Novena", "Outram", "Pasir Ris", "Punggol", "Queenstown", 
                 "River Valley", "Rochor", "Sembawang", "Sengkang", "Serangoon", "Sungei Kadut", "Tanglin", 
                 "Toa Payoh", "Woodlands")

# File patterns to loop through
file_patterns <- c("W_%d_Deviation_WithinSample.csv", "W_%d_Deviation_OutofSample.csv", "W_%d_True_OutofSample.csv", "W_%d_True_WithinSample.csv", "W_%d_Predictions_OutofSample.csv", "W_%d_Predictions_WithinSample.csv")

# Base directory for files
base_dir <- c(dir_path_2, dir_path_3)

for(dir in base_dir){
  # Loop through each file pattern
  for (pattern in file_patterns) {
    # Loop through each file index (1 to 12)
    for (i in 1:12) {
      # Build file path using sprintf to insert the file index and pattern
      file_path <- sprintf(paste(dir, pattern, sep="/"), i)
      
      # Read the CSV file, assuming the first row is data not column names
      df <- read.csv(file_path)
      
      # Change column names
      colnames(df) <- new_columns
      
      # Overwrite the original file
      write.csv(df, file_path, row.names = FALSE, quote = FALSE)
      
      cat(sprintf("File '%s' has been updated and saved.\n", file_path))
    }
  }
}


setwd(paste(dir_path,"/Model", sep = ""))
