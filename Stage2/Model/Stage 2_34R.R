rm(list = keep(dir_path, start_date_input_train, end_date_input_train, start_date_input_test, end_date_input_test))
library(dplyr)
library(DirichletReg)

final_predictions_presort <- list()
regression_coefficients <- list()
regression_models <- list()



model_names <- c("LASSO","SVR1","SVR2","SVR3","SVR4","DT1","DT2","Random Forest","Bagging","GBM1","GBM2","LSTM","GRU")
# ML = 1
for(ML in 1:length(model_names)){
  setwd(paste(dir_path,"/Model/", model_names[ML], sep = ""))
  print(paste("Executing Dirichlet Regression for algorithm: " ,model_names[ML],"....... Within Sample" ,sep = "" ))
  
  load("All_Forecasts.RData") ## Number decides model chosen
  Predictions_Individual <- All_Predictions_Master[[ML]]
  pred_days <- nrow(Predictions_Individual)
  weight_matrix = read.csv("Dengue Weight Matrix.csv", sep = ",", header = TRUE, row.names = 1)
  weight_matrix = as.matrix(weight_matrix)
  
  
  
  n_regions = 34
  n_models = 12
  n_lags = 20
  models_days <- seq(7, 7 * n_models, 7)
  ##################################################################################################################################
  
  
  # Filtering needs to be done for all 12 weeks' models
  ### Models' loops start from here
  # week_num = 12
  for(week_num in 1:n_models){
    print(paste("Dirichlet Regression for week-ahead:",week_num))
    dataset_allreg = read.csv("case data.csv")
    dataset_allreg$Date <- as.Date(dataset_allreg$end.date, format = "%d/%m/%Y")
    # Get the column indices of "North.Region" and "West.Region"
    start_index <- which(colnames(dataset_allreg) == "Bukit.Batok")
    end_index <- which(colnames(dataset_allreg) == "Woodlands")
    
    # Calculate the sum of the region columns
    dataset_allreg$Sum <- rowSums(dataset_allreg[, start_index:end_index])
    
    # Loop over the columns
    for (i in start_index:end_index) {
      # Calculate the proportion for each region and create a new column
      new_col_name <- paste0(colnames(dataset_allreg)[i], ".p.t1")
      dataset_allreg[[new_col_name]] <- dataset_allreg[, i] / dataset_allreg$Sum
    }
    model1 <- models_days[week_num]
    stage1_fit <- Predictions_Individual[,week_num]
    end_date <- as.Date(end_date_input_train)
    end_date_for_wts <- as.Date(end_date_input_train) + model1
    dataset_allreg_pre_wt <- dataset_allreg %>%
      filter(Date <= end_date_for_wts)
    dataset_allreg <- dataset_allreg %>%
      filter(Date <= end_date)
    
    model1_num <- week_num
    sub_df2 = dataset_allreg_pre_wt[-1:(-n_lags - model1_num),]
    sub_df2 = subset(sub_df2, select = -c(seq(1, 3 + n_regions + 2)))
    
    sub_df3 = dataset_allreg[-1:-n_lags,]
    sub_df3 = subset(sub_df3, select = -c(c(seq(1,3,1), seq(3 + n_regions + 1, ncol(sub_df3), 1))))
    
    YYY2 <- cbind(sub_df2, stage1_fit)
    
    filter1 = function(region_number){
      names = colnames(sub_df3)[1:ncol(sub_df3)]
      num_iterations = num_names = length(names)
      start_index = (region_number - 1) %% num_names + 1
      end_index = start_index + num_names - 1
      if (end_index > num_names) {
        sequence = c(names[start_index:num_names], names[1:(end_index %% num_names)])
      } else {
        sequence = names[start_index:end_index]
      }
    }
    col_chooser <- function(df){
      n <- ncol(df)  # Number of columns
      
      # Initialize an empty list to store the selected column subsets
      selected_subsets <- list()
      
      # Iterate from 1 to n
      for (i in 1:n) {
        # Create a vector of column names to select
        cols_to_select <- names(df)[-i]
        
        # Use the column names to select the corresponding columns
        selected_subsets[[i]] <- cols_to_select
      }
      return(selected_subsets)
    }
    
    ## Loop begins
    for(region_number in 1:n_regions){

      testtest <- as.matrix(sub_df3)
      
      region1_f = matrix(0, nrow = nrow(testtest), ncol = ncol(testtest))
      
      colnames(region1_f) = filter1(1)
      
      
      for(i in 1:nrow(region1_f)){
        region1_f[i,] = weight_matrix[region_number,] * testtest[i,]
        if(i == nrow(region1_f)){
          region1_f[,region_number] = testtest[,region_number]
        }
      }
      
      region1_f <- as.data.frame(region1_f)
      
      new_colname <- paste("X", region_number, sep = "")

      iter_cols <- col_chooser(region1_f)[[region_number]]
      region1_f <- region1_f %>% mutate(!!new_colname := rowSums(region1_f[,iter_cols])   ) # Correction needed!

      
      
      YYY2 <- cbind(YYY2, region1_f[,ncol(region1_f)])
      colnames(YYY2)[ncol(YYY2)] = new_colname
      
    }
    
    YYY2 <- cbind(YYY2, sub_df3)
    # Loop over the range
    for (i in 1:n_regions) {
      # Generate the new column name
      new_col_name <- paste0("C", i)
      
      # Rename the column
      colnames(YYY2)[(2*n_regions + 1) + i] <- new_col_name
    }
    
    
    YYY2$Y <- DR_data(YYY2[, 1:n_regions])
    #######################################################################################
    
    
    # Try executing the main function and catch any errors
    tryCatch({
      stage2_model <- DirichReg(Y ~ stage1_fit + C1 + X1 | stage1_fit + C2 + X2 | stage1_fit + C3 + X3 | stage1_fit + C4 + X4 | stage1_fit + C5 + X5 | stage1_fit + C6 + X6 | stage1_fit + C7 + X7 | stage1_fit + C8 + X8 | stage1_fit + C9 + X9 | stage1_fit + C10 + X10 |
                                  stage1_fit + C11 + X11 | stage1_fit + C12 + X12 | stage1_fit + C13 + X13 | stage1_fit + C14 + X14 | stage1_fit + C15 + X15 | stage1_fit + C16 + X16 | stage1_fit + C17 + X17 | stage1_fit + C18 + X18 | stage1_fit + C19 + X19 | stage1_fit + C20 + X20 | 
                                  stage1_fit + C21 + X21 | stage1_fit + C22 + X22 | stage1_fit + C23 + X23 | stage1_fit + C24 + X24 | stage1_fit + C25 + X25 | stage1_fit + C26 + X26 | stage1_fit + C27 + X27 | stage1_fit + C28 + X28 | stage1_fit + C29 + X29 | stage1_fit + C30 + X30 |
                                  stage1_fit + C31 + X31 | stage1_fit + C32 + X32 | stage1_fit + C33 + X33 | stage1_fit + C34 + X34
                                , data = YYY2, model = "common")
    }, error = function(e) {
      # If an error occurs, execute the alternate function
      final_predictions_presort[[week_num]] <- matrix(NA, pred_days, n_regions)
    })
    
    final_predictions_presort[[week_num]] <- fitted.values(stage2_model) * YYY2$stage1_fit
    regression_coefficients[[week_num]] <- stage2_model$coefficients
    regression_models[[week_num]] <- stage2_model
    
    
  }
  
  save(final_predictions_presort, file = "final predictions presort.RData")
  #####################################################################################
  load("final predictions presort.RData")
  
  # Initialize regions_forecast list
  regions_forecast <- vector("list", length = n_regions)
  
  # Loop over regions
  for (regions in 1:n_regions) {
    # Initialize a matrix for the current region
    region_matrix <- matrix(NA, nrow = pred_days, ncol = n_models)
    
    # Loop over weeks
    for (week in 1:n_models) {
      # Extract the data from final_prediction and assign it to the corresponding position in region_matrix
      region_matrix[, week] <- final_predictions_presort[[week]][, regions]
    }
    
    # Assign the region_matrix to regions_forecast
    regions_forecast[[regions]] <- region_matrix
  }
  
  save(regions_forecast, file = "Final_Predictions.RData")
  save(regression_coefficients, file = "Coefficients.RData")
  save(regression_models, file = "Models.RData")
  setwd(paste(dir_path,"/Model", sep = ""))
  ################################### Now I can proceed to plotting #########################
}





