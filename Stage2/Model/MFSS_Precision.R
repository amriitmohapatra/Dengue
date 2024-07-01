# A Master file that calculates summary statistics for both within sample and out of sample and stores them in multiple folders.

rm(list = keep(dir_path, start_date_input_train, end_date_input_train, start_date_input_test, end_date_input_test))
options(rstudio.help.showDataPreview = FALSE)
options(scipen =999)
setwd(dir_path)


model_names <- c("LASSO","SVR1","SVR2","SVR3","SVR4","DT1","DT2","Random Forest","Bagging","GBM1","GBM2","LSTM","GRU","Ensemble")


for(ML in 1:length(model_names)){
  
  setwd(paste(dir_path, "/Model/", model_names[ML], sep = ""))
  print(paste("Calculating performance indicators for algorithm: " ,model_names[ML],"....... Within Sample" ,sep = ""  ))
  # source("Stage 2_34R.R")
  
  library(xts)
  library(dygraphs)
  library(ggplot2)
  library(lubridate)
  library(dplyr)
  library(grid)
  library(gridExtra)
  
  n_regions = 34
  n_models = 12
  conf_level = 0.05
  n_lags = 20
  
  coverage = c()
  mean_coverage = c()
  MAPE = c()
  MSE = c()
  load("Final_Predictions.RData")
  MAPE_matrix = matrix(NA, nrow(regions_forecast[[1]]), n_regions)
  MSE_matrix = matrix(NA, nrow(regions_forecast[[1]]), n_regions)
  
  dataset_total = read.csv("case data.csv")
  
  region_names <- colnames(dataset_total)[4:ncol(dataset_total)]
  train_days <-nrow(regions_forecast[[1]])
  sd_residuals = matrix(NA, nrow = n_models ,ncol = n_regions)
  
  actual_data_begin_index <- n_lags + 2
  actual_data_end_index <- actual_data_begin_index - 1 + train_days
  get_week_number <- function(input_date, year) {
    # Create a date object with the specified year
    target_date <- make_date(year, month(input_date), day(input_date))
    
    # Use the week() function to get the week number
    week_number <- week(target_date)
    
    return(week_number)
  }
  
  
  pdf(paste(model_names[ML]," Forecasted Dengue Cases for All Regions_New_Floored.pdf", sep = ""), width = 17, height = 9)
  for (regions in 1:n_regions){
    for (weeks in 1:n_models) {
      predicted = unname(regions_forecast[[regions]][,weeks])
      actual = dataset_total[actual_data_begin_index:actual_data_end_index, regions + 3]
      residuals = actual - predicted
      sd_residuals[weeks,regions]=sd(residuals)
    }
  }
  
  aape_array = dev_sq_array = dev_abs_array <- array(NA, dim = c(train_days, n_models, n_regions))
  
  for(regions in 1:n_regions){
    print(paste("Plotting Region : ", region_names[regions], sep = ""))
    for(rows in 1:nrow(regions_forecast[[regions]])){
      
      lower_bound <- vector()
      upper_bound <- vector()
      actual_data <- dataset_total[(n_lags + 1 + rows):(n_lags + 1 + 11 + rows), regions + 3]
      predicted_data <- unname(regions_forecast[[regions]][rows,])
      predicted_data[predicted_data < 0] <- 0 
      predicted_data[is.na(predicted_data)] <- 0 # NaNs Uniquely observed only for LSTM. So strange!!!!
      
      z_value = qnorm(1 - (conf_level / 2 ))
      
      lower_bound <- predicted_data - z_value * sd_residuals[,regions]
      upper_bound <- predicted_data + z_value * sd_residuals[,regions]
      lower_bound[lower_bound < 0] <- 0
      upper_bound[upper_bound < 0] <- 0 
      
      
      
      date_strings <- dataset_total[(n_lags + 1 + rows):(n_lags + 1 + 11 + rows), 2]
      dates <- dmy(date_strings)  # Assuming the format is day/month/year
      
      
      previous_weeks <- 6
      previous_dates <- dataset_total[(n_lags + 1 - previous_weeks + rows) : (n_lags + 1 + 5 - previous_weeks + rows), 2]
      previous_actual_data <- dataset_total[(n_lags + 1 - previous_weeks + rows):(n_lags + 1 + 5 - previous_weeks + rows), regions+3]
      
      dates <- c(dmy(previous_dates), dates)
      actual_data <- c(previous_actual_data, actual_data)
      
      
      df <- data.frame(
        x = dates,
        actual = actual_data,
        predicted = c(rep(NA, previous_weeks), predicted_data),
        lower_bound = c(rep(NA, previous_weeks), lower_bound),
        upper_bound = c(rep(NA, previous_weeks), upper_bound)
      ) 
      
      
      
      year <- substr(dates, 1, 4)
      
      counter = 0
      APE = c()
      dev_sq = c()
      dev_abs = c()
      for(c in 1:12){
        if(df$actual[7:18][c] == 0){
          APE[c] = 0
        }else{
          APE[c] = abs(df$actual[7:18][c] - df$predicted[7:18][c])/ df$actual[7:18][c]
        }
        
        if(df$actual[7:18][c] >= df$lower_bound[7:18][c] & df$actual[7:18][c] <= df$upper_bound[7:18][c]){
          counter = counter + 1
        }
        dev_sq[c] = (df$actual[7:18][c] - df$predicted[7:18][c])^2
        dev_sq_array[rows, c, regions] = dev_sq[c]
        dev_abs[c] = abs(df$actual[7:18][c] - df$predicted[7:18][c])
        dev_abs_array[rows, c, regions] = dev_abs[c]
        aape_array[rows, c, regions] = atan((abs(df$actual[7:18][c] - df$predicted[7:18][c])/ (0.000001 + df$actual[7:18][c])))
      }
      
      
      
      coverage[rows] = counter/12
      MAPE[rows] = mean(APE)
      MSE[rows] = sum(dev_sq)/(12-1)
      
      y_scaler = max(regions_forecast[[regions]])
      # grid.newpage()
      # 
      # pushViewport(plotViewport(c(6.5,7.5,3,3), xscale = c(0,length(df$actual)+1), yscale = c(0,y_scaler + 50)))
      # grid.rect()
      # x <- seq(1, length(df$actual), 1)
      # grid.points(x, df$actual, gp = gpar(col = 'black', cex = 0.5 , lwd = 7), pch = 20)
      # grid.lines(x[7:18], df$predicted[7:18], default.units = 'native', gp = gpar(col = '#d62976', lwd = 3))
      # 
      # grid.polygon(c(x[7:18], rev(x[7:18])), c(df$lower_bound[7:18], rev(df$upper_bound[7:18])), default.units = 'native', gp = gpar(col = NA, fill = '#ff6289', alpha = 0.3))
      # grid.segments(x0 = x[6], x1 = x[7], y0 = df$actual[6], y1 = df$lower_bound[7], default.units = 'native', gp = gpar(col = "black", lty = "dashed", lwd = 3))
      # grid.segments(x0 = x[6], x1 = x[7], y0 = df$actual[6], y1 = df$upper_bound[7], default.units = 'native', gp = gpar(col = "black", lty = "dashed", lwd = 3))
      # grid.segments(x0 = x[6], x1 = x[7], y0 = df$actual[6], y1 = df$predicted[7], default.units = 'native', gp = gpar(col = "black", lty = "dashed", lwd = 3))
      # grid.yaxis(gp = gpar(fontsize = 18))
      # # Add x-axis labels for the earliest data point of each month
      # grid.text(paste('Region:',region_names[regions]), y = 1.045 ,just = "center",rot = 0, gp = gpar(fontsize = 20))
      # grid.text('Dengue Cases', x = unit(-3, 'lines'), rot = 90, gp = gpar(fontsize = 20))
      # grid.xaxis(at=x,label=as.character(paste('W',get_week_number(dates, year), sep = "")), gp = gpar(fontsize = 16))

      ## A conditional statement to check if year changes, if yes, then find position of year change and mention the new year just below W1 of new year. Old year mentioned only once below W52.
      ## Add a partition (a dashed vertical line) at exactly end of old year.
      # if(year[1] == year[length(year)]){
      # 
      #   grid.text(label = as.character(year[1]), x = unit(9, "native"), y = unit(-3, "lines"),just = "center", gp = gpar(fontsize = 18))
      # 
      # }else{
      #   change_pos = which(year == year[length(year)])[1] # Find out where the next year appears first.
      # 
      #   grid.text(label = as.character(year[1]), x = unit((change_pos-1), "native"), y = unit(-3, "lines"),just = "center", gp = gpar(fontsize = 18))
      #   grid.text(label = as.character(year[length(year)]), x = unit(change_pos, "native"), y = unit(-3, "lines"),just = "center", gp = gpar(fontsize = 18))
      #   grid.lines( x = unit((change_pos - 0.5), "native"), gp = gpar(lty = "dashed", col = "brown"))
      # 
      # }

      # pushViewport(viewport(width = 0.32, height = 0.195, x = 1, y = 1, just = c("right", "top")))


      ## Create a legend
      # grid.legend(
      #   labels = c('Actual Cases', 'Forecasted Cases', 'Confidence Intervals'),
      #   nrow = 3,
      #   ncol = 1,
      #   do.lines = TRUE,
      #   pch = c(20, NA, NA),  # 'Actual Cases' marked as a dot
      #   hgap = unit(1.2, "lines"),
      #   vgap = unit(1.2, "lines"),
      #   gp = gpar(
      #     col = c('black', '#d62976', '#ff6289'),
      #     lwd = c(NA, 3.0, 10.0),
      #     fontsize = 15
      #   ),
      #   draw = TRUE
      # )
      # popViewport()
      # 
      # 
      # 
      # popViewport()
      
      
      
    }
    
    MAPE_matrix[,regions] = MAPE
    mean_coverage[regions] = mean(coverage)
    MSE_matrix[,regions] = MSE
  }
  dev.off()
  
  MSE2 <- apply(dev_sq_array, c(2,3), mean)
  MAE <- apply(dev_abs_array, c(2,3), mean)
  MAAPE <- apply(aape_array, c(2,3), mean)
  
  write.csv(MSE2, paste("MSE2_", model_names[ML], "_Matrix.csv", sep = ""), sep = ",", col.names = TRUE)
  write.csv(MAE, paste("MAE_", model_names[ML], "_Matrix.csv", sep = ""), sep = ",", col.names = TRUE)
  write.csv(MAAPE, paste("MAAPE_", model_names[ML], "_Matrix.csv", sep = ""), sep = ",", col.names = TRUE)

  ##############################################################################################################################
  print(paste("Calculating performance indicators for algorithm: " ,model_names[ML],"....... Out of Sample" ,sep = ""  ))

  
  library(xts)
  library(dygraphs)
  library(ggplot2)
  library(lubridate)
  library(dplyr)
  library(grid)
  library(gridExtra)
  
  n_regions = 34
  n_models = 12
  conf_level = 0.05
  n_lags = 20
  
  coverage = c()
  mean_coverage = c()
  MAPE = c()
  MSE = c()
  
  load("Final_Predictions.RData")
  dataset_total = read.csv("case data.csv")
  
  region_names <- colnames(dataset_total)[4:ncol(dataset_total)]
  train_days <-nrow(regions_forecast[[1]])
  
  sd_residuals = matrix(NA, nrow = n_models ,ncol = n_regions)
  
  actual_data_begin_index <- n_lags + 2
  actual_data_end_index <- actual_data_begin_index - 1 + train_days
  get_week_number <- function(input_date, year) {
    # Create a date object with the specified year
    target_date <- make_date(year, month(input_date), day(input_date))
    
    # Use the week() function to get the week number
    week_number <- week(target_date)
    
    return(week_number)
  }
  
  
  
  for (regions in 1:n_regions){
    for (weeks in 1:n_models) {
      predicted = unname(regions_forecast[[regions]][,weeks])
      actual = dataset_total[actual_data_begin_index:actual_data_end_index, regions + 3]
      residuals = actual - predicted
      sd_residuals[weeks,regions]=sd(residuals)
    }
  }
  rm(regions_forecast)
  ###########################################
  load("Final_Predictions_OOS.RData")
  test_days <- nrow(regions_forecast[[1]])
  MAPE_matrix = matrix(NA, test_days, n_regions)
  MSE_matrix = matrix(NA, test_days, n_regions)
  aape_array = dev_sq_array = dev_abs_array <- array(NA, dim = c(test_days, n_models, n_regions))

  # pdf(paste(model_names[ML]," Forecasted Dengue Cases for All Regions_New_Floored_OOS.pdf", sep = ""), width = 17, height = 9)
  for(regions in 1:n_regions){
    print(paste("Plotting Region : ", region_names[regions], sep = ""))
    for(rows in 1:nrow(regions_forecast[[regions]])){
      
      lower_bound <- vector()
      upper_bound <- vector()
      actual_data <- dataset_total[(actual_data_end_index + 1 + rows):(actual_data_end_index + 1 + 11 + rows), regions + 3]
      predicted_data <- unname(regions_forecast[[regions]][rows,])
      predicted_data[predicted_data < 0] <- 0 
      predicted_data[is.na(predicted_data)] <- 0 # NaNs Uniquely observed only for LSTM. So strange!!!!
      
      z_value = qnorm(1 - (conf_level / 2 ))
      
      lower_bound <- predicted_data - z_value * sd_residuals[,regions]
      upper_bound <- predicted_data + z_value * sd_residuals[,regions]
      lower_bound[lower_bound < 0] <- 0
      upper_bound[upper_bound < 0] <- 0 
      
      
      
      date_strings <- dataset_total[(actual_data_end_index + 1 + rows):(actual_data_end_index + 1 + 11 + rows), 2]
      dates <- dmy(date_strings)  # Assuming the format is day/month/year
      
      
      previous_weeks <- 6
      previous_dates <- dataset_total[((actual_data_end_index + 1) - previous_weeks + rows) : ((actual_data_end_index + 6) - previous_weeks + rows), 2]
      previous_actual_data <- dataset_total[((actual_data_end_index + 1) - previous_weeks + rows):((actual_data_end_index + 6) - previous_weeks + rows), regions+3]
      
      dates <- c(dmy(previous_dates), dates)
      actual_data <- c(previous_actual_data, actual_data)
      
      
      df <- data.frame(
        x = dates,
        actual = actual_data,
        predicted = c(rep(NA, previous_weeks), predicted_data),
        lower_bound = c(rep(NA, previous_weeks), lower_bound),
        upper_bound = c(rep(NA, previous_weeks), upper_bound)
      ) 
      
      
      
      year <- substr(dates, 1, 4)
      
      #MAPE
      counter = 0
      APE = c()
      dev_sq = c()
      dev_abs = c()
      for(c in 1:12){
        if(df$actual[7:18][c] == 0){
          APE[c] = 0
        }else{
          APE[c] = abs(df$actual[7:18][c] - df$predicted[7:18][c])/ df$actual[7:18][c]
        }
        
        if(df$actual[7:18][c] >= df$lower_bound[7:18][c] & df$actual[7:18][c] <= df$upper_bound[7:18][c]){
          counter = counter + 1
        }
        dev_sq[c] = (df$actual[7:18][c] - df$predicted[7:18][c])^2
        dev_sq_array[rows, c, regions] = dev_sq[c]
        dev_abs[c] = abs(df$actual[7:18][c] - df$predicted[7:18][c])
        dev_abs_array[rows, c, regions] = dev_abs[c]
        aape_array[rows, c, regions] = atan(abs((df$actual[7:18][c] - df$predicted[7:18][c])/ (0.000001 + df$actual[7:18][c])))
      }
      
      
      
      coverage[rows] = counter/12
      MAPE[rows] = mean(APE)
      MSE[rows] = sum(dev_sq)/(12-1)
      y_scaler = max(regions_forecast[[regions]], na.rm = TRUE)
      # grid.newpage()
    
      # pushViewport(plotViewport(c(6.5,7.5,3,3), xscale = c(0,length(df$actual)+1), yscale = c(0,y_scaler + 50)))
      # grid.rect()
      # x <- seq(1, length(df$actual), 1)
      # grid.points(x, df$actual, gp = gpar(col = 'black', cex = 0.5 , lwd = 7), pch = 20)
      # grid.lines(x[7:18], df$predicted[7:18], default.units = 'native', gp = gpar(col = '#d62976', lwd = 3))
      # 
      # grid.polygon(c(x[7:18], rev(x[7:18])), c(df$lower_bound[7:18], rev(df$upper_bound[7:18])), default.units = 'native', gp = gpar(col = NA, fill = '#ff6289', alpha = 0.3))
      # grid.segments(x0 = x[6], x1 = x[7], y0 = df$actual[6], y1 = df$lower_bound[7], default.units = 'native', gp = gpar(col = "black", lty = "dashed", lwd = 3))
      # grid.segments(x0 = x[6], x1 = x[7], y0 = df$actual[6], y1 = df$upper_bound[7], default.units = 'native', gp = gpar(col = "black", lty = "dashed", lwd = 3))
      # grid.segments(x0 = x[6], x1 = x[7], y0 = df$actual[6], y1 = df$predicted[7], default.units = 'native', gp = gpar(col = "black", lty = "dashed", lwd = 3))
      # grid.yaxis(gp = gpar(fontsize = 18))
      # # Add x-axis labels for the earliest data point of each month
      # grid.text(paste('Region:',region_names[regions]), y = 1.045 ,just = "center",rot = 0, gp = gpar(fontsize = 20))
      # grid.text('Dengue Cases', x = unit(-3, 'lines'), rot = 90, gp = gpar(fontsize = 20))
      # grid.xaxis(at=x,label=as.character(paste('W',get_week_number(dates, year), sep = "")), gp = gpar(fontsize = 16))
      # 
      # # A conditional statement to check if year changes, if yes, then find position of year change and mention the new year just below W1 of new year. Old year mentioned only once below W52.
      # # Add a partition (a dashed vertical line) at exactly end of old year.
      # if(year[1] == year[length(year)]){
      # 
      #   grid.text(label = as.character(year[1]), x = unit(9, "native"), y = unit(-3, "lines"),just = "center", gp = gpar(fontsize = 18))
      # 
      # }else{
      #   change_pos = which(year == year[length(year)])[1] # Find out where the next year appears first.
      # 
      #   grid.text(label = as.character(year[1]), x = unit((change_pos-1), "native"), y = unit(-3, "lines"),just = "center", gp = gpar(fontsize = 18))
      #   grid.text(label = as.character(year[length(year)]), x = unit(change_pos, "native"), y = unit(-3, "lines"),just = "center", gp = gpar(fontsize = 18))
      #   grid.lines( x = unit((change_pos - 0.5), "native"), gp = gpar(lty = "dashed", col = "brown"))
      # 
      # }
      # 
      # pushViewport(viewport(width = 0.32, height = 0.195, x = 1, y = 1, just = c("right", "top")))
      # 
      # 
      # # Create a legend
      # grid.legend(
      #   labels = c('Actual Cases', 'Forecasted Cases', 'Confidence Intervals'),
      #   nrow = 3,
      #   ncol = 1,
      #   do.lines = TRUE,
      #   pch = c(20, NA, NA),  # 'Actual Cases' marked as a dot
      #   hgap = unit(1.2, "lines"),
      #   vgap = unit(1.2, "lines"),
      #   gp = gpar(
      #     col = c('black', '#d62976', '#ff6289'),
      #     lwd = c(NA, 3.0, 10.0),
      #     fontsize = 15
      #   ),
      #   draw = TRUE
      # )
      # popViewport()
      # 
      # 
      # 
      # popViewport()
      
    }
    
    MAPE_matrix[,regions] = MAPE
    mean_coverage[regions] = mean(coverage)
    MSE_matrix[,regions] = MSE
  }
  # dev.off()
  MSE2 <- apply(dev_sq_array, c(2,3), mean)
  MAE <- apply(dev_abs_array, c(2,3), mean)
  MAAPE <- apply(aape_array, c(2,3), mean)

  write.csv(MSE2, paste("MSE2_", model_names[ML], "_Matrix_OOS.csv", sep = ""), sep = ",", col.names = TRUE)
  write.csv(MAE, paste("MAE_", model_names[ML], "_Matrix_OOS.csv", sep = ""), sep = ",", col.names = TRUE)
  write.csv(MAAPE, paste("MAAPE_", model_names[ML], "_Matrix_OOS.csv", sep = ""), sep = ",", col.names = TRUE)

}

setwd(paste(dir_path,"/Model", sep = ""))
