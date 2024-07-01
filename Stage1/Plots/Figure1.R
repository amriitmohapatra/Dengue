rm(list = keep(dir_path, start_date_input_train, end_date_input_train, start_date_input_test, end_date_input_test))
setwd(paste(dir_path,"/","Model", sep = ""))
library(xts)
library(dygraphs)
library(ggplot2)
library(lubridate)
library(grid)
library(gridExtra)

##########

n_regions = 1
n_models = 12
conf_level = 0.05
n_lags = 20

coverage = c()
mean_coverage = c()
MAPE = c()
MAPE_matrix = matrix(NA, 52, n_regions)

load("Predictions Ensemble.RData")
Predictions_Ensemble <- Predictions_Ensemble_Results[1, , ]
dataset_total = read.csv("Dengue Weekly Total.csv")


sd_residuals = matrix(NA, nrow = n_models ,ncol = n_regions)

get_week_number <- function(input_date, year) {
  # Create a date object with the specified year
  target_date <- make_date(year, month(input_date), day(input_date))
  
  # Use the week() function to get the week number
  week_number <- week(target_date)
  
  return(week_number)
}
train_days <- nrow(Predictions_Ensemble)
actual_data_begin_index <- n_lags + 2
actual_data_end_index <- actual_data_begin_index - 1 + train_days

for (regions in 1:n_regions){
  for (weeks in 1:n_models) {
    predicted = unname(Predictions_Ensemble[,weeks])
    actual = dataset_total[actual_data_begin_index:actual_data_end_index, 2]
    residuals = actual - predicted
    sd_residuals[weeks,regions]=sd(residuals)
  }
}

rm(Predictions_Ensemble)

#############################################################################################################
## I need a 2 x 2 set of 4 plots. Fix the timeframe from W1 of 2019 to W52 of 2020.
## Positioning of the smaller plots matters.
load("Predictions Ensemble OOS.RData")
Predictions_Ensemble <- Predictions_Ensemble_Results[1, , ]
Predictions_Ensemble_LB <- Predictions_Ensemble_Results[2, , ]
Predictions_Ensemble_UB <- Predictions_Ensemble_Results[3, , ]

panel_plotter = function(weeks, row, column, plot_num){
  
  
  rows = weeks
  total_vec <- 104
  l_vec <- ((actual_data_end_index - 51) + 51 + rows)
  
  lower_bound1 <- vector()
  upper_bound1 <- vector()
  lower_bound2 <- vector()
  upper_bound2 <- vector()
  actual_data <- dataset_total[(actual_data_end_index - 51):l_vec, 2] # rest to be NAs, to be shown as solid black line
  lad1 <- length(actual_data)
  actual_data_observed <- dataset_total[l_vec: (l_vec + 11), 2] # To be shown as dots
  predicted_data <- unname(Predictions_Ensemble[rows,])
  predicted_data[predicted_data < 0] <- 0 
  ## Make the vectors equal in length
  
  actual_data <- c(actual_data, rep(NA, (total_vec - lad1)))
  actual_data_observed <- c(rep(NA,lad1),actual_data_observed, rep(NA,total_vec - lad1 - n_models))
  
  lower_bound1 <- unname(Predictions_Ensemble_LB[rows, ])
  upper_bound1 <- unname(Predictions_Ensemble_UB[rows, ])
  lower_bound1[lower_bound1 < 0] <- 0
  upper_bound1[upper_bound1 < 0] <- 0
  
  z_value = qnorm(1 - (conf_level / 2 ))
  
  lower_bound2 <- predicted_data - z_value * sd_residuals[,regions]
  upper_bound2 <- predicted_data + z_value * sd_residuals[,regions]
  lower_bound2[lower_bound2 < 0] <- 0
  upper_bound2[upper_bound2 < 0] <- 0 
  
  predicted_data <- c(rep(NA,lad1),predicted_data, rep(NA,total_vec - lad1 - n_models))
  lower_bound1 <- c(rep(NA,lad1),lower_bound1, rep(NA,total_vec - lad1 - n_models))
  upper_bound1 <- c(rep(NA,lad1),upper_bound1, rep(NA,total_vec - lad1 - n_models))
  lower_bound1[lad1] <- actual_data[lad1]
  upper_bound1[lad1] <- actual_data[lad1]
  
  lower_bound2 <- c(rep(NA,lad1),lower_bound2, rep(NA,total_vec - lad1 - n_models))
  upper_bound2 <- c(rep(NA,lad1),upper_bound2, rep(NA,total_vec - lad1 - n_models))
  lower_bound2[lad1] <- actual_data[lad1]
  upper_bound2[lad1] <- actual_data[lad1]
  
  
  date_strings <- dataset_total[(actual_data_end_index - 52):(actual_data_end_index + 51), 9]
  dates <- dmy(date_strings)  # Assuming the format is day/month/year
  
  df <- data.frame(
    x = dates,
    actual = actual_data,
    obs = actual_data_observed,
    predicted = predicted_data,
    lower_bound1 = lower_bound1,
    upper_bound1 = upper_bound1,
    lower_bound2 = lower_bound2,
    upper_bound2 = upper_bound2
  ) 
  year2 <- substr(dates, 1, 4)
  
  unique_months <- unique(format(df$x, "%Y-%m"))
  earliest_positions <- numeric(length(unique_months))
  
  # Iterate through each unique month and find the position of the earliest data point
  for (i in 1:length(unique_months)) {
    current_month <- unique_months[i]
    
    # Extract the year and month from the current_month
    year <- substr(current_month, 1, 4)
    month <- substr(current_month, 6, 7)
    
    # Create a reference date for the beginning of the month
    reference_date <- as.Date(paste(year, month, "01", sep = "-"))
    
    # Find the position of the earliest data point in this subset
    earliest_positions[i] <- which(df$x >= reference_date)[1]
  }
  
  
  
  month_labels <- substr(format(seq(as.Date(df$x[1]), as.Date(df$x[length(df$x)]), by = "1 month"), "%b"),1,1)
  mid_points <- (earliest_positions[-1] + earliest_positions[-length(earliest_positions)])/2
  
  y_scaler = max(Predictions_Ensemble) + 100
  
  ##########################################################################################################
  pushViewport(viewport(layout.pos.col=column,layout.pos.row=row))
  if(row == 1){
    pushViewport(plotViewport(c(1, 2, 1, 0), xscale = c(0,length(df$actual)+1), yscale = c(0,y_scaler )))
  }else{
    pushViewport(plotViewport(c(2, 2, 0, 0), xscale = c(0,length(df$actual)+1), yscale = c(0,y_scaler )))
  }
  
  ##########################################################################################################
  grid.rect()
  x <- seq(1, length(df$actual), 1)
  grid.lines(x, df$actual, default.units = 'native', gp = gpar(col = "black", lwd = 0.7)) 
  grid.lines(x, df$predicted, default.units = 'native', gp = gpar(col = '#d62976', lwd = 0.7))
  
  grid.polygon(c(x[(lad1+1):(lad1 + n_models)], rev(x[(lad1+1):(lad1 + n_models)])), c(df$lower_bound1[(lad1+1):(lad1 + n_models)], rev(df$upper_bound1[(lad1+1):(lad1 + n_models)])), default.units = 'native', gp = gpar(col = NA, fill = '#d36060', alpha = 0.5))
  grid.polygon(c(x[(lad1):(lad1 + 1)], rev(x[(lad1):(lad1 + 1)])), c(df$lower_bound1[(lad1):(lad1 + 1)], rev(df$upper_bound1[(lad1):(lad1 + 1)])), default.units = 'native', gp = gpar(col = NA, fill = '#d36060', alpha = 0.5))
  grid.polygon(c(x[(lad1+1):(lad1 + n_models)], rev(x[(lad1+1):(lad1 + n_models)])), c(df$lower_bound2[(lad1+1):(lad1 + n_models)], rev(df$upper_bound2[(lad1+1):(lad1 + n_models)])), default.units = 'native', gp = gpar(col = NA, fill = '#ff6289', alpha = 0.5))
  grid.polygon(c(x[(lad1):(lad1 + 1)], rev(x[(lad1):(lad1 + 1)])), c(df$lower_bound2[(lad1):(lad1 + 1)], rev(df$upper_bound2[(lad1):(lad1 + 1)])), default.units = 'native', gp = gpar(col = NA, fill = '#ff6289', alpha = 0.5))

  grid.segments(x0 = x[lad1], x1 = x[lad1 + 1], y0 = df$actual[lad1], y1 = df$predicted[lad1 + 1], default.units = 'native', gp = gpar(col = "#ff6289", lwd = 0.7))
  grid.points(x, df$obs, gp = gpar(col = 'black', cex = 0.1 , lwd = 2), pch = 20)
  grid.yaxis(gp = gpar(fontsize = 8))
  multiples <- seq(1, length(x)/2, by = 5)
  multiples <- c(multiples, 52 + multiples)
  # Add x-axis labels 
  if(column == 1){
    grid.text('Dengue Cases (/w)', x = unit(-2.8, 'lines'), rot = 90, gp = gpar(fontsize = 9, fontface = "bold")) # Only for (1,1) and (2,1)
  }
  mid_points <- c(mid_points, 103)
  
  if(row == 2){
    grid.xaxis(at = earliest_positions, label = FALSE, gp = gpar(fontsize = 7)) # Only for (2,1) and (2,2)
    # grid.text(month_labels, x = unit(mid_points, 'native'), y = unit(-0.05, "npc"), gp = gpar(fontsize = 5))
    grid.xaxis(at = earliest_positions[13], label = FALSE, gp = gpar(fontsize = 15))
  }
  
  # grid.xaxis(at=pretty(x, n = 24), gp = gpar(fontsize = 3))
  # grid.text(paste("(",letters[plot_num], ") ", "Week ", rows,sep = ""), x = 0.15, y = 0.90, gp = gpar(fontsize = 9, fontface = "bold"))
  grid.text(paste(letters[plot_num], sep = ""), x = 0.05, y = 0.90, gp = gpar(fontsize = 9, fontface = "bold"))
  if(column == 2 & row == 2){
    grid.text(paste("Week ", rows,sep = ""), x = 0.20, y = 0.90, gp = gpar(fontsize = 9))
  }else{
    grid.text(paste("Week ", rows,sep = ""), x = 0.18, y = 0.90, gp = gpar(fontsize = 9))
  }
  
  
  
  if(column == 1 && row == 1){
    pushViewport(viewport(width = 0.1, height = 0.5, x = 0.875, y = 0.8, just = c("right", "top")))
    grid.legend(
      labels = c('Actual', 'Forecasted', 'CI band 1', 'CI band 2'),
      nrow = 4,
      ncol = 1,
      do.lines = TRUE,
      pch = c(20, NA, NA, NA),  # 'Actual Cases' marked as a dot
      hgap = unit(0.2, "npc"),
      vgap = unit(0.2, "npc"),
      gp = gpar(
        col = c('black', '#d62976', '#d36060','#ff6289'),
        lwd = c(NA, 1, 3, 3),
        fontsize = 9
      ),
      draw = TRUE
    )
    popViewport()
  }

  
  
  # A conditional statement to check if year changes, if yes, then find position of year change and mention the new year just below W1 of new year. Old year mentioned only once below W52.
  # Add a partition (a dashed vertical line) at exactly end of old year.
  if(year2[1] == year2[length(year2)]){
    if(row == 2){
    grid.text(label = as.character(year2[1]), x = unit(9, "native"), y = unit(-3, "lines"),just = "center", gp = gpar(fontsize = 9, fontface = "bold"))
    }
  }else{
    change_pos = which(year2 == year2[length(year2)])[1] # Find out where the next year appears first.
    if(row == 2){
    grid.text(label = as.character(year2[1]), x = unit((change_pos-25.5), "native"), y = unit(-2, "lines"),just = "center", gp = gpar(fontsize = 9, fontface = "bold"))
    grid.text(label = as.character(year2[length(year2)]), x = unit(change_pos + 25.5, "native"), y = unit(-2, "lines"),just = "center", gp = gpar(fontsize = 9, fontface = "bold"))
    }
    grid.lines( x = unit((change_pos - 0.5), "native"), gp = gpar(lty = "dashed", col = "brown", lwd = 0.5))
  }
  
  popViewport()
  popViewport()
}

setwd(paste(dir_path,"/","Plots", sep = ""))

png(file = 'Stage1Forecast.png',height=8,width=15,units='cm',res=300,pointsize=10)
pushViewport(plotViewport(c(1,1,0,0.5)))
pushViewport(viewport(layout=grid.layout(nrow=2,ncol=2)))
panel_plotter(1,1,1,1)
panel_plotter(4,1,2,2)
panel_plotter(8,2,1,3)
panel_plotter(12,2,2,4)
popViewport()
popViewport()
dev.off()

setwd(paste(dir_path,"/","Plots", sep = ""))