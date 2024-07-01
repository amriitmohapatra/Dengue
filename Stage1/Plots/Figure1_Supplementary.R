rm(list = keep(dir_path, start_date_input_train, end_date_input_train, start_date_input_test, end_date_input_test))
setwd(paste(dir_path,"/","Model", sep = ""))
library(xts)
library(dygraphs)
library(ggplot2)
library(lubridate)
library(dplyr)
library(grid)
library(gridExtra)

n_regions = 1
n_models = 12
conf_level = 0.05
n_lags = 20

load("Predictions Ensemble.RData")
Predictions_Ensemble <- Predictions_Ensemble_Results[1, ,]
Predictions_Ensemble_Lower <- Predictions_Ensemble_Results[2, ,]
Predictions_Ensemble_Upper <- Predictions_Ensemble_Results[3, ,]

dataset_total = dataset_total1 = read.csv("Dengue Weekly Total.csv")
dataset_total$Date <- as.Date(dataset_total$Last.Date.of.this.week, format = "%d/%m/%Y")
all_dates <- dataset_total$Date
dataset_total <- subset(dataset_total, select = -c(1,9,10))
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
dataset_total <- dataset_total %>%
  filter(all_dates <= as.Date(end_date_input_train))

sub_df = dataset_total[-1:-n_lags, -length(dataset_total)]

sd_residuals = matrix(NA, nrow = n_models ,ncol = 1)

get_week_number <- function(input_date, year) {
  # Create a date object with the specified year
  target_date <- make_date(year, month(input_date), day(input_date))
  
  # Use the week() function to get the week number
  week_number <- week(target_date)
  
  return(week_number)
}


for (weeks in 1:n_models) {
  predicted = unname(Predictions_Ensemble[,weeks])
  actual = dataset_total[(n_lags + 1):((n_lags + 1) + nrow(sub_df) - 1), 1]
  residuals = actual - predicted
  sd_residuals[weeks,1]=sd(residuals)
}
setwd(paste(dir_path,"/","Plots", sep = ""))
dir.create("WSPlots")

# pdf("Ensembled Forecasted Dengue Cases for Singapore_WS.pdf", width = 17, height = 9)

for(rows in 1:nrow(Predictions_Ensemble)){
  print(paste("Plotting WS Plot: ",rows ,sep = ""))
  setwd(paste(dir_path,"/","Plots/WSPlots", sep = ""))
  png(file = paste('WS',rows,'.png', sep = ""),height=24,width=40,units='cm',res=300,pointsize=10)
  lower_bound1 <- vector()
  upper_bound1 <- vector()
  lower_bound2 <- vector()
  upper_bound2 <- vector()
  actual_data <- dataset_total1[(21+rows):(32+rows), 2]
  predicted_data <- unname(Predictions_Ensemble[rows,])
  predicted_data[predicted_data < 0] <- 0 
  
  z_value = qnorm(1 - (conf_level / 2 ))
  
  lower_bound1 <- predicted_data - z_value * sd_residuals[,1]
  upper_bound1 <- predicted_data + z_value * sd_residuals[,1]
  lower_bound1[lower_bound1 < 0] <- 0
  upper_bound1[upper_bound1 < 0] <- 0 
  
  lower_bound2 <- unname(Predictions_Ensemble_Lower[rows, ])
  upper_bound2 <- unname(Predictions_Ensemble_Upper[rows, ])
  
  date_strings <- dataset_total1[(21 + rows):(32 + rows), 9]
  dates <- dmy(date_strings)  # Assuming the format is day/month/year
  
  previous_weeks <- 6
  previous_dates <- dataset_total1[(21 - previous_weeks + rows) : (26 - previous_weeks + rows), 9]
  previous_actual_data <- dataset_total1[(21 - previous_weeks + rows):(26 - previous_weeks + rows), 2]
  
  dates <- c(dmy(previous_dates), dates)
  actual_data <- c(previous_actual_data, actual_data)
  
  
  df <- data.frame(
    x = dates,
    actual = actual_data,
    predicted = c(rep(NA, previous_weeks), predicted_data),
    lower_bound1 = c(rep(NA, previous_weeks), lower_bound1),
    upper_bound1 = c(rep(NA, previous_weeks), upper_bound1),
    lower_bound2 = c(rep(NA, previous_weeks), lower_bound2),
    upper_bound2 = c(rep(NA, previous_weeks), upper_bound2)
  ) 
  
  year <- substr(dates, 1, 4)
  
  y_scaler = max(Predictions_Ensemble)
  grid.newpage()
  pushViewport(plotViewport(c(6.5,7.5,3,3), xscale = c(0,length(df$actual)+1), yscale = c(0,y_scaler + 300)))
  grid.rect()
  x <- seq(1, length(df$actual), 1)
  grid.points(x, df$actual, gp = gpar(col = 'black', cex = 0.5 , lwd = 7), pch = 20)
  grid.lines(x[7:18], df$predicted[7:18], default.units = 'native', gp = gpar(col = '#d62976', lwd = 3))
  grid.polygon(c(x[7:18], rev(x[7:18])), c(df$lower_bound2[7:18], rev(df$upper_bound2[7:18])), default.units = 'native', gp = gpar(col = NA, fill = '#d36060', alpha = 0.3))
  grid.polygon(c(x[7:18], rev(x[7:18])), c(df$lower_bound1[7:18], rev(df$upper_bound1[7:18])), default.units = 'native', gp = gpar(col = NA, fill = '#ff6289', alpha = 0.3))
  grid.segments(x0 = x[6], x1 = x[7], y0 = df$actual[6], y1 = df$lower_bound1[7], default.units = 'native', gp = gpar(col = "black", lty = "dashed", lwd = 3))
  grid.segments(x0 = x[6], x1 = x[7], y0 = df$actual[6], y1 = df$upper_bound1[7], default.units = 'native', gp = gpar(col = "black", lty = "dashed", lwd = 3))
  grid.segments(x0 = x[6], x1 = x[7], y0 = df$actual[6], y1 = df$predicted[7], default.units = 'native', gp = gpar(col = "black", lty = "dashed", lwd = 3))
  grid.yaxis(gp = gpar(fontsize = 18))
  # Add x-axis labels for the earliest data point of each month
  grid.text(paste('Singapore'), y = 1.045 ,just = "center",rot = 0, gp = gpar(fontsize = 20))
  grid.text('Dengue Cases', x = unit(-3, 'lines'), rot = 90, gp = gpar(fontsize = 20))
  grid.xaxis(at=x,label=as.character(paste('W',get_week_number(dates, year), sep = "")), gp = gpar(fontsize = 16))
  
  # A conditional statement to check if year changes, if yes, then find position of year change and mention the new year just below W1 of new year. Old year mentioned only once below W52.
  # Add a partition (a dashed vertical line) at exactly end of old year.
  if(year[1] == year[length(year)]){
    
    grid.text(label = as.character(year[1]), x = unit(9, "native"), y = unit(-3, "lines"),just = "center", gp = gpar(fontsize = 18))
    
  }else{
    change_pos = which(year == year[length(year)])[1] # Find out where the next year appears first.
    
    grid.text(label = as.character(year[1]), x = unit((change_pos-1), "native"), y = unit(-3, "lines"),just = "center", gp = gpar(fontsize = 18))
    grid.text(label = as.character(year[length(year)]), x = unit(change_pos, "native"), y = unit(-3, "lines"),just = "center", gp = gpar(fontsize = 18))
    grid.lines( x = unit((change_pos - 0.5), "native"), gp = gpar(lty = "dashed", col = "brown"))
    
  }
  
  pushViewport(viewport(width = 0.32, height = 0.195, x = 1, y = 1, just = c("right", "top")))
  
  # Create a legend
  grid.legend(
    labels = c('Actual Cases', 'Forecasted Cases', 'Confidence Intervals Inner', 'Confidence Intervals Outer'),
    nrow = 4,
    ncol = 1,
    do.lines = TRUE,
    pch = c(20, NA, NA, NA),  # 'Actual Cases' marked as a dot
    hgap = unit(1.2, "lines"),
    vgap = unit(1.2, "lines"),
    gp = gpar(
      col = c('black', '#d62976', '#d36060','#ff6289'),
      lwd = c(NA, 3.0, 10.0, 10.0), 
      fontsize = 15
    ),
    draw = TRUE
  )
  popViewport()
  popViewport()
  dev.off()
  setwd(paste(dir_path,"/","Model", sep = ""))
}


#######################################################################################################
load("Predictions Ensemble OOS.RData")
Predictions_Ensemble <- Predictions_Ensemble_Results[1, , ]
Predictions_Ensemble_Lower <- Predictions_Ensemble_Results[2, , ]
Predictions_Ensemble_Upper <- Predictions_Ensemble_Results[3, , ]
# regions = 1
# rows = 1
# nrow(LASSO_Predictions_Index[[regions]]$LASSO_forecast)

llim <- (n_lags + 1) + nrow(sub_df - 1) + 1
setwd(paste(dir_path,"/","Plots", sep = ""))
dir.create("OOSPlots")
# pdf("Ensembled Forecasted Dengue Cases for Singapore_OOS.pdf", width = 17, height = 9)
for(rows in 1:nrow(Predictions_Ensemble)){
  print(paste("Plotting OOS Plot: ",rows ,sep = ""))
  setwd(paste(dir_path,"/","Plots/OOSPlots", sep = ""))
  png(file = paste('OOS',rows,'.png', sep = ""),height=24,width=40,units='cm',res=300,pointsize=10)
  lower_bound1 <- vector()
  upper_bound1 <- vector()
  lower_bound2 <- vector()
  upper_bound2 <- vector()
  
  
  actual_data <- dataset_total1[(llim+rows):(llim+ 11 +rows), 2]
  predicted_data <- unname(Predictions_Ensemble[rows,])
  predicted_data[predicted_data < 0] <- 0 
  
  z_value = qnorm(1 - (conf_level / 2 ))
  
  lower_bound1 <- predicted_data - z_value * sd_residuals[,1]
  upper_bound1 <- predicted_data + z_value * sd_residuals[,1]
  lower_bound1[lower_bound1 < 0] <- 0
  upper_bound1[upper_bound1 < 0] <- 0 
  
  lower_bound2 <- unname(Predictions_Ensemble_Lower[rows, ])
  upper_bound2 <- unname(Predictions_Ensemble_Upper[rows, ])
  
  date_strings <- dataset_total1[(llim + rows):(llim + 11 + rows), 9]
  dates <- dmy(date_strings)  # Assuming the format is day/month/year
  
  
  previous_weeks <- 6
  previous_dates <- dataset_total1[(llim - previous_weeks + rows) : ((llim + 5) - previous_weeks + rows), 9]
  previous_actual_data <- dataset_total1[(llim - previous_weeks + rows):((llim + 5) - previous_weeks + rows), 2]
  
  dates <- c(dmy(previous_dates), dates)
  actual_data <- c(previous_actual_data, actual_data)
  
  
  df <- data.frame(
    x = dates,
    actual = actual_data,
    predicted = c(rep(NA, previous_weeks), predicted_data),
    lower_bound1 = c(rep(NA, previous_weeks), lower_bound1),
    upper_bound1 = c(rep(NA, previous_weeks), upper_bound1),
    lower_bound2 = c(rep(NA, previous_weeks), lower_bound2),
    upper_bound2 = c(rep(NA, previous_weeks), upper_bound2)
  ) 
  
  year <- substr(dates, 1, 4)
  y_scaler = max(Predictions_Ensemble)
  grid.newpage()
  # pushViewport(plotViewport(c(6.5,7.5,3,12.5), xscale = c(0,length(df$actual)+1), yscale = c((max(0, min(df$actual, df$predicted, df$upper_bound, df$lower_bound, na.rm = TRUE)) * 0.9),max(df$actual, df$predicted, df$upper_bound, df$lower_bound, na.rm = TRUE)*1.1)))
  pushViewport(plotViewport(c(6.5,7.5,3,3), xscale = c(0,length(df$actual)+1), yscale = c(0,y_scaler + 500)))
  grid.rect()
  x <- seq(1, length(df$actual), 1)
  grid.points(x, df$actual, gp = gpar(col = 'black', cex = 0.5 , lwd = 7), pch = 20)
  grid.lines(x[7:18], df$predicted[7:18], default.units = 'native', gp = gpar(col = '#d62976', lwd = 3))
  # grid.polygon(c(x, rev(x)), c(lci_mild_cases[2, , b], rev(lci_mild_cases[3, , b])), default.units = 'native', gp = gpar(col = NA, fill = '#ff6289', alpha = 0.5))
  grid.polygon(c(x[7:18], rev(x[7:18])), c(df$lower_bound2[7:18], rev(df$upper_bound2[7:18])), default.units = 'native', gp = gpar(col = NA, fill = '#d36060', alpha = 0.3))
  grid.polygon(c(x[7:18], rev(x[7:18])), c(df$lower_bound1[7:18], rev(df$upper_bound1[7:18])), default.units = 'native', gp = gpar(col = NA, fill = '#ff6289', alpha = 0.3))
  grid.segments(x0 = x[6], x1 = x[7], y0 = df$actual[6], y1 = df$lower_bound1[7], default.units = 'native', gp = gpar(col = "black", lty = "dashed", lwd = 3))
  grid.segments(x0 = x[6], x1 = x[7], y0 = df$actual[6], y1 = df$upper_bound1[7], default.units = 'native', gp = gpar(col = "black", lty = "dashed", lwd = 3))
  grid.segments(x0 = x[6], x1 = x[7], y0 = df$actual[6], y1 = df$predicted[7], default.units = 'native', gp = gpar(col = "black", lty = "dashed", lwd = 3))
  grid.yaxis(gp = gpar(fontsize = 18))
  # Add x-axis labels for the earliest data point of each month
  grid.text(paste('Singapore'), y = 1.045 ,just = "center",rot = 0, gp = gpar(fontsize = 20))
  grid.text('Dengue Cases', x = unit(-3, 'lines'), rot = 90, gp = gpar(fontsize = 20))
  grid.xaxis(at=x,label=as.character(paste('W',get_week_number(dates, year), sep = "")), gp = gpar(fontsize = 16))
  
  # A conditional statement to check if year changes, if yes, then find position of year change and mention the new year just below W1 of new year. Old year mentioned only once below W52.
  # Add a partition (a dashed vertical line) at exactly end of old year.
  if(year[1] == year[length(year)]){
    
    grid.text(label = as.character(year[1]), x = unit(9, "native"), y = unit(-3, "lines"),just = "center", gp = gpar(fontsize = 18))
    
  }else{
    change_pos = which(year == year[length(year)])[1] # Find out where the next year appears first.
    
    grid.text(label = as.character(year[1]), x = unit((change_pos-1), "native"), y = unit(-3, "lines"),just = "center", gp = gpar(fontsize = 18))
    grid.text(label = as.character(year[length(year)]), x = unit(change_pos, "native"), y = unit(-3, "lines"),just = "center", gp = gpar(fontsize = 18))
    grid.lines( x = unit((change_pos - 0.5), "native"), gp = gpar(lty = "dashed", col = "brown"))
    
  }
  
  pushViewport(viewport(width = 0.32, height = 0.195, x = 1, y = 1, just = c("right", "top")))
  
  # Create a legend
  grid.legend(
    labels = c('Actual Cases', 'Forecasted Cases', 'Confidence Intervals Inner' ,'Confidence Intervals Outer'),
    nrow = 4,
    ncol = 1,
    do.lines = TRUE,
    pch = c(20, NA, NA, NA),  # 'Actual Cases' marked as a dot
    hgap = unit(1.2, "lines"),
    vgap = unit(1.2, "lines"),
    gp = gpar(
      col = c('black', '#d62976', '#d36060','#ff6289'),
      lwd = c(NA, 3.0, 10.0, 10.0), 
      fontsize = 15
    ),
    draw = TRUE
  )
  popViewport()
  popViewport()
  dev.off()
  setwd(paste(dir_path,"/","Model", sep = ""))
}

setwd(paste(dir_path,"/","Plots", sep = ""))









