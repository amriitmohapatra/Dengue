rm(list = keep(dir_path, start_date_input_train, end_date_input_train, start_date_input_test, end_date_input_test))
setwd(paste(dir_path, "/Model/Ensemble", sep = ""))

library(dplyr)
library(grid)
library(gridExtra)
n_lags <- 20
n_regions <- 34
colours_scheme <- c("#6d1a36", "#d7b8f3", "#f397d6", "#f42272", "#232e21", "#3772ff", "#df2935", "#fdca40", "#587b7f", "#4f1271", "#cfee9e",
                    "#fcde9c", "#ffa552", "#ba5624", "#381d2a", "#339989", "#7de2d1", "#416788", "#7389ae", "#b5bad0", "#885053", "#fe5f55",
                    "#777da7", "#94c9a9", "#d3b99f", "#3b3561", "#b8c480", "#922d50", "#32de8a", "#192bc2", "#ae8e1c", "#c1df1f", "#23b5d3",
                    "#b8b8f3")


df <- read.csv("case data.csv")
data_type <- c("Train", "Test")
week_looper <- seq(from = 7, by = 7, length.out = 12)



beautification <- function(df){
  df <- read.csv("case data.csv")
  data_type <- c("Train", "Test")
  week_looper <- seq(from = 7, by = 7, length.out = 12)
  
  load("Final_Predictions.RData")
  WS_data <- regions_forecast
  load("Final_Predictions_OOS.RData")
  OOS_data <- regions_forecast
  
  log_WS_data <- lapply(WS_data, function(mat) {
    apply(mat, c(1,2) ,function(x) log10(1+x))
  })
  
  log_OOS_data <- lapply(OOS_data, function(mat) {
    apply(mat, c(1,2) ,function(x) log10(1+x))
  })
  
  df$Date <- as.Date(df$end.date, format = "%d/%m/%Y")
  df <- df %>%
    filter(Date < as.character(as.Date(end_date_input_test) + week_looper[length(week_looper)]))
  df <- df[-1:(-(n_lags)), -c(1,2,3,38)]
  
  log_df <- df %>%
    mutate_all(~log10(. + 1))
  ticks <- c(1,10,100,1000)
  # ticks <- pretty(range(c(0, (max(log_df, max(unlist(lapply(log_WS_data, max))), max(unlist(lapply(log_OOS_data, max)))) ))), n = 15)
  png_lb =  min(log10(ticks)) - 1
  png_ub = max(log10(ticks)) + 1
  return(list(ticks = ticks, png_lb = png_lb, png_ub = png_ub))
}

preprocessor <- function(df, weeks, regions_forecast, data_type){
  pred_days <- nrow(regions_forecast[[1]])
  week_looper <- seq(from = 7, by = 7, length.out = 12)
  df$Date <- as.Date(df$end.date, format = "%d/%m/%Y")
  if(data_type == "Train"){
    df <- df %>%
      filter(Date < as.character(as.Date(end_date_input_train) + week_looper[weeks])) # This part needs some change for the OOS part
    df <- df[-1:(-(n_lags + weeks)),]
    
  } else if (data_type == "Test"){
    df <- df %>%
      filter(Date >= as.Date(start_date_input_test) & Date <= as.character(as.Date(end_date_input_test) + week_looper[weeks]))
    df <- df[-1:- weeks,]
  }
  true_matrix <- as.matrix(df[,-c(1,2,3,38)]) 
  log_true_matrix <- log10(1 + true_matrix)
  predicted_matrix <- matrix(NA, nrow = pred_days, ncol = n_regions)
  for(region in 1:n_regions){
    predicted_matrix[,region] <- regions_forecast[[region]][,weeks]
  }
  log_predicted_matrix <- log10(1 + predicted_matrix)
  
  A <- as.vector(log_true_matrix)
  B <- as.vector(log_predicted_matrix)
  correlation <- cor(A, B)
  
  return(list(true_matrix = log_true_matrix, predicted_matrix = log_predicted_matrix, r = correlation))
}

panel.plotter <- function(data_type, true_matrix, predicted_matrix, ticks, png_lb, png_ub, week_num, letter_counter, r){
  pushViewport(viewport(layout.pos.col=data_type,layout.pos.row=week_num))
  
  if(data_type == 2){
    pushViewport(plotViewport(c(2,3,0,0), xscale = c(png_lb, png_ub) , yscale = c(png_lb, png_ub)))
  }else{
    pushViewport(plotViewport(c(2,3,0,0), xscale = c(png_lb, png_ub) , yscale = c(png_lb, png_ub)))
  }
  
  grid.rect() 
  
  grid.lines(gp = gpar(lty = "dashed", col = "black", lwd = 0.7))
  # Need to fix scale b/w 0 and max value + 50
  # use pretty to come up with good scales for the x and y axes, they need to be uniform.
  
  for(regions in 1:n_regions){
    
    # A sub-loop that loops through all regions
    x <- predicted_matrix[,regions] # 1 is for Bukit Batok
    y <- true_matrix[,regions]
    
    
    grid.points(x, y, gp = gpar(col = colours_scheme[regions], cex = 0.1 , lwd = 1, alpha = 0.6), pch = 20)
  }
  myticks <- c(1,10,100,1000)

  grid.xaxis(at = log10(myticks) , label = myticks ,gp = gpar(fontsize = 8))
  grid.yaxis(at = log10(myticks) , label = myticks ,gp = gpar(fontsize = 8))
  if(data_type == 1){
    grid.text('Observed', x = unit(-3, 'lines'), rot = 90, gp = gpar(fontsize = 10, fontface = "bold")) 
  }
  if(week_num == 4){
    grid.text('Predicted', y = unit(-3, 'lines'), gp = gpar(fontsize = 10, fontface = "bold")) 
  }
  if(data_type == 1){
    if(weeks > 9){
      grid.text(paste(letters[letter_counter],sep = ""), x = unit(1, 'lines'), y = 0.95, gp = gpar(fontsize = 10, fontface = "bold"))
      grid.text(paste("Week ", weeks, " Within Sample",sep = ""), x = unit(7.7, 'lines'), y = 0.94, gp = gpar(fontsize = 8))
      
      # grid.text(paste("(",letters[letter_counter],")", " Week ", weeks, " Within Sample",sep = ""), x = unit(5.35, 'lines'), y = 0.95, gp = gpar(fontsize = 8))
    }else{
      # grid.text(paste("(",letters[letter_counter],")", " Week ", weeks, " Within Sample",sep = ""), x = unit(5, 'lines'), y = 0.95, gp = gpar(fontsize = 8))
      grid.text(paste(letters[letter_counter],sep = ""), x = unit(1.3, 'lines'), y = 0.95, gp = gpar(fontsize = 10, fontface = "bold"))
      grid.text(paste("Week ", weeks, " Within Sample",sep = ""), x = unit(7.7, 'lines'), y = 0.94, gp = gpar(fontsize = 8))
    }

  }else if(data_type == 2){
    if(weeks > 9){
      grid.text(paste(letters[letter_counter],sep = ""), x = unit(1, 'lines'), y = 0.95, gp = gpar(fontsize = 10, fontface = "bold"))
      grid.text(paste("Week ", weeks, " Out of Sample",sep = ""), x = unit(7.7, 'lines'), y = 0.94, gp = gpar(fontsize = 8))
      # grid.text(paste("(",letters[letter_counter],")", " Week ", weeks, " Out of Sample",sep = ""), x = unit(5.35, 'lines'), y = 0.95, gp = gpar(fontsize = 8))
    }else{
      grid.text(paste(letters[letter_counter],sep = ""), x = unit(1.3, 'lines'), y = 0.95, gp = gpar(fontsize = 10, fontface = "bold"))
      grid.text(paste("Week ", weeks, " Out of Sample",sep = ""), x = unit(7.7, 'lines'), y = 0.94, gp = gpar(fontsize = 8))
      # grid.text(paste("(",letters[letter_counter],")", " Week ", weeks, " Out of Sample",sep = ""), x = unit(5, 'lines'), y = 0.95, gp = gpar(fontsize = 8))
    }
  }
  grid.text(paste("r = ", round(r,2), sep = ""), x = unit(5, 'lines'), y = 0.86, gp = gpar(fontsize = 8))
  popViewport()
  popViewport()
}


########################### Auto Run ################################
filename <- paste("LogAcuPlot_v2" ,".png", sep = "")
png(file = filename,height=24,width=15,units='cm',res=300,pointsize=10)
pushViewport(plotViewport(c(2,2,1,1)))
pushViewport(viewport(layout=grid.layout(nrow=4,ncol=2)))
my_weeks <- c(1,4,8,12)
letter_counter = 1
for(weeks in my_weeks){
  if(weeks == 1){
    week_edit = 1
  }else if(weeks == 4){
    week_edit = 2
  }else if(weeks == 8){
    week_edit = 3
  }else{
    week_edit = 4
  }
  for(i in data_type){
    if(i == "Train"){
      load("Final_Predictions.RData")
      data_type_num = 1
    }else if(i == "Test"){
      load("Final_Predictions_OOS.Rdata")
      data_type_num = 2
    }
    op_set <- preprocessor(df, weeks, regions_forecast, i)
    beautifier <- beautification(df)
    panel.plotter(data_type = data_type_num, true_matrix = op_set$true_matrix, predicted_matrix = op_set$predicted_matrix, ticks = beautifier$ticks, png_lb = beautifier$png_lb, png_ub = beautifier$png_ub, week_num = week_edit, letter_counter = letter_counter, r = op_set$r)
    letter_counter = letter_counter + 1
  }
}
popViewport()
popViewport()
dev.off()

setwd(paste(dir_path,"/Plots and Videos", sep = ""))







