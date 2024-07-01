rm(list = keep(dir_path, start_date_input_train, end_date_input_train, start_date_input_test, end_date_input_test))
setwd(paste(dir_path,"/","Model", sep = ""))
load("Sampled Weights WS.RData")
library(grid)
library(gridExtra)
library(ggplot2)
library(tidyr)



set.seed(19)
colors <- sample(c("#f72585", "#7209b7", "#3a0ca3", "#4361ee", "#4cc9f0", "#bc4b51", "#f4a259", "#720026", "#ef476f", "#ef233c", "#f49cbb", "#f75c03", "#ebb3a9"))
set.seed(10)
colors2 <- sample(c("#f72585", "#7209b7", "#3a0ca3", "#4361ee", "#4cc9f0", "#bc4b51", "#f4a259", "#720026", "#ef476f", "#ef233c", "#f49cbb", "#f75c03", "#ebb3a9"))
# colors <- sample(rainbow(13))
model_names <- c("LASSO","SVR1","SVR2","SVR3","SVR4","DT1","DT2","RF","Bagging","GBM1","GBM2","LSTM","GRU")
setwd(paste(dir_path,"/","Plots", sep = ""))
png(file = 'WeightsDistv2.png',height=70,width=100,units='cm',res=300,pointsize=10)
pushViewport(plotViewport(c(7,1,1,1)))
pushViewport(viewport(layout=grid.layout(nrow=2,ncol=2)))
## Row 1 is for Weeks 1,2,3,4
#row looper
#column looper
#Week 1, row =1,col =1
letter_counter = 1
for(week_num in c(1,4,8,12)){
  
  my_matrix <- matrix(NA, nrow = length(model_names), ncol = length(Weights_Matrix_List))
  for(ML_number in 1:length(model_names)){
    for(ref_week in 1:length(Weights_Matrix_List)){
      my_matrix[ML_number,ref_week] <- Weights_Matrix_List[[ref_week]][ML_number,week_num] 
    }
  }
  my_matrix <- t(my_matrix)
  colnames(my_matrix) <- model_names
  my_df <- as.data.frame(my_matrix)
  df_reordered <- my_df[,model_names]
  new_df <- pivot_longer(df_reordered, cols = everything(),names_to = "group", values_to = "value")
  
  if(week_num == 1){
    row_counter <- 1
    col_counter <- 1
  } else if(week_num == 4){
    row_counter <- 1
    col_counter <- 2
  } else if(week_num == 8){
    row_counter <- 2
    col_counter <- 1
  } else{
    row_counter <- 2
    col_counter <- 2
  }

  cat("For Week =", week_num, "Result =", row_counter, ",", col_counter, "\n")
  
  pushViewport(viewport(layout.pos.row=row_counter, layout.pos.col=col_counter,))
  pushViewport(plotViewport(c(1, 1, 1, 1)))
  # grid.rect()
  boxplot_plot <- ggplot(new_df, aes(x = factor(group, levels = rev(model_names)), y = value, fill = group)) +
    geom_boxplot(color = colors2, outlier.size = 3.5, linewidth = 2) +
    stat_summary(fun.y = median, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..), color = "white", size = 1.5) +
    stat_summary(fun.y = median, geom = "point", shape = 18, size = 5, color = "white", fill = "black") +
    coord_flip() +
    scale_fill_manual(values = colors) +
    guides(fill = "none")+
    theme_minimal()+
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_text(face = ifelse(new_df$group %in% model_names, "plain"), size = 50, colour = "black"),
          axis.text.x = element_text(size = 50, color = "black"),
          panel.border = element_rect(color = "black", fill = NA, size = 1))+
    scale_y_continuous(breaks = seq(0, 1, by = 0.10))
  # theme(axis.title.x = element_blank(),
  #       axis.text.x = element_blank(),
  #       axis.line.x = element_blank(),
  #       axis.title.y = element_blank())
  
  boxplot_grob <- ggplotGrob(boxplot_plot)
  grid.draw(boxplot_grob)
  grid.text(paste(letters[letter_counter], sep = ""), x = unit(0.68, 'npc'), y = unit(0.9, 'npc'), gp = gpar(fontsize = 70, fontface = "bold"))
  if(row_counter == 2 & col_counter == 2){
    grid.text(paste("Week ", week_num,sep = ""), x = unit(0.82, 'npc'), y = unit(0.9, 'npc'), gp = gpar(fontsize = 70))
  }else{
    grid.text(paste("Week ", week_num,sep = ""), x = unit(0.80, 'npc'), y = unit(0.9, 'npc'), gp = gpar(fontsize = 70))
  }
  
  if(row_counter == 2){
    grid.text("Weights", x = unit(0.5, 'npc'), y = unit(-0.04, 'npc'), gp = gpar(fontsize = 70, fontface = "bold"))
  }
  popViewport()
  popViewport()
  letter_counter = letter_counter + 1
}

popViewport()
popViewport()
dev.off()

setwd(paste(dir_path,"/","Plots", sep = ""))
























