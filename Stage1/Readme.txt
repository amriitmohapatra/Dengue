The following sequence is to be followed when running the R files:
A. Stage 1
1) Files "Dengue Weekly Total.csv", All_Functions.R, All_Functions_OOS.R, Within Sample Predictions.R, Out of Sample Predictions.R, Ensembling Predictions_byweek.R and
    Ensembling Predictions_byweek_OOS.R are to be in the same directory.
2) In All_Functions.R and All_Functions_OOS.R, ensure that all setwd() functions are set to the same directory as the rest of the aforementioned files.
3) The files are to be run in the following sequence:
    i) Within Sample Predictions.R
    ii) Out of Sample Predictions.R
    iii) Ensembling Predictions_byweek.R
    iv) Ensembling Predictions_byweek_OOS.R
    v) SummaryStatsStage1.R
    vi) Shi Yuan Style Plot_Updated.R (Improper Name)
    
    
4) In Within Sample Predictions.R, I had run the script in three phases. In the first phase, I execute all models except LSTM and GRU and the results are stored as
    All_Forecasts1.RData. The second and third phase each involves running only the LSTM and the GRU model separately respectively and storing the results as All_Forecasts2.RData and All_Forecasts3.RData.
    In the subsequent step, I load these three files separately and combine the results in the list called All_Predictions_Master stored as All_Forecasts.RData.
    (The reason for using such an approach is that LSTM and GRU models took longer time to run and hence they were run independently.)
    
File Update 1: Some changes have been made and bugs removed. 
