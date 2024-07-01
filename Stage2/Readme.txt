In Stage 2, all models, including the ensemble model generated in Stage 1, are subjected to Dirichlet regression. However, for the purpose of useful results, only the 
region-wise forecasts from the ensemble model forecasts are to be used. The results from the rest of the models are used for comparison and performance analysis.

For Stage 2 regression, firstly, create 13 different folders in the main directory and name them as per the names of the machine learning models used in Stage 1 and 
another folder named "Ensemble".
In the 13 folders, paste All_Forecasts.RData, All_Forecasts_OOS.RData, case data.csv, and Dengue Weight Matrix.RData from Stage 1.
In the folder named "Ensemble", add Dengue Weight Matrix.RData, Predictions Ensemble.RData and Predictions Ensemble OOS.RData from Stage 1.

Go back to the Stage 2 directory and then execute the following files in this sequence:
1) Stage 2_34R.R
2) Stage 2_34R_Ensemble.R
3) Stage 2_34R_OOS.R
4) Stage 2_34R_OOS_Ensemble.R
5) MFSS_Precision.R
6) ACU2.R
7) Melter.R
8) Metler_OOS.R

Scripts 7) and 8) are used to generate a sequence of files that are used for spatial-temporal plots as well as videos, which are generated using python code.

Plots
1) Log Accuracy Plots (main article + supplementary)
2) 4-panel plot for Obs vs Actual(.py script)
3) 4-panel plot for deviation(.py script)
4) All 12 forecast model videos for Obs vs Actual Within and Out of Sample (2 .py scripts)
5) All 12 forecast model videos for Deviation Within and Out of Sample (2 .py scripts)

ideally, the Melter.R and Melter_OOS.R should generate the files in a folder within the plots folder so that the
.py scripts can run smoothly and produce the .mp4 files in the plots folder.