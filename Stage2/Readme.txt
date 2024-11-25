The user needs to run Stage2.R

Please enter the directory path correctly to avoid any error. When running Stage2.R, the code will ask to enter the user directory.
Paste the directory path where the Stage2 folder is located.
It should look something like this: "C:\\username\...\Dengue\Stage2"

There are a few plots that were done using python. The directory needs to be set manually for each file before 
running the code.

Note: For the plots generated via python, run the files only after Stage2.R
The following files need to be run independently: 
1) Obs_vs_Pred_WS.py -> Within sample spatial-temporal forecast plots for each week-ahead ensemble + dirichlet model. 
2) Obs_vs_Pred_OOS.py -> Out of sample spatial-temporal forecast plots for each week-ahead ensemble + dirichlet model.
3) Difference_WS.py -> Within sample spatial-temporal deviation plots for each week-ahead ensemble + dirichlet model.
4) Difference_OOS.py -> Out of sample spatial-temporal deviation plots for each week-ahead ensemble + dirichlet model.
5) Error_Plots.py -> Mean Absolute Error, Mean Arctangent Absolute Percentage Error and Root Mean Squared Error plots for Stage1 and Stage2.
For Stage 2, apart from the Ensemble model, Dirichlet regression is performed on all inidividual machine learning models to compare prediction performance. 

