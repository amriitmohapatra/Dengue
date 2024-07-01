import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from matplotlib.colors import LinearSegmentedColormap

# Load the updated data from the provided file
new_file_path = '/Users/shangyingying/Desktop/Dengue/june/Error/stage 1/MAAPE_OOS_Precision.csv'
new_data = pd.read_csv(new_file_path)

plt.rcParams.update({'font.size': 15})

# If the data includes an 'Unnamed: 0' column, set it as the index. Otherwise, directly use the data.
if 'Unnamed: 0' in new_data.columns:
    new_data.set_index('Unnamed: 0', inplace=True)

# Transpose the data to have models on the y-axis and weeks of prediction on the x-axis
new_data_transposed = new_data.T

# Normalize the heatmap data by the range to scale the color mapping appropriately
new_norm = plt.Normalize(vmin=new_data_transposed.min().min(), vmax=new_data_transposed.max().max())

# Define a custom colormap (Red to Purple gradient)
colors = ["#67a9cf", "#ffffbf", "#ef8a62"]
n_bins = 100  # Specifies the number of bins for the gradient
cmap_name = "custom_colormap"
custom_colormap = LinearSegmentedColormap.from_list(cmap_name, colors, N=n_bins)

# Plot the heatmap with the custom colormap
plt.figure(figsize=(12, 8))
heatmap = sns.heatmap(new_data_transposed, annot=True, fmt=".2f", linewidths=.5, cmap=custom_colormap, norm=new_norm)

# Set the y-axis labels to the desired labels
new_y_labels = ["LASSO", "SVR1", "SVR2", "SVR3", "SVR4", "DT1", "DT2", "RF", "Bagging", "GBM1", "GBM2", "LSTM", "GRU", "Ensemble"]
heatmap.set_yticklabels(new_y_labels, rotation=0)  # Rotate the labels for better readability

# Add title and labels with sizes adjusted for clarity
plt.title('MAAPE values with different models out of sample', fontsize=20)
plt.xlabel('Weeks of Prediction', fontsize=18)
plt.ylabel('Models', fontsize=18)

# Adjust the tick labels for better readability
plt.xticks(rotation=45)  # Rotate x-axis labels for better visibility
plt.yticks(rotation=0)   # Keep y-axis labels horizontal

# Show the plot or save it as an image
plt.savefig("MAAPE_OOS.png")

