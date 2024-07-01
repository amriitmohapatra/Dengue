import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from matplotlib.colors import LinearSegmentedColormap

plt.rcParams.update({'font.size': 15})

# Define the directory variable
directory = 'C:\\Users\\amrit.LAPTOP-82H6QHOJ\\Documents\\nBox\\DengueFinal\\Stage2\\'

# Load the new data from the provided file
new_file_path = f'{directory}MAAPE_WS_Precision.csv'
new_data = pd.read_csv(new_file_path)

# Get the new heatmap data and set the 'Unnamed: 0' column as the index
new_heatmap_data = new_data.set_index('Unnamed: 0')

# Normalize the new heatmap data by the range of the data to scale the color mapping
new_norm = plt.Normalize(vmin=new_heatmap_data.min().min(), vmax=new_heatmap_data.max().max())

# Define the colors for the custom colormap (Red to Purple gradient)
colors = ["#67a9cf", "#ffffbf", "#ef8a62"]
n_bins = 100  # Use 100 bins
cmap_name = "custom_colormap"
custom_colormap = LinearSegmentedColormap.from_list(cmap_name, colors, N=n_bins)

# Plot the new heatmap with the custom colormap (Red to Purple gradient)
plt.figure(figsize=(12, 8))
heatmap = sns.heatmap(new_heatmap_data, annot=True, fmt=".2f", linewidths=.5, cmap=custom_colormap, norm=new_norm)

# Set the y-axis labels to the desired labels
new_y_labels = ["LASSO", "SVR1", "SVR2", "SVR3", "SVR4", "DT1", "DT2", "RF", "Bagging", "GBM1", "GBM2", "LSTM", "GRU", "Ensemble"]
heatmap.set_yticklabels(new_y_labels, rotation=0)  # Rotate the labels for better readability

# Set the y-axis labels to the desired labels
new_x_labels = ["W1", "W2", "W3", "W4", "W5", "W6", "W7", "W8", "W9", "W10", "W11", "W12"]
heatmap.set_xticklabels(new_x_labels, rotation=0)  # Rotate the labels for better readability

# Add title and labels
plt.title('MAAPE values with different models within sample', fontsize=20)
plt.xlabel('Weeks of Prediction', fontsize=18)
plt.ylabel('Models', fontsize=18)

# Show the plot or save it as an image
plt.savefig(f"{directory}MAAPE.png")
