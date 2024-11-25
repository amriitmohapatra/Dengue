import os

# Ask the user to input the root directory path
root_dir = input("Enter the root directory path: ")

# Replace single backslashes with double backslashes
formatted_dir = root_dir.replace("\\", "\\\\")

# Change the directory
try:
    os.chdir(formatted_dir)
    print(f"Successfully changed the directory to: {os.getcwd()}")
except FileNotFoundError:
    print("The directory does not exist.")
except Exception as e:
    print(f"An error occurred: {e}")


import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from matplotlib.colors import LinearSegmentedColormap


# Load the updated data from the provided file
new_file_path = root_dir + '\\Stage1\\Model\\MAE_Precision.csv'
new_data = pd.read_csv(new_file_path)

plt.rcParams.update({'font.size': 15})

# If the data includes an 'Unnamed: 0' column, set it as the index. Otherwise, directly use the data.
if 'Unnamed: 0' in new_data.columns:
    new_data.set_index('Unnamed: 0', inplace=True)

# Transpose the data to have models on the y-axis and weeks of prediction on the x-axis
new_data_transposed = new_data.T

# Convert the data to integers and ensure values rounded to 0 are set to 1
new_data_transposed = new_data_transposed.round().astype(int)
new_data_transposed[new_data_transposed == 0] = 1

# Normalize the heatmap data by the range to scale the color mapping appropriately
new_norm = plt.Normalize(vmin=new_data_transposed.min().min(), vmax=new_data_transposed.max().max())

# Define a custom colormap (Red to Purple gradient)
colors = ["#67a9cf", "#ffffbf", "#ef8a62"]
n_bins = 100  # Specifies the number of bins for the gradient
cmap_name = "custom_colormap"
custom_colormap = LinearSegmentedColormap.from_list(cmap_name, colors, N=n_bins)

# Plot the heatmap with the custom colormap
plt.figure(figsize=(12, 8))
heatmap = sns.heatmap(new_data_transposed, annot=True, fmt="d", linewidths=.5, cmap=custom_colormap, norm=new_norm)

# Set the y-axis labels to the desired labels
new_y_labels = ["LASSO", "SVR1", "SVR2", "SVR3", "SVR4", "DT1", "DT2", "RF", "Bagging", "GBM1", "GBM2", "LSTM", "GRU", "Ensemble"]
heatmap.set_yticklabels(new_y_labels, rotation=0)  # Rotate the labels for better readability

# Add title and labels with sizes adjusted for clarity
plt.title('MAE values with different models within sample', fontsize=20)
plt.xlabel('Weeks of Prediction', fontsize=18)
plt.ylabel('Models', fontsize=18)

# Adjust the tick labels for better readability
plt.xticks(rotation=45)  # Rotate x-axis labels for better visibility
plt.yticks(rotation=0)   # Keep y-axis labels horizontal

# Show the plot or save it as an image
os.chdir(root_dir + '\\Stage2\\Plots and Videos')
plt.savefig("MAE_WS_S1.png")


import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from matplotlib.colors import LinearSegmentedColormap

# Load the updated data from the provided file
new_file_path = root_dir + '\\Stage1\\Model\\MAE_OOS_Precision.csv'
new_data = pd.read_csv(new_file_path)

plt.rcParams.update({'font.size': 15})

# If the data includes an 'Unnamed: 0' column, set it as the index. Otherwise, directly use the data.
if 'Unnamed: 0' in new_data.columns:
    new_data.set_index('Unnamed: 0', inplace=True)

# Transpose the data to have models on the y-axis and weeks of prediction on the x-axis
new_data_transposed = new_data.T

# Convert the data to integers and ensure values rounded to 0 are set to 1
new_data_transposed = new_data_transposed.round().astype(int)
new_data_transposed[new_data_transposed == 0] = 1

# Normalize the heatmap data by the range to scale the color mapping appropriately
new_norm = plt.Normalize(vmin=new_data_transposed.min().min(), vmax=new_data_transposed.max().max())

# Define a custom colormap (Red to Purple gradient)
colors = ["#67a9cf", "#ffffbf", "#ef8a62"]
n_bins = 100  # Specifies the number of bins for the gradient
cmap_name = "custom_colormap"
custom_colormap = LinearSegmentedColormap.from_list(cmap_name, colors, N=n_bins)

# Plot the heatmap with the custom colormap
plt.figure(figsize=(12, 8))
heatmap = sns.heatmap(new_data_transposed, annot=True, fmt="d", linewidths=.5, cmap=custom_colormap, norm=new_norm)

# Set the y-axis labels to the desired labels
new_y_labels = ["LASSO", "SVR1", "SVR2", "SVR3", "SVR4", "DT1", "DT2", "RF", "Bagging", "GBM1", "GBM2", "LSTM", "GRU", "Ensemble"]
heatmap.set_yticklabels(new_y_labels, rotation=0)  # Rotate the labels for better readability

# Add title and labels with sizes adjusted for clarity
plt.title('MAE values with different models out of sample', fontsize=20)
plt.xlabel('Weeks of Prediction', fontsize=18)
plt.ylabel('Models', fontsize=18)

# Adjust the tick labels for better readability
plt.xticks(rotation=45)  # Rotate x-axis labels for better visibility
plt.yticks(rotation=0)   # Keep y-axis labels horizontal

# Show the plot or save it as an image
os.chdir(root_dir + '\\Stage2\\Plots and Videos')
plt.savefig("MAE_OOS_S1.png")

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from matplotlib.colors import LinearSegmentedColormap

# Load the updated data from the provided file
new_file_path = root_dir + '\\Stage1\\Model\\MAAPE_Precision.csv'
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
plt.title('MAAPE values with different models within sample', fontsize=20)
plt.xlabel('Weeks of Prediction', fontsize=18)
plt.ylabel('Models', fontsize=18)

# Adjust the tick labels for better readability
plt.xticks(rotation=45)  # Rotate x-axis labels for better visibility
plt.yticks(rotation=0)   # Keep y-axis labels horizontal

# Show the plot or save it as an image
os.chdir(root_dir + '\\Stage2\\Plots and Videos')
plt.savefig("MAAPE_WS_S1.png")

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from matplotlib.colors import LinearSegmentedColormap

# Load the updated data from the provided file
new_file_path = root_dir + '\\Stage1\\Model\\MAAPE_OOS_Precision.csv'
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
os.chdir(root_dir + '\\Stage2\\Plots and Videos')
plt.savefig("MAAPE_OOS_S1.png")

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from matplotlib.colors import LinearSegmentedColormap

# Load the updated data from the provided file
new_file_path = root_dir + '\\Stage1\\Model\\RMSE_Precision.csv'
new_data = pd.read_csv(new_file_path)

plt.rcParams.update({'font.size': 15})

# If the data includes an 'Unnamed: 0' column, set it as the index. Otherwise, directly use the data.
if 'Unnamed: 0' in new_data.columns:
    new_data.set_index('Unnamed: 0', inplace=True)

# Transpose the data to have models on the y-axis and weeks of prediction on the x-axis
new_data_transposed = new_data.T

# Convert the data to integers and ensure values rounded to 0 are set to 1
new_data_transposed = new_data_transposed.round().astype(int)
new_data_transposed[new_data_transposed == 0] = 1

# Normalize the heatmap data by the range to scale the color mapping appropriately
new_norm = plt.Normalize(vmin=new_data_transposed.min().min(), vmax=new_data_transposed.max().max())

# Define a custom colormap (Red to Purple gradient)
colors = ["#67a9cf", "#ffffbf", "#ef8a62"]
n_bins = 100  # Specifies the number of bins for the gradient
cmap_name = "custom_colormap"
custom_colormap = LinearSegmentedColormap.from_list(cmap_name, colors, N=n_bins)

# Plot the heatmap with the custom colormap
plt.figure(figsize=(12, 8))
heatmap = sns.heatmap(new_data_transposed, annot=True, fmt="d", linewidths=.5, cmap=custom_colormap, norm=new_norm)

# Set the y-axis labels to the desired labels
new_y_labels = ["LASSO", "SVR1", "SVR2", "SVR3", "SVR4", "DT1", "DT2", "RF", "Bagging", "GBM1", "GBM2", "LSTM", "GRU", "Ensemble"]
heatmap.set_yticklabels(new_y_labels, rotation=0)  # Rotate the labels for better readability

# Add title and labels with sizes adjusted for clarity
plt.title('RMSE values with different models within sample', fontsize=20)
plt.xlabel('Weeks of Prediction', fontsize=18)
plt.ylabel('Models', fontsize=18)

# Adjust the tick labels for better readability
plt.xticks(rotation=45)  # Rotate x-axis labels for better visibility
plt.yticks(rotation=0)   # Keep y-axis labels horizontal

# Show the plot or save it as an image
os.chdir(root_dir + '\\Stage2\\Plots and Videos')
plt.savefig("RMSE_WS_S1.png")

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from matplotlib.colors import LinearSegmentedColormap

# Load the updated data from the provided file
new_file_path = root_dir + '\\Stage1\\Model\\RMSE_OOS_Precision.csv'
new_data = pd.read_csv(new_file_path)

plt.rcParams.update({'font.size': 15})

# If the data includes an 'Unnamed: 0' column, set it as the index. Otherwise, directly use the data.
if 'Unnamed: 0' in new_data.columns:
    new_data.set_index('Unnamed: 0', inplace=True)

# Transpose the data to have models on the y-axis and weeks of prediction on the x-axis
new_data_transposed = new_data.T

# Convert the data to integers and ensure values rounded to 0 are set to 1
new_data_transposed = new_data_transposed.round().astype(int)
new_data_transposed[new_data_transposed == 0] = 1

# Normalize the heatmap data by the range to scale the color mapping appropriately
new_norm = plt.Normalize(vmin=new_data_transposed.min().min(), vmax=new_data_transposed.max().max())

# Define a custom colormap (Red to Purple gradient)
colors = ["#67a9cf", "#ffffbf", "#ef8a62"]
n_bins = 100  # Specifies the number of bins for the gradient
cmap_name = "custom_colormap"
custom_colormap = LinearSegmentedColormap.from_list(cmap_name, colors, N=n_bins)

# Plot the heatmap with the custom colormap
plt.figure(figsize=(12, 8))
heatmap = sns.heatmap(new_data_transposed, annot=True, fmt="d", linewidths=.5, cmap=custom_colormap, norm=new_norm)

# Set the y-axis labels to the desired labels
new_y_labels = ["LASSO", "SVR1", "SVR2", "SVR3", "SVR4", "DT1", "DT2", "RF", "Bagging", "GBM1", "GBM2", "LSTM", "GRU", "Ensemble"]
heatmap.set_yticklabels(new_y_labels, rotation=0)  # Rotate the labels for better readability

# Add title and labels with sizes adjusted for clarity
plt.title('RMSE values with different models out of sample', fontsize=20)
plt.xlabel('Weeks of Prediction', fontsize=18)
plt.ylabel('Models', fontsize=18)

# Adjust the tick labels for better readability
plt.xticks(rotation=45)  # Rotate x-axis labels for better visibility
plt.yticks(rotation=0)   # Keep y-axis labels horizontal

# Show the plot or save it as an image
os.chdir(root_dir + '\\Stage2\\Plots and Videos')
plt.savefig("RMSE_OOS_S1.png")

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from matplotlib.colors import LinearSegmentedColormap

plt.rcParams.update({'font.size': 15})


# Load the new data from the provided file
new_file_path = root_dir + '\\Stage2\\MAE_WS_Precision.csv'
new_data = pd.read_csv(new_file_path)

# Get the new heatmap data and set the 'Unnamed: 0' column as the index
new_heatmap_data = new_data.set_index('Unnamed: 0')

# Convert the data to integers and ensure values rounded to 0 are set to 1
new_heatmap_data = new_heatmap_data.round().astype(int)
new_heatmap_data[new_heatmap_data == 0] = 1

# Normalize the new heatmap data by the range of the data to scale the color mapping
new_norm = plt.Normalize(vmin=new_heatmap_data.min().min(), vmax=new_heatmap_data.max().max())

# Define the colors for the custom colormap (Red to Purple gradient)
colors = ["#67a9cf", "#ffffbf", "#ef8a62"]
n_bins = 100  # Use 100 bins
cmap_name = "custom_colormap"
custom_colormap = LinearSegmentedColormap.from_list(cmap_name, colors, N=n_bins)

# Plot the new heatmap with the custom colormap (Red to Purple gradient)
plt.figure(figsize=(12, 8))
heatmap = sns.heatmap(new_heatmap_data, annot=True, fmt="d", linewidths=.5, cmap=custom_colormap, norm=new_norm)

# Set the y-axis labels to the desired labels
new_y_labels = ["LASSO", "SVR1", "SVR2", "SVR3", "SVR4", "DT1", "DT2", "RF", "Bagging", "GBM1", "GBM2", "LSTM", "GRU", "Ensemble"]
heatmap.set_yticklabels(new_y_labels, rotation=0)  # Rotate the labels for better readability

# Set the y-axis labels to the desired labels
new_x_labels = ["W1", "W2", "W3", "W4", "W5", "W6", "W7", "W8", "W9", "W10", "W11", "W12"]
heatmap.set_xticklabels(new_x_labels, rotation=0)  # Rotate the labels for better readability

# Add title and labels
plt.title('MAE values with different models within sample', fontsize=20)
plt.xlabel('Weeks of Prediction', fontsize=18)
plt.ylabel('Models', fontsize=18)

# Show the plot or save it as an image
os.chdir(root_dir + '\\Stage2\\Plots and Videos')
plt.savefig("MAE_WS_S2.png")

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from matplotlib.colors import LinearSegmentedColormap

plt.rcParams.update({'font.size': 15})


# Load the new data from the provided file
new_file_path = root_dir + '\\Stage2\\MAE_OOS_Precision.csv'
new_data = pd.read_csv(new_file_path)

# Get the new heatmap data and set the 'Unnamed: 0' column as the index
new_heatmap_data = new_data.set_index('Unnamed: 0')

# Convert the data to integers and ensure values rounded to 0 are set to 1
new_heatmap_data = new_heatmap_data.round().astype(int)
new_heatmap_data[new_heatmap_data == 0] = 1

# Normalize the new heatmap data by the range of the data to scale the color mapping
new_norm = plt.Normalize(vmin=new_heatmap_data.min().min(), vmax=new_heatmap_data.max().max())

# Define the colors for the custom colormap (Red to Purple gradient)
colors = ["#67a9cf", "#ffffbf", "#ef8a62"]
n_bins = 100  # Use 100 bins
cmap_name = "custom_colormap"
custom_colormap = LinearSegmentedColormap.from_list(cmap_name, colors, N=n_bins)

# Plot the new heatmap with the custom colormap (Red to Purple gradient)
plt.figure(figsize=(12, 8))
heatmap = sns.heatmap(new_heatmap_data, annot=True, fmt="d", linewidths=.5, cmap=custom_colormap, norm=new_norm)

# Set the y-axis labels to the desired labels
new_y_labels = ["LASSO", "SVR1", "SVR2", "SVR3", "SVR4", "DT1", "DT2", "RF", "Bagging", "GBM1", "GBM2", "LSTM", "GRU", "Ensemble"]
heatmap.set_yticklabels(new_y_labels, rotation=0)  # Rotate the labels for better readability

# Set the y-axis labels to the desired labels
new_x_labels = ["W1", "W2", "W3", "W4", "W5", "W6", "W7", "W8", "W9", "W10", "W11", "W12"]
heatmap.set_xticklabels(new_x_labels, rotation=0)  # Rotate the labels for better readability

# Add title and labels
plt.title('MAE values with different models out of sample', fontsize=20)
plt.xlabel('Weeks of Prediction', fontsize=18)
plt.ylabel('Models', fontsize=18)

# Show the plot or save it as an image
os.chdir(root_dir + '\\Stage2\\Plots and Videos')
plt.savefig("MAE_OOS_S2.png")

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from matplotlib.colors import LinearSegmentedColormap

plt.rcParams.update({'font.size': 15})

# Load the new data from the provided file
new_file_path = root_dir + '\\Stage2\\MAAPE_WS_Precision.csv'
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
os.chdir(root_dir + '\\Stage2\\Plots and Videos')
plt.savefig("MAAPE_WS_S2.png")

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from matplotlib.colors import LinearSegmentedColormap

plt.rcParams.update({'font.size': 15})

# Load the new data from the provided file using the directory variable
new_file_path = root_dir + '\\Stage2\\MAAPE_OOS_Precision.csv'

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
plt.title('MAAPE values with different models out of sample', fontsize = 20)
plt.xlabel('Weeks of Prediction', fontsize = 18)
plt.ylabel('Models',fontsize = 18)

# Show the plot or save it as an image
os.chdir(root_dir + '\\Stage2\\Plots and Videos')
plt.savefig("MAAPE_OOS_S2.png")

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from matplotlib.colors import LinearSegmentedColormap

plt.rcParams.update({'font.size': 15})


# Load the new data from the provided file
new_file_path = root_dir + '\\Stage2\\RMSE_WS_Precision.csv'
new_data = pd.read_csv(new_file_path)

# Get the new heatmap data and set the 'Unnamed: 0' column as the index
new_heatmap_data = new_data.set_index('Unnamed: 0')

# Convert the data to integers and ensure values rounded to 0 are set to 1
new_heatmap_data = new_heatmap_data.round().astype(int)
new_heatmap_data[new_heatmap_data == 0] = 1

# Normalize the new heatmap data by the range of the data to scale the color mapping
new_norm = plt.Normalize(vmin=new_heatmap_data.min().min(), vmax=new_heatmap_data.max().max())

# Define the colors for the custom colormap (Red to Purple gradient)
colors = ["#67a9cf", "#ffffbf", "#ef8a62"]
n_bins = 100  # Use 100 bins
cmap_name = "custom_colormap"
custom_colormap = LinearSegmentedColormap.from_list(cmap_name, colors, N=n_bins)

# Plot the new heatmap with the custom colormap (Red to Purple gradient)
plt.figure(figsize=(12, 8))
heatmap = sns.heatmap(new_heatmap_data, annot=True, fmt="d", linewidths=.5, cmap=custom_colormap, norm=new_norm)

# Set the y-axis labels to the desired labels
new_y_labels = ["LASSO", "SVR1", "SVR2", "SVR3", "SVR4", "DT1", "DT2", "RF", "Bagging", "GBM1", "GBM2", "LSTM", "GRU", "Ensemble"]
heatmap.set_yticklabels(new_y_labels, rotation=0)  # Rotate the labels for better readability

# Set the y-axis labels to the desired labels
new_x_labels = ["W1", "W2", "W3", "W4", "W5", "W6", "W7", "W8", "W9", "W10", "W11", "W12"]
heatmap.set_xticklabels(new_x_labels, rotation=0)  # Rotate the labels for better readability

# Add title and labels
plt.title('RMSE values with different models within sample', fontsize=20)
plt.xlabel('Weeks of Prediction', fontsize=18)
plt.ylabel('Models', fontsize=18)

# Show the plot or save it as an image
os.chdir(root_dir + '\\Stage2\\Plots and Videos')
plt.savefig("RMSE_WS_S2.png")

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from matplotlib.colors import LinearSegmentedColormap

plt.rcParams.update({'font.size': 15})


# Load the new data from the provided file
new_file_path = root_dir + '\\Stage2\\RMSE_OOS_Precision.csv'
new_data = pd.read_csv(new_file_path)

# Get the new heatmap data and set the 'Unnamed: 0' column as the index
new_heatmap_data = new_data.set_index('Unnamed: 0')

# Convert the data to integers and ensure values rounded to 0 are set to 1
new_heatmap_data = new_heatmap_data.round().astype(int)
new_heatmap_data[new_heatmap_data == 0] = 1

# Normalize the new heatmap data by the range of the data to scale the color mapping
new_norm = plt.Normalize(vmin=new_heatmap_data.min().min(), vmax=new_heatmap_data.max().max())

# Define the colors for the custom colormap (Red to Purple gradient)
colors = ["#67a9cf", "#ffffbf", "#ef8a62"]
n_bins = 100  # Use 100 bins
cmap_name = "custom_colormap"
custom_colormap = LinearSegmentedColormap.from_list(cmap_name, colors, N=n_bins)

# Plot the new heatmap with the custom colormap (Red to Purple gradient)
plt.figure(figsize=(12, 8))
heatmap = sns.heatmap(new_heatmap_data, annot=True, fmt="d", linewidths=.5, cmap=custom_colormap, norm=new_norm)

# Set the y-axis labels to the desired labels
new_y_labels = ["LASSO", "SVR1", "SVR2", "SVR3", "SVR4", "DT1", "DT2", "RF", "Bagging", "GBM1", "GBM2", "LSTM", "GRU", "Ensemble"]
heatmap.set_yticklabels(new_y_labels, rotation=0)  # Rotate the labels for better readability

# Set the y-axis labels to the desired labels
new_x_labels = ["W1", "W2", "W3", "W4", "W5", "W6", "W7", "W8", "W9", "W10", "W11", "W12"]
heatmap.set_xticklabels(new_x_labels, rotation=0)  # Rotate the labels for better readability

# Add title and labels
plt.title('RMSE values with different models out of sample', fontsize=20)
plt.xlabel('Weeks of Prediction', fontsize=18)
plt.ylabel('Models', fontsize=18)

# Show the plot or save it as an image
os.chdir(root_dir + '\\Stage2\\Plots and Videos')
plt.savefig("RMSE_OOS_S2.png")
