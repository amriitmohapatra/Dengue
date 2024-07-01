pip install os
pip install sys
pip install geopandas
pip install pandas
pip install matplotlib
pip install numpy
pip install shapely
pip install moviepy


import os
import sys
os.getcwd()
folder_path = sys.argv[1]
os.chdir(folder_path)

import geopandas as gpd
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.colors as mcolors
from matplotlib.lines import Line2D
import numpy as np
from shapely.geometry import Point
from matplotlib.colors import LinearSegmentedColormap
from moviepy.editor import ImageSequenceClip
import os

# Create a custom colormap
colors = [(0,"#64B5F6"),(0.05,"#81C784"),(0.1,"#FFF176"),(0.2,"#FFD54F"),(0.5,"#FFB74D"),(1,"#BF360C")] # Green to yellow to red
cmap = LinearSegmentedColormap.from_list("custom_gradient", colors, N=256)

# Load the GeoJSON file
sg_regions_geojson_path = folder_path + "\\modified_sg_regions.geojson"
sg_regions_gdf = gpd.read_file(sg_regions_geojson_path)

def generate_uniform_points_in_polygon(polygon, num_points):
    min_x, min_y, max_x, max_y = polygon.bounds
    width = max_x - min_x
    height = max_y - min_y

    # Calculate a denser grid size to ensure there are enough points
    grid_x = int(np.ceil(np.sqrt(num_points))) + 6 # Add extra lines
    grid_y = int(np.ceil(np.sqrt(num_points))) + 6  # Add additional columns

    # Calculate the width and height of grid cells
    dx = width / grid_x
    dy = height / grid_y

    
    # Determine the center point of the polygon
    center_x, center_y = polygon.centroid.x, polygon.centroid.y

    #Create a sorted list of candidate points
    candidate_points = []
    for i in range(grid_x):
        for j in range(grid_y):
            # Generate points in the grid
            x = min_x + i * dx
            y = min_y + j * dy
            candidate_points.append((x, y, (x - center_x)**2 + (y - center_y)**2))  # Record the distance between each point and the center point

    
    # Sort by distance from center point
    candidate_points.sort(key=lambda x: x[2])

    # Select the point closest to the center until the desired number of points is reached
    points = []
    for point in candidate_points:
        if len(points) >= num_points:
            break
        if polygon.contains(Point(point[0], point[1])):
            points.append(Point(point[0], point[1]))

    return points


# Adjusted function to plot a map for a given week number
def plot_week(week_number, vmin, vmax, prediction_data_df, true_case_data_df, output_prefix, sg_regions_gdf):
    week_end_date = prediction_data_df[prediction_data_df['week number'] == week_number]['end date'].iloc[0]
    year_week = pd.to_datetime(week_end_date, dayfirst=True).isocalendar()
    year = year_week[0]
    week_of_year = year_week[1]

    week_data_predictions = prediction_data_df[prediction_data_df['week number'] == week_number].copy()
    week_data_predictions = week_data_predictions.drop(['week number', 'start date', 'end date'], axis=1).melt(var_name='pln_area_n', value_name='predictions')
    week_data_predictions['pln_area_n'] = week_data_predictions['pln_area_n'].str.title()

    week_data_true = true_case_data_df[true_case_data_df['week number'] == week_number].copy()
    week_data_true = week_data_true.drop(['week number', 'start date', 'end date'], axis=1).melt(var_name='pln_area_n', value_name='true_cases')
    week_data_true['pln_area_n'] = week_data_true['pln_area_n'].str.title()

    sg_regions_gdf['pln_area_n'] = sg_regions_gdf['pln_area_n'].str.title()
    merged_gdf = sg_regions_gdf.merge(week_data_predictions, on='pln_area_n', how='left').merge(week_data_true, on='pln_area_n', how='left')

    fig, ax = plt.subplots(1, 1, figsize=(10, 6))
    merged_gdf.plot(column='predictions', cmap=cmap, linewidth=0.8, ax=ax, edgecolor='0.8', missing_kwds={"color": "lightgrey", "label": "Missing values"}, vmin=0, vmax=150)

    for _, row in merged_gdf.iterrows():
        if np.isnan(row['true_cases']) or row['true_cases'] == 0:
            continue
        points = generate_uniform_points_in_polygon(row['geometry'], int(row['true_cases']))
        for point in points:
            ax.plot(point.x, point.y, 'k.', markersize=2)

    ax.set_axis_off()
    ax.set_title(f'{year} week {week_of_year}', fontsize=14)
    norm = mcolors.Normalize(vmin=0, vmax=106)  # Start from 0
    sm = plt.cm.ScalarMappable(cmap=cmap, norm=norm)
    sm.set_array([])
    cbar = fig.colorbar(sm, ax=ax, orientation='horizontal', fraction=0.046, pad=0.04)
    cbar.set_label('Number of Predicted Cases', fontsize=10)  # Adjusted font size

    image_path = folder_path + f'\\{output_prefix}_map_week_{week_number}.png'  # Update the save path as needed
    plt.savefig(image_path, dpi=300)
    plt.close()
    return image_path

for i in range(1, 13):
    prediction_data_path = folder_path + f'\\W_{i}_Predictions_WithinSample.csv'
    true_case_data_path = folder_path + f'\\W_{i}_True_WithinSample.csv'

    prediction_data_df = pd.read_csv(prediction_data_path)
    true_case_data_df = pd.read_csv(true_case_data_path)

    global_cases = prediction_data_df.drop(['week number', 'start date', 'end date'], axis=1)
    global_min_cases = global_cases.min().min()
    global_max_cases = global_cases.max().max()

    week_numbers = prediction_data_df['week number'].unique()
    image_paths = []
    for week in week_numbers:
        image_path = plot_week(week, global_min_cases, global_max_cases, prediction_data_df, true_case_data_df, f'with_{i}', sg_regions_gdf)
        image_paths.append(image_path)

    output_video_path = folder_path + f'\\fix_with_{i}.mp4'
    clip = ImageSequenceClip(image_paths, fps=1)
    clip.write_videofile(output_video_path, codec='libx264')

    for image_path in image_paths:
        os.remove(image_path)
    print(f"Video out_{i}.mp4 created and images deleted successfully.")

