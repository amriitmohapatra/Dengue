pip install os
pip install sys
pip install geopandas
pip install pandas
pip install matplotlib
pip install numpy
pip install shapely
pip install moviepy
pip install datetime
import os
import sys
os.getcwd()
folder_path = sys.argv[1]
os.chdir(folder_path)

import geopandas as gpd
import pandas as pd
import matplotlib.pyplot as plt
from matplotlib.colors import LinearSegmentedColormap, TwoSlopeNorm
import numpy as np
import os
from datetime import datetime
from moviepy.editor import ImageSequenceClip

#Load geographic information data
sg_regions_geojson_path = folder_path + "\\modified_sg_regions.geojson"
sg_regions_gdf = gpd.read_file(sg_regions_geojson_path)


#Create custom colormap
colors = ["#2805AE", "#FFFFFF", "#AE0505"]
cmap = LinearSegmentedColormap.from_list("custom_gradient", colors, N=256)


# Define the function for drawing weekly data graphs
def plot_week(sg_regions_gdf, case_data_df, week_of_year, year, img_number, vmin, vmax, dataset_number):
    week_data = case_data_df[case_data_df['week_of_year'] == week_of_year]
    week_data_cases = week_data.drop(['week number', 'start date', 'end date', 'year', 'week_of_year'], axis=1).melt(
        var_name='pln_area_n', value_name='cases')
    week_data_cases['pln_area_n'] = week_data_cases['pln_area_n'].str.title()  # Format area name
    sg_regions_gdf['pln_area_n'] = sg_regions_gdf['pln_area_n'].str.title()
    merged_gdf = sg_regions_gdf.merge(week_data_cases, on='pln_area_n', how='left')

    fig, ax = plt.subplots(1, 1, figsize=(10, 6))
    norm = TwoSlopeNorm(vmin=-120, vcenter=0, vmax=120) # Set normalization
    merged_gdf.plot(column='cases', cmap=cmap, linewidth=0.8, ax=ax, edgecolor='0.8',
                    missing_kwds={"color": "lightgrey", "label": "Missing values"},
                    norm=norm)

    ax.set_axis_off()
    ax.set_title(f'{year} week {week_of_year}', fontsize=14)
    sm = plt.cm.ScalarMappable(cmap=cmap, norm=norm)
    sm.set_array([])
    cbar = fig.colorbar(sm, ax=ax, orientation='horizontal', fraction=0.046, pad=0.04)
    cbar.set_label('Difference')

    image_path = folder_path + f'\\with_{dataset_number}_map_week_{img_number}.png'
    plt.savefig(image_path, dpi=300)
    plt.close()
    return image_path


# Process all datasets
for dataset_number in range(1, 13):
    case_data_path = folder_path + f'\\W_{dataset_number}_Deviation_WithinSample.csv'
    case_data_df = pd.read_csv(case_data_path)

    # Convert date format and extract year and week number
    case_data_df['end date'] = pd.to_datetime(case_data_df['end date'], format='%d/%m/%Y')
    case_data_df['year'] = case_data_df['end date'].dt.year
    case_data_df['week_of_year'] = case_data_df['end date'].dt.isocalendar().week

    # Get the global minimum and maximum number of cases
    global_cases = case_data_df.drop(['week number', 'start date', 'end date', 'year', 'week_of_year'], axis=1)
    global_min_cases = global_cases.min().min()
    global_max_cases = global_cases.max().max()

    # Adjust vmin and vmax to ensure symmetry of the scale
    max_abs_cases = max(abs(global_min_cases), abs(global_max_cases))
    vmin = -max_abs_cases
    vmax = max_abs_cases

    # Draw the map of all weeks and save the file path
    image_paths = [
        plot_week(sg_regions_gdf, case_data_df, row['week_of_year'], row['year'], index + 1, vmin, vmax, dataset_number)
        for index, row in case_data_df.iterrows()]

    output_video_path = folder_path + f'\\fix_with_{dataset_number}.mp4'
    clip = ImageSequenceClip(image_paths, fps=1)  # Adjust frame rate as needed
    clip.write_videofile(output_video_path, codec='libx264')

    # Delete all images
    for image_path in image_paths:
        try:
            os.remove(image_path)
            print(f"Deleted: {image_path}")
        except OSError as e:
            print(f"Error: {image_path} : {e.strerror}")

print("All videos created and images deleted successfully.")
