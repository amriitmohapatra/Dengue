import os
import shutil
from moviepy.editor import ImageSequenceClip
folder_path = "C:\\Users\\amritm\\nBox\\May-Jun 2024\\Final Code\\Stage1\\Plots"
os.chdir(folder_path)


def delete_folder(folder_path):
    # Check if the folder exists
    if os.path.exists(folder_path):
        # Use shutil.rmtree to delete the folder and all its contents
        shutil.rmtree(folder_path)
        print(f"Folder '{folder_path}' and all its contents have been deleted.")
    else:
        print(f"Folder '{folder_path}' does not exist.")


image_folder = folder_path + '\\WSPlots'
# Create a list of image file paths in the correct order
image_files = [os.path.join(image_folder, f'WS{i}.png') for i in range(1, 242)]  # Adjust the range as needed
# Check if all the files exist
for image_file in image_files:
    if not os.path.exists(image_file):
        raise FileNotFoundError(f"{image_file} does not exist")

# Create a video clip from the image sequence
clip = ImageSequenceClip(image_files, fps=1)  # Adjust fps (frames per second) as needed

# Write the video clip to a file
output_video_path = os.path.join(folder_path, 'Figure1_WS.mp4')
clip.write_videofile(output_video_path, codec='libx264')

# Delete folder once done
delete_folder(image_folder)


image_folder = folder_path + '\\OOSPlots'
# Create a list of image file paths in the correct order
image_files = [os.path.join(image_folder, f'OOS{i}.png') for i in range(1, 53)]  # Adjust the range as needed
# Check if all the files exist
for image_file in image_files:
    if not os.path.exists(image_file):
        raise FileNotFoundError(f"{image_file} does not exist")

# Create a video clip from the image sequence
clip = ImageSequenceClip(image_files, fps=1)  # Adjust fps (frames per second) as needed

# Write the video clip to a file
output_video_path = os.path.join(folder_path, 'Figure1_OOS.mp4')
clip.write_videofile(output_video_path, codec='libx264')

# Delete folder once done
delete_folder(image_folder)
