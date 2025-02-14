# Install and load required libraries
install.packages("magick")
library(magick)

# Set the directory containing the images
image_dir <- "/path/to/your/images"

# List all image files in the directory
image_files <- list.files(image_dir, pattern = ".jpg|.jpeg|.png", full.names = TRUE)

# Function to zoom in and cut the middle part of an image
zoom_and_cut <- function(image_file, output_dir) {
  # Read the image
  img <- image_read(image_file)
  
  # Get the dimensions of the image
  img_width <- image_info(img)$width
  img_height <- image_info(img)$height
  
  # Calculate the dimensions for zooming and cutting (e.g., 50% zoom)
  zoom_width <- img_width * 0.5
  zoom_height <- img_height * 0.5
  
  # Zoom in on the image
  img_zoomed <- image_scale(img, geometry = paste0(zoom_width, "x", zoom_height))
  
  # Calculate the coordinates for cutting the middle part
  cut_width <- zoom_width / 2
  cut_height <- zoom_height / 2
  
  # Cut the middle part of the zoomed image
  img_cut <- image_crop(img_zoomed, geometry = paste0("+", cut_width, "+", cut_height))
  
  # Save the resulting image
  output_file <- file.path(output_dir, basename(image_file))
  image_write(img_cut, path = output_file)
}

# Output directory for saving the resulting images
output_dir <- "/path/to/your/output/directory"

# Create the output directory if it doesn't exist
if (!file.exists(output_dir)) {
  dir.create(output_dir)
}

# Apply the zoom_and_cut function to all image files
for (file in image_files) {
  zoom_and_cut(file, output_dir)
}
