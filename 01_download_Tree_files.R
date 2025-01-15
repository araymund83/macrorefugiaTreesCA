# Load necessary libraries
library(googledrive)
library(purrr)
library(fs) # For directory creation

# Authenticate with Google Drive
drive_auth()

# Define folder ID of the Google Drive folder
folder_id <- "15sod3AB8zPJbHJvv8kKdghlukfYtOhi5"  # Replace with your folder ID

# Recursively list all files and folders in the Google Drive folder
files <- drive_ls(as_id(folder_id))



# Loop through all items in the 'files' dribble
for (i in seq_len(nrow(files))) {
  file <- files[i, ]
  
  # Extract folder or file name and MIME type
  folder_name <- file$name  # Tree species name
  mime_type <- file$drive_resource[[1]]$mimeType
  
  if (mime_type == "application/vnd.google-apps.folder") {
    # If the item is a folder, list its contents
    folder_files <- googledrive::drive_ls(as_id(file$id))
    
    # Create a local folder for the tree species if it doesn't exist
    if (!dir.exists(folder_name)) {
      dir.create(folder_name)
    }
    
    # Download all files within the folder
    for (subfile in seq_len(nrow(folder_files))) {
      subfile_name <- folder_files$name[subfile]  # Get the subfile name
      subfile_id <- folder_files$id[subfile]     # Get the subfile ID
      
      # Define the custom path
      local_path <- file.path(folder_name, subfile_name)
      
      # Download the file to the custom path
      googledrive::drive_download(as_id(subfile_id), path = local_path)
    }
  } else {
    # If the item is a file, download it (optional, if files outside folders exist)
    googledrive::drive_download(as_id(file$id))
  }
}

# Define the root directory where your files are located
root_dir <- "inputs/tree_spp/tree_fut3/west"

# Define the target directory for saving renamed files
output_dir <- "inputs/tree_spp/pres_tree_3/west"

# Ensure the target directory exists, create if not
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}
##### THIS CODE EXTRACTS THE NORMAL FILES FROM EACH SP SUBDIRECTORY AND RENAMES THE FILE
# List all files recursively
all_files <- list.files(root_dir, recursive = TRUE, full.names = TRUE)

# Filter files with "NORMAL_" in their names
normal_files <- all_files[grepl("Normal_", basename(all_files))]

# Function to process each file
process_file <- function(file_path) {
  # Extract the subfolder name
  subfolder_name <- dirname(file_path)  # Gets the directory path
  subfolder_name <- basename(subfolder_name)  # Extracts the folder name
  
  # Extract the original file name
  file_name <- basename(file_path)
  
  # Create the new file name with subfolder name appended
  new_file_name <- paste0(subfolder_name, "_", file_name)
  
  # Define the new file path in the target directory
  new_file_path <- file.path(output_dir, new_file_name)
  
  # Copy the file to the target directory with the new name
  file.copy(file_path, new_file_path, overwrite = TRUE)
  
  return(new_file_path)
}

# Apply the process function to all files with NORMAL_ in their names
new_files <- lapply(normal_files, process_file)

# Print confirmation
cat("Files have been renamed and moved to:", output_dir, "\n")
