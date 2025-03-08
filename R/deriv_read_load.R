# Function to read an RDS file from the "result" directory
derive_read <- function(derivation_name) {
  # Resolve the "result" symlink to get the directory path
  result_path <- Sys.readlink("result")
  if (result_path == "") {
    stop("Symlink 'result' not found or not a symlink")
  }
  
  # Check if the result_path is a directory
  if (!dir.exists(result_path)) {
    stop("The 'result' path is not a directory")
  }
  
  # List all files in the directory with full paths
  files <- list.files(result_path, full.names = TRUE)
  
  # Use grepl to find files matching the derivation_name and ending with .rds
  pattern <- paste0(derivation_name, ".*\\.rds$")
  matching_files <- files[grepl(pattern, basename(files))]
  
  # If no matching files are found, stop with an error
  if (length(matching_files) == 0) {
    stop(paste("No matching RDS file found for derivation:", derivation_name))
  }
  
  # Read and return the first matching RDS file
  readRDS(matching_files[1])
}

# Function to load an RDS file and assign it to the global environment
deriv_load <- function(derivation_name) {
  # Resolve the "result" symlink to get the directory path
  result_path <- Sys.readlink("result")
  if (result_path == "") {
    stop("Symlink 'result' not found or not a symlink")
  }
  
  # Check if the result_path is a directory
  if (!dir.exists(result_path)) {
    stop("The 'result' path is not a directory")
  }
  
  # List all files in the directory with full paths
  files <- list.files(result_path, full.names = TRUE)
  
  # Use grepl to find files matching the derivation_name and ending with .rds
  pattern <- paste0(derivation_name, ".*\\.rds$")
  matching_files <- files[grepl(pattern, basename(files))]
  
  # If no matching files are found, stop with an error
  if (length(matching_files) == 0) {
    stop(paste("No matching RDS file found for derivation:", derivation_name))
  }
  
  # Read the first matching RDS file and assign it to the global environment
  assign(derivation_name, readRDS(matching_files[1]), envir = .GlobalEnv)
}
