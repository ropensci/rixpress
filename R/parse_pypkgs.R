#' Extract Python packages from a default.nix file
#'
#' @param nix_file Path to the default.nix file (default: "default.nix")
#' @param project_path Path to root of project, typically "."
#' @return List of Python packages defined in the pypkgs block of a default.nix file
#' @noRd
parse_pypkgs <- function(nix_file, project_path) {
  # Read the file as lines
  lines <- readLines(file.path(project_path, nix_file))

  # Find the starting index of the pypkgs block
  start_idx <- grep("^\\s*pypkgs\\s*=\\s*builtins\\.attrValues\\s*\\{", lines)
  if (length(start_idx) == 0) {
    stop("pypkgs block not found in the file")
  }
  start_idx <- start_idx[1]

  # Find the end of the pypkgs block (a line that starts with "};")
  end_idx <- grep("^\\s*\\};", lines)
  end_idx <- end_idx[end_idx > start_idx][1]
  if (is.na(end_idx)) {
    stop("Could not find the end of the pypkgs block")
  }

  # Extract lines within the block
  block_lines <- lines[(start_idx + 1):(end_idx - 1)]

  # Remove comments and trim whitespace
  block_lines <- gsub("#.*", "", block_lines)
  block_lines <- trimws(block_lines)

  # Remove any empty lines
  block_lines <- block_lines[block_lines != ""]

  # Remove the "inherit (pkgs.python312Packages)" phrase if present
  # Use a regex to match any Python version (e.g., python311Packages, python312Packages)
  block_lines <- gsub("inherit \\(pkgs\\.python[0-9]+Packages\\)", "", block_lines)

  # Remove semicolon characters
  block_lines <- gsub(";", "", block_lines)

  # Combine all lines into one string and split by whitespace
  packages <- unlist(strsplit(paste(block_lines, collapse = " "), "\\s+"))

  # Remove empty strings if any
  packages <- packages[packages != ""]

  # Unlike R, Python package names typically don't need transformation (e.g., _ to .)
  # Return the packages as-is
  packages
}

#' Generate a Python script with import statements for Python packages
#'
#' @param packages List of Python package names
#' @param outfile Path to the output file, typically in "_rixpress/"
#' @return A Python script with import statements for the specified packages
#' @noRd
generate_python_libraries_script <- function(packages, outfile) {
  # Generate "import package_name" for each package
  import_lines <- paste0("import ", packages)
  
  # Write the import statements to the output file
  writeLines(import_lines, outfile)
}

#' Generate a Python script with import statements from a default.nix file
#'
#' @param nix_file Path to the default.nix file (default: "default.nix")
#' @param project_path Path to root of project, typically "."
#' @return A Python script to import the libraries inside of derivations
#' @noRd
generate_python_libraries_from_nix <- function(nix_file, project_path) {
  # Extract the Python packages from the nix file
  packages <- parse_pypkgs(nix_file, project_path)
  
  # Determine the output file name based on the nix_file
  nix_file_name <- gsub("\\.nix", "", nix_file)
  output_file <- file.path(
    project_path,
    "_rixpress",
    paste0(nix_file_name, "_libraries.py")
  )
  
  # Ensure the _rixpress directory exists
  dir.create(dirname(output_file), showWarnings = FALSE, recursive = TRUE)
  
  # Generate the Python script
  generate_python_libraries_script(packages, output_file)
}
