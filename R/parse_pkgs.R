#' Extract packages from a specified block in a default.nix file
#'
#' @param nix_file Path to the default.nix file (default: "default.nix")
#' @param project_path Path to root of project, typically "."
#' @param block_name Name of the block to parse (e.g., "rpkgs" or "pyconf")
#' @param transform Function to transform package names (default: identity)
#' @return List of package names, or NULL if the block is not found
#' @noRd
parse_packages <- function(
  nix_file,
  project_path,
  block_name,
  transform = identity
) {
  lines <- readLines(file.path(project_path, nix_file))

  # Find the start of the block (e.g., "rpkgs = builtins.attrValues {" or "pyconf = ...")
  start_pattern <- paste0(
    "^\\s*",
    block_name,
    "\\s*=\\s*builtins\\.attrValues\\s*\\{"
  )
  start_idx <- grep(start_pattern, lines)
  if (length(start_idx) == 0) {
    return(NULL)
  }
  start_idx <- start_idx[1]

  # Find the end of the block ("};")
  end_idx <- grep("^\\s*\\};", lines)
  end_idx <- end_idx[end_idx > start_idx][1]
  if (is.na(end_idx)) {
    stop(paste("Could not find the end of the", block_name, "block"))
  }

  # Extract and clean lines within the block
  block_lines <- lines[(start_idx + 1):(end_idx - 1)]
  block_lines <- gsub("#.*", "", block_lines) # Remove comments
  block_lines <- trimws(block_lines) # Trim whitespace
  block_lines <- block_lines[block_lines != ""] # Remove empty lines

  # Remove inherit statements (e.g., "inherit (pkgs.rPackages)" or "inherit (pkgs.python312Packages)")
  inherit_pattern <- "inherit \\(pkgs\\.[a-zA-Z0-9]+\\)"
  block_lines <- gsub(inherit_pattern, "", block_lines)

  block_lines <- gsub(";", "", block_lines) # Remove semicolons

  # Split into package names and apply transformation
  packages <- unlist(strsplit(paste(block_lines, collapse = " "), "\\s+"))
  packages <- packages[packages != ""]

  transform(packages)
}

#' Generate a script with import statements and optional additional content
#'
#' @param packages List of package names
#' @param additional_files Character vector of additional files to include
#' @param outfile Path to the output file
#' @param import_formatter Function to format import statements
#' @param additional_file_pattern Regex pattern to identify additional files
#' @return Writes a script to the specified outfile
#' @noRd
generate_libraries_script <- function(
  packages,
  additional_files,
  outfile,
  import_formatter,
  additional_file_pattern
) {
  # Generate import statements
  import_lines <- sapply(packages, import_formatter)

  # Filter additional files based on the pattern
  additional_scripts <- Filter(
    function(x) grepl(additional_file_pattern, x),
    additional_files
  )

  # Append additional content if present
  if (length(additional_scripts) > 0) {
    list_additional_content <- lapply(additional_scripts, readLines)
    additional_content <- Reduce(append, list_additional_content)
    output <- append(import_lines, additional_content)
  } else {
    output <- import_lines
  }

  writeLines(output, outfile)
}

# R-specific helpers
transform_r <- function(packages) {
  gsub("_", ".", packages) # Replace _ with . for R packages
}

adjust_r_packages <- identity # No additional adjustments for R

import_formatter_r <- function(package) {
  paste0("library(", package, ")") # R import format
}

# Python-specific helpers
adjust_py_packages <- function(packages) {
  packages <- packages[!(packages %in% c("pip", "ipykernel"))] # Exclude pip and ipykernel
  packages <- sort(c("pickle", packages)) # Add pickle
  packages <- gsub("scikit-learn", "sklearn", packages) # Replace scikit-learn with sklearn
  packages
}

import_formatter_py <- function(package) {
  paste0("import ", package) # Python import format
}

#' Generate a script with import statements from a default.nix file
#'
#' @param nix_file Path to the default.nix file (default: "default.nix")
#' @param additional_files Character vector of additional files to include
#' @param project_path Path to root of project, typically "."
#' @param language Language to generate the script for ("R" or "Python")
#' @return A script file for the specified language, or NULL if no packages are found
#' @noRd
generate_r_or_py_libraries_from_nix <- function(
  nix_file,
  additional_files = "",
  project_path,
  language
) {
  # Set language-specific parameters
  if (language == "R") {
    block_name <- "rpkgs"
    transform <- transform_r
    adjust <- adjust_r_packages
    import_formatter <- import_formatter_r
    additional_file_pattern <- "functions\\.[Rr]"
    extension <- "R"
  } else if (language == "Python") {
    block_name <- "pyconf"
    transform <- identity
    adjust <- adjust_py_packages
    import_formatter <- import_formatter_py
    additional_file_pattern <- "functions\\.py"
    extension <- "py"
  } else {
    stop("Unsupported language")
  }

  # Parse packages
  packages <- parse_packages(nix_file, project_path, block_name, transform)
  if (is.null(packages)) {
    return(NULL)
  }

  # Adjust packages
  packages <- adjust(packages)

  # Generate output file name
  nix_file_name <- gsub("[^a-zA-Z0-9]", "_", nix_file)
  nix_file_name <- sub("_nix$", "", nix_file_name)
  outfile <- file.path(
    project_path,
    "/_rixpress/",
    paste0(nix_file_name, "_libraries.", extension)
  )

  # Generate the script
  generate_libraries_script(
    packages,
    additional_files,
    outfile,
    import_formatter,
    additional_file_pattern
  )
}

generate_r_libraries_from_nix <- function(
  nix_file,
  additional_files = "",
  project_path
) {
  generate_r_or_py_libraries_from_nix(
    nix_file,
    additional_files,
    project_path,
    "R"
  )
}

generate_py_libraries_from_nix <- function(
  nix_file,
  additional_files = "",
  project_path
) {
  generate_r_or_py_libraries_from_nix(
    nix_file,
    additional_files,
    project_path,
    "Python"
  )
}

#' Adjust Python import statements
#'
#' @param old_import Import statement to replace (e.g., "import pillow")
#' @param new_import New import statement (e.g., "from PIL import Image")
#' @return No return value; modifies files in-place
#' @export
adjust_imports <- function(old_import, new_import) {
  files <- list.files(path = "_rixpress", full.names = TRUE, recursive = TRUE)
  for (file in files) {
    content <- readLines(file, warn = FALSE)
    new_content <- gsub(old_import, new_import, content, fixed = TRUE)
    writeLines(new_content, con = file)
  }
}
