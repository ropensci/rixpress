#' Extract Packages from a Specified Block in a default.nix File
#'
#' @param nix_file Path to the default.nix file
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
  file_path <- file.path(project_path, nix_file)
  if (!file.exists(file_path)) {
    warning(paste("File not found:", file_path))
    return(NULL)
  }

  lines <- readLines(file_path)

  # Find the start of the block
  # (e.g., "rpkgs = builtins.attrValues {" or "pyconf = ...")
  start_pattern <- paste0(
    "^\\s*",
    block_name,
    "\\s*=\\s*(?:",
    "builtins\\.attrValues\\s*\\{", # matches rpkgs (or pyconf) = builtins.attrValues {
    "|", # or, matches Julia blocks
    "pkgs\\.julia(?:[-_\\.][A-Za-z0-9]+)*\\.withPackages\\s*\\[",
    ")"
  )

  start_idx <- grep(start_pattern, lines)
  if (length(start_idx) == 0) {
    return(NULL)
  }
  start_idx <- start_idx[1]

  # Find the end of the block ("};" or "];" or "} ++ [ ... ];")
  # We look for a closing brace/bracket followed by optional ++ list, ending with ;
  end_idxs <- grep("^\\s*(\\}|\\])(\\s*\\+\\+.*\\])?;\\s*$", lines, perl = TRUE)
  end_idx <- end_idxs[end_idxs > start_idx][1]

  if (is.na(end_idx)) {
    stop(paste("Could not find the end of the", block_name, "block"))
  }

  # Extract lines. If the end line contains packages (e.g. in ++ list), include it.
  end_line <- lines[end_idx]
  if (grepl("\\+\\+", end_line)) {
    block_lines <- lines[(start_idx + 1):end_idx]
  } else {
    block_lines <- lines[(start_idx + 1):(end_idx - 1)]
  }

  block_lines <- gsub("#.*", "", block_lines) # Remove comments
  block_lines <- trimws(block_lines) # Remove whitespace
  block_lines <- block_lines[block_lines != ""] # Remove empty lines

  # Remove inherit statements
  # (e.g., "inherit (pkgs.rPackages)" or "inherit (pkgs.python312Packages)")
  inherit_pattern <- "inherit \\(pkgs\\.[a-zA-Z0-9]+\\)"
  block_lines <- gsub(inherit_pattern, "", block_lines)

  # Clean up structural tokens
  # Remove semicolons, braces, brackets, equals sign, builtins, ++, etc.
  block_lines <- gsub(";", " ", block_lines, fixed = TRUE)
  block_lines <- gsub("=", " ", block_lines, fixed = TRUE)
  block_lines <- gsub("{", " ", block_lines, fixed = TRUE)
  block_lines <- gsub("}", " ", block_lines, fixed = TRUE)
  block_lines <- gsub("[", " ", block_lines, fixed = TRUE)
  block_lines <- gsub("]", " ", block_lines, fixed = TRUE)
  block_lines <- gsub("+", " ", block_lines, fixed = TRUE)
  block_lines <- gsub("builtins\\.attrValues", " ", block_lines)

  # Split into package names and apply transformation
  packages <- unlist(strsplit(paste(block_lines, collapse = " "), "\\s+"))
  packages <- packages[packages != ""]

  transform(packages)
}

#' Extract R Packages Defined via pkgs.rPackages.buildRPackage from Git Sources
#'
#' @param nix_file Path to the default.nix file
#' @param project_path Path to root of project, typically "."
#' @param transform Function to transform package names (default: identity)
#' @return List of R package names from git, or NULL if none are found
#' @noRd
parse_rpkgs_git <- function(
  nix_file,
  project_path,
  transform = identity
) {
  file_path <- file.path(project_path, nix_file)
  if (!file.exists(file_path)) {
    warning(paste("File not found:", file_path))
    return(NULL)
  }

  lines <- readLines(file_path)
  git_packages <- c()

  # Pattern to find the start of a buildRPackage definition
  # e.g., my_pkg = (pkgs.rPackages.buildRPackage {
  build_r_pkg_start_pattern <- "^\\s*([a-zA-Z0-9_.-]+)\\s*=\\s*\\(pkgs\\.rPackages\\.buildRPackage\\s*\\{"
  start_indices <- grep(build_r_pkg_start_pattern, lines)

  if (length(start_indices) == 0) {
    return(NULL)
  }

  # Pattern for the end of these blocks is typically "});"
  end_block_pattern <- "^\\s*\\}\\);"

  for (start_idx in start_indices) {
    # Find the corresponding end of this specific block
    potential_end_indices <- grep(
      end_block_pattern,
      lines[(start_idx + 1):length(lines)],
      fixed = FALSE
    )
    if (length(potential_end_indices) == 0) {
      next # No proper end found for this block, skip
    }
    # The index is relative to lines[(start_idx + 1):length(lines)], so adjust
    end_idx <- start_idx + potential_end_indices[1]

    block_lines <- lines[(start_idx + 1):(end_idx - 1)]

    # Check if this block contains a src = pkgs.fetchgit (or similar)
    # Covers fetchgit, fetchFromGitHub, fetchFromGitLab
    fetch_pattern <- "^\\s*src\\s*=\\s*pkgs\\.(fetchgit|fetchFromGitHub|fetchFromGitLab)\\s*\\{"
    if (any(grepl(fetch_pattern, block_lines))) {
      # If it's a git source, look for the 'name' attribute
      name_pattern <- "^\\s*name\\s*=\\s*\"([a-zA-Z0-9_.-]+)\"\\s*;"
      name_lines_match <- grep(name_pattern, block_lines, value = TRUE)

      if (length(name_lines_match) > 0) {
        # Extract the package name (the part in quotes)
        pkg_name <- sub(name_pattern, "\\1", name_lines_match[1])
        git_packages <- c(git_packages, pkg_name)
      }
    }
  }

  if (length(git_packages) == 0) {
    return(NULL)
  }

  transform(unique(git_packages))
}


#' Generate a Script with Import Statements and Optional Additional Content
#'
#' @param packages List of package names
#' @param additional_files Character vector of additional files to include
#' @param outfile Path to the output file
#' @param import_formatter Function to format import statements
#' @param additional_file_pattern Regex pattern to identify additional files
#' @return Writes a script to the specified outfile and returns the path
#' @noRd
generate_libraries_script <- function(
  packages,
  additional_files,
  outfile,
  import_formatter,
  additional_file_pattern
) {
  # Generate import statements for each package
  import_lines <- sapply(packages, import_formatter)

  # Filter additional files based on pattern (only if pattern is not NULL)
  if (!is.null(additional_file_pattern)) {
    additional_scripts <- Filter(
      function(x) grepl(additional_file_pattern, x),
      additional_files
    )

    # Combine import statements with additional content
    if (length(additional_scripts) > 0) {
      list_additional_content <- lapply(additional_scripts, readLines)
      additional_content <- Reduce(append, list_additional_content)
      output <- append(import_lines, additional_content)
    } else {
      output <- import_lines
    }
  } else {
    output <- import_lines
  }

  # Create output directory if it doesn't exist
  output_dir <- dirname(outfile)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Write the output to file
  writeLines(output, outfile)

  return(outfile)
}

#' @noRd
transform_r <- function(packages) {
  gsub("_", ".", packages) # in nixpkgs: data_table, but in CRAN data.table
}

#' @noRd
adjust_r_packages <- identity # No additional adjustments for R

#' @noRd
import_formatter_r <- function(package) {
  paste0("library(", package, ")") # R import format
}

#' @noRd
adjust_py_packages <- function(packages) {
  # Only needed for interactive work
  packages <- packages[!(packages %in% c("pip", "ipykernel"))]
  # Pickle is needed to import pickle’d objects
  packages <- sort(c("pickle", packages))
  # Scikit is a common package where its name is different
  # when importing
  packages <- gsub("scikit-learn", "sklearn", packages)
  packages
}

#' @noRd
transform_py <- function(packages) {
  packages # No transformation by default
}

#' @noRd
import_formatter_py <- function(package) {
  paste0("import ", package)
}

#' @noRd
transform_jl <- function(packages) {
  packages # No transformation by default
}

#' @noRd
adjust_jl_packages <- function(packages) {
  # Serialization is needed to import pickle’d objects
  packages <- sort(c("Serialization", packages))
  # Julia Packages are passed as strings in the nix expression
  packages <- gsub("\"", "", packages)
  packages
}

#' @noRd
import_formatter_jl <- function(package) {
  paste0("using ", package)
}

#' Configuration for Supported Languages
#'
#' This list defines the configuration for each supported language,
#' including block names, file patterns, and function references.
#' @noRd
language_configs <- list(
  "R" = list(
    block_name = "rpkgs",
    transform_func = transform_r,
    adjust_func = adjust_r_packages,
    import_formatter = import_formatter_r,
    additional_file_pattern = NULL, # Changed from "functions\\.[Rr]"
    extension = "R",
    parse_git = TRUE
  ),
  "Python" = list(
    block_name = "pyconf",
    transform_func = transform_py,
    adjust_func = adjust_py_packages,
    import_formatter = import_formatter_py,
    additional_file_pattern = NULL, # Changed from "functions\\.[py]"
    extension = "py",
    parse_git = FALSE
  ),
  "Julia" = list(
    block_name = "jlconf",
    transform_func = transform_jl,
    adjust_func = adjust_jl_packages,
    import_formatter = import_formatter_jl,
    additional_file_pattern = NULL, # Changed from "functions\\.[jl]"
    extension = "jl",
    parse_git = FALSE
  )
)

#' Generate a Script with Import Statements from a default.nix File
#'
#' @param nix_file Path to the default.nix file
#' @param additional_files Character vector of additional files to include
#' @param project_path Path to root of project, typically "."
#' @param language Language to generate the script for ("R", "Python", or "Julia")
#' @return A script file path for the specified language,
#'   or NULL if no packages are found
#' @noRd
generate_r_or_py_libraries_from_nix <- function(
  nix_file,
  additional_files = "",
  project_path,
  language
) {
  # Validate language parameter
  if (!language %in% names(language_configs)) {
    stop(paste(
      "Unsupported language:",
      language,
      "- Must be one of:",
      paste(names(language_configs), collapse = ", ")
    ))
  }

  # Get language configuration
  config <- language_configs[[language]]
  all_parsed_packages <- c()

  # Parse packages from the main block
  packages_from_block <- parse_packages(
    nix_file = nix_file,
    project_path = project_path,
    block_name = config$block_name,
    transform = config$transform_func
  )
  if (!is.null(packages_from_block)) {
    all_parsed_packages <- c(all_parsed_packages, packages_from_block)
  }

  # Parse packages from git sources (R only)
  if (config$parse_git && language == "R") {
    packages_from_git <- parse_rpkgs_git(
      nix_file = nix_file,
      project_path = project_path,
      transform = config$transform_func
    )
    if (!is.null(packages_from_git)) {
      all_parsed_packages <- c(all_parsed_packages, packages_from_git)
    }
  }

  # If no packages found, return NULL
  if (length(all_parsed_packages) == 0) {
    return(NULL)
  }

  # Process packages
  packages <- unique(all_parsed_packages)
  packages <- config$adjust_func(packages)
  packages <- sort(packages)

  # Create output filename (use basename to handle relative paths)
  nix_file_name <- gsub("[^a-zA-Z0-9]", "_", basename(nix_file))
  nix_file_name <- sub("_nix$", "", nix_file_name)
  outfile_dir <- file.path(project_path, "_rixpress")
  outfile <- file.path(
    outfile_dir,
    paste0(nix_file_name, "_libraries.", config$extension)
  )

  # Generate the script
  generate_libraries_script(
    packages,
    additional_files,
    outfile,
    config$import_formatter,
    config$additional_file_pattern
  )

  return(outfile)
}

#' Generate Language-Specific Library Scripts from a Nix File
#'
#' These are convenience wrappers around generate_r_or_py_libraries_from_nix
#' for specific languages.
#' Generate R library script from a Nix file
#' @param nix_file Path to the default.nix file
#' @param additional_files Character vector of additional files to include
#' @param project_path Path to root of project, typically "."
#' @return Path to the generated script, or NULL if no packages found
#' @noRd
generate_r_libraries_from_nix <- function(
  nix_file,
  additional_files = "",
  project_path = "."
) {
  generate_r_or_py_libraries_from_nix(
    nix_file,
    additional_files,
    project_path,
    language = "R"
  )
}

#' Generate Python library script from a Nix file
#'
#' @param nix_file Path to the default.nix file
#' @param additional_files Character vector of additional files to include
#' @param project_path Path to root of project, typically "."
#' @return Path to the generated script, or NULL if no packages found
#' @noRd
generate_py_libraries_from_nix <- function(
  nix_file,
  additional_files = "",
  project_path = "."
) {
  generate_r_or_py_libraries_from_nix(
    nix_file,
    additional_files,
    project_path,
    language = "Python"
  )
}

#' Generate Julia library script from a Nix file
#' @param nix_file Path to the default.nix file
#' @param additional_files Character vector of additional files to include
#' @param project_path Path to root of project, typically "."
#' @return Path to the generated script, or NULL if no packages found
#' @noRd
generate_jl_libraries_from_nix <- function(
  nix_file,
  additional_files = "",
  project_path = "."
) {
  generate_r_or_py_libraries_from_nix(
    nix_file,
    additional_files,
    project_path,
    language = "Julia"
  )
}

#' Adjust Python Import Statements
#'
#' When calling `rxp_populate()`, a file containing Python import statements is
#' automatically generated inside the `_rixpress` folder. For example, if the
#' `numpy` package is needed, the file will include a line like
#' `"import numpy"`. However, Python programmers often write
#' `"import numpy as np"` instead.
#'
#' In some cases, the correct import statement is entirely different. For
#' example, for the `pillow` package, the generated file will contain
#' `"import pillow"`, which is incorrect—Python code should import from the
#' `PIL` namespace instead, e.g., `"from PIL import Image"`.
#'
#' Because these adjustments cannot be automated reliably, the `adjust_import()`
#' function allows you to search and replace import statements programmatically.
#' It reads each file in the `_rixpress` folder, performs the replacement, and
#' writes the modified content back to the file.
#' @family python import
#' @param old_import A character string representing the import statement to be
#'   replaced. For example, `"import pillow"`.
#' @param new_import A character string representing the new import statement to
#'   replace with. For example, `"from PIL import Image"`.
#' @param project_path Path to root of project, typically ".".
#'
#' @return No return value; the function performs in-place
#'   modifications of the files.
#'
#' @examples
#' \dontrun{
#' adjust_import("import pillow", "from PIL import Image")
#' adjust_import("import pillow", "from PIL import Image", project_path = "path/to/project")
#' }
#' @export
adjust_import <- function(old_import, new_import, project_path = ".") {
  # Validate inputs
  if (!is.character(old_import) || length(old_import) != 1) {
    stop("old_import must be a single character string")
  }
  if (!is.character(new_import) || length(new_import) != 1) {
    stop("new_import must be a single character string")
  }

  # Check if _rixpress folder exists
  rixpress_folder <- file.path(project_path, "_rixpress")
  if (!dir.exists(rixpress_folder)) {
    warning(paste("_rixpress folder not found in", project_path))
    return(invisible(NULL))
  }

  # Get all files in the _rixpress folder
  files <- list.files(
    path = rixpress_folder,
    full.names = TRUE,
    recursive = TRUE
  )

  # Process each file
  for (file in files) {
    content <- readLines(file, warn = FALSE)
    new_content <- gsub(old_import, new_import, content, fixed = TRUE)

    # Only write if content changed
    if (!identical(content, new_content)) {
      writeLines(new_content, con = file)
    }
  }

  invisible(NULL)
}

#' Add an Import Statement to Python Files in the _rixpress Folder Matching a
#' Nix Environment Name
#'
#' This function appends a specified import statement to the end of each Python
#' file within the `_rixpress` folder and its subdirectories, but only for files
#' whose base name matches the provided Nix environment.
#'
#' @family python import
#' @param import_statement A character string representing the import statement
#'   to be added. For example, `"import numpy as np"`.
#' @param nix_env A character string naming the Nix environment file (e.g.
#'   `"default.nix"` or `"py-env.nix"` or similar).
#' @param project_path Path to root of project, typically ".".
#'
#' @return No return value; the function performs in-place modifications of the
#'   files.
#'
#' @examples
#' \dontrun{
#' add_import("import numpy as np", "default.nix")
#' add_import("import numpy as np", "default.nix", project_path = "path/to/project")
#' }
#' @export
add_import <- function(import_statement, nix_env, project_path = ".") {
  # Validate inputs
  if (!is.character(import_statement) || length(import_statement) != 1) {
    stop("import_statement must be a single character string")
  }
  if (!is.character(nix_env) || length(nix_env) != 1) {
    stop("nix_env must be a single character string, e.g. 'default.nix'.")
  }

  # Check if _rixpress folder exists
  rixpress_folder <- file.path(project_path, "_rixpress")
  if (!dir.exists(rixpress_folder)) {
    warning(paste("_rixpress folder not found in", project_path))
    return(invisible(NULL))
  }

  # Extract base name from nix_env
  base_name <- sub("\\.nix$", "", nix_env)
  if (identical(base_name, nix_env)) {
    warning(
      "Provided nix_env did not end with '.nix'; using entire string as base name."
    )
  }

  # Construct regex to match Python files starting with the base name
  # e.g. ^default.*\.[pP]y$
  file_pattern <- paste0(
    "^",
    base_name,
    ".*\\.[pP]y$"
  )

  # List only .py/.Py files whose names match the base_name prefix
  files <- list.files(
    path = rixpress_folder,
    pattern = file_pattern,
    full.names = TRUE,
    recursive = TRUE
  )

  if (length(files) == 0) {
    return(invisible(NULL))
  }

  # Loop through each matching Python file
  for (file in files) {
    content <- readLines(file, warn = FALSE)

    # Escape special regex characters in the import statement
    escaped_import <- gsub(
      "([.*+?^${}()|\\[\\]\\\\])",
      "\\\\\\1",
      import_statement,
      perl = TRUE
    )

    # Check if import statement already exists
    if (!any(grepl(paste0("^", escaped_import, "$"), content))) {
      new_content <- c(content, import_statement)
      writeLines(new_content, con = file)
    }
  }

  invisible(NULL)
}
