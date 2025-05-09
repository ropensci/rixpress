#' Extract packages from a specified block in a default.nix file
#'
#' @param nix_file Defaults to "default.nix", path to the default.nix file
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

  # Find the start of the block
  # (e.g., "rpkgs = builtins.attrValues {" or "pyconf = ...")
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
  block_lines <- gsub("#.*", "", block_lines)
  block_lines <- trimws(block_lines)
  block_lines <- block_lines[block_lines != ""]

  # Remove inherit statements
  # (e.g., "inherit (pkgs.rPackages)" or "inherit (pkgs.python312Packages)")
  inherit_pattern <- "inherit \\(pkgs\\.[a-zA-Z0-9]+\\)"
  block_lines <- gsub(inherit_pattern, "", block_lines)

  block_lines <- gsub(";", "", block_lines)

  # Split into package names and apply transformation
  packages <- unlist(strsplit(paste(block_lines, collapse = " "), "\\s+"))
  packages <- packages[packages != ""]

  transform(packages)
}

#' Extract R packages defined via pkgs.rPackages.buildRPackage from git sources
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
  lines <- readLines(file.path(project_path, nix_file))

  git_packages <- c()

  # Pattern to find the start of a buildRPackage definition
  # e.g., my_pkg = (pkgs.rPackages.buildRPackage {
  # We capture the variable name for context if needed,
  # but don't use it for the package name itself
  build_r_pkg_start_pattern <- "^\\s*([a-zA-Z0-9_.-]+)\\s*=\\s*\\(pkgs\\.rPackages\\.buildRPackage\\s*\\{"
  start_indices <- grep(build_r_pkg_start_pattern, lines)

  if (length(start_indices) == 0) {
    return(NULL)
  }

  # Pattern for the end of these blocks is typically "});"
  end_block_pattern <- "^\\s*\\}\\);"

  for (start_idx in start_indices) {
    # Find the corresponding end of this specific block
    # Search for end_block_pattern *after* start_idx
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
  import_lines <- sapply(packages, import_formatter)

  additional_scripts <- Filter(
    function(x) grepl(additional_file_pattern, x),
    additional_files
  )

  if (length(additional_scripts) > 0) {
    list_additional_content <- lapply(additional_scripts, readLines)
    additional_content <- Reduce(append, list_additional_content)
    output <- append(import_lines, additional_content)
  } else {
    output <- import_lines
  }

  output_dir <- dirname(outfile)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  writeLines(output, outfile)
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
import_formatter_py <- function(package) {
  paste0("import ", package)
}

#' Generate a script with import statements from a default.nix file
#'
#' @param nix_file Defaults to "default.nix", path to the default.nix file
#' @param additional_files Character vector of additional files to include
#' @param project_path Path to root of project, typically "."
#' @param language Language to generate the script for ("R" or "Python")
#' @return A script file for the specified language,
#'   or NULL if no packages are found
#' @noRd
generate_r_or_py_libraries_from_nix <- function(
  nix_file,
  additional_files = "",
  project_path,
  language
) {
  all_parsed_packages <- c()

  if (language == "R") {
    block_name <- "rpkgs"
    transform_func <- transform_r
    adjust <- adjust_r_packages
    import_formatter <- import_formatter_r
    additional_file_pattern <- "functions\\.[Rr]"
    extension <- "R"

    # Parse packages from the 'rpkgs' block
    packages_from_block <- parse_packages(
      nix_file = nix_file,
      project_path = project_path,
      block_name = block_name,
      transform = transform_func
    )
    if (!is.null(packages_from_block)) {
      all_parsed_packages <- c(all_parsed_packages, packages_from_block)
    }

    # Parse R packages from git definitions
    packages_from_git <- parse_rpkgs_git(
      nix_file = nix_file,
      project_path = project_path,
      transform = transform_func
    )
    if (!is.null(packages_from_git)) {
      all_parsed_packages <- c(all_parsed_packages, packages_from_git)
    }
  } else if (language == "Python") {
    block_name <- "pyconf"
    transform_func <- identity
    adjust <- adjust_py_packages
    import_formatter <- import_formatter_py
    additional_file_pattern <- "functions\\.py"
    extension <- "py"

    # Python packages are only in the main block
    packages_from_block <- parse_packages(
      nix_file = nix_file,
      project_path = project_path,
      block_name = block_name,
      transform = transform_func
    )
    if (!is.null(packages_from_block)) {
      all_parsed_packages <- c(all_parsed_packages, packages_from_block)
    }
  } else {
    stop("Unsupported language")
  }

  if (length(all_parsed_packages) == 0) {
    return(NULL)
  }

  packages <- unique(all_parsed_packages)
  packages <- adjust(packages)
  packages <- sort(packages)

  nix_file_name <- gsub("[^a-zA-Z0-9]", "_", nix_file)
  nix_file_name <- sub("_nix$", "", nix_file_name)

  outfile_dir <- file.path(project_path, "_rixpress")

  outfile <- file.path(
    outfile_dir,
    paste0(nix_file_name, "_libraries.", extension)
  )

  generate_libraries_script(
    packages,
    additional_files,
    outfile,
    import_formatter,
    additional_file_pattern
  )

  return(outfile)
}

#' @noRd
generate_r_libraries_from_nix <- function(
  nix_file,
  additional_files = "",
  project_path
) {
  generate_r_or_py_libraries_from_nix(
    nix_file,
    additional_files,
    project_path,
    language = "R"
  )
}

#' @noRd
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
#' When calling `rixpress()`, a file containing Python import statements is
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
#' # Assuming your project is in the current working directory
#' adjust_import("import pillow", "from PIL import Image")
#' # If project is elsewhere:
#' # adjust_import("import pillow", "from PIL import Image", project_path = "path/to/project")
#' }
#' @export
adjust_import <- function(old_import, new_import, project_path = ".") {
  rixpress_folder <- file.path(project_path, "_rixpress")
  if (!dir.exists(rixpress_folder)) {
    warning(paste("_rixpress folder not found in", project_path))
    return(invisible(NULL))
  }
  files <- list.files(
    path = rixpress_folder,
    full.names = TRUE,
    recursive = TRUE
  )
  for (file in files) {
    content <- readLines(file, warn = FALSE)
    new_content <- gsub(old_import, new_import, content, fixed = TRUE)
    writeLines(new_content, con = file)
  }
}

#' Add an import statement to Python files in the _rixpress folder matching a
#' Nix environment name
#'
#' This function appends a specified import statement to the end of each Python
#' file within the `_rixpress` folder and its subdirectories, but only for files
#' whose base name matches the provided Nix environment.
#'
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
#' # Assuming project is in current working directory
#' add_import("import numpy as np", "default.nix")
#' # If project is elsewhere:
#' # add_import("import numpy as np", "default.nix", project_path = "path/to/project")
#' }
#' @export
add_import <- function(import_statement, nix_env, project_path = ".") {
  rixpress_folder <- file.path(project_path, "_rixpress")
  if (!dir.exists(rixpress_folder)) {
    warning(paste("_rixpress folder not found in", project_path))
    return(invisible(NULL))
  }
  # Validate and extract base name from nix_env
  if (!is.character(nix_env) || length(nix_env) != 1) {
    stop("nix_env must be a single character string, e.g. 'default.nix'.")
  }
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
    message(paste(
      "No Python library files matching pattern",
      file_pattern,
      "found in",
      rixpress_folder
    ))
    return(invisible(NULL))
  }

  # Loop through each matching Python file
  for (file in files) {
    content <- readLines(file, warn = FALSE)
    # Avoid adding duplicate import statements
    if (
      !any(grepl(
        paste0(
          "^",
          gsub(
            "([.*+?^${}()|\\[\\]\\\\])",
            "\\\\\\1",
            import_statement,
            perl = TRUE
          ),
          "$"
        ),
        content
      ))
    ) {
      new_content <- c(content, import_statement)
      writeLines(new_content, con = file)
    } else {
      message(paste("Import statement already present in:", file))
    }
  }
}
