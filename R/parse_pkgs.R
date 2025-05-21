#' Extract packages from a specified block in a default.nix file
#'
#' @param nix_file Defaults to "default.nix", path to the default.nix file
#' @param project_path Path to root of project, typically "."
#' @param block_name Name of the block to parse (e.g., "rpkgs", "pyconf", "jlconf").
#' @param transform Function to transform package names (default: identity).
#' @return List of package names, or NULL if the block is not found.
#' @noRd
parse_packages <- function(
  nix_file,
  project_path,
  block_name,
  transform = identity
) {
  lines <- readLines(file.path(project_path, nix_file))

  # Regex to find the start of the specified package block.
  # It handles two main patterns:
  # 1.  `<block_name> = builtins.attrValues { ... }` (common for R, Python).
  # 2.  `<block_name> = pkgs.julia<...>.withPackages [ ... ]` (common for Julia).
  # The `(?: ... )` creates a non-capturing group for the OR condition.
  start_pattern <- paste0(
    "^\\s*",
    block_name,
    "\\s*=\\s*(?:", # Start of non-capturing group for OR
    "builtins\\.attrValues\\s*\\{", # Pattern 1: R/Python style
    "|",                            # OR
    "pkgs\\.julia(?:[-_\\.][A-Za-z0-9]+)*\\.withPackages\\s*\\[", # Pattern 2: Julia style
    ")"                             # End of non-capturing group
  )

  start_idx <- grep(start_pattern, lines)
  if (length(start_idx) == 0) {
    return(NULL)
  }
  start_idx <- start_idx[1]

  # Find the end of the block ("};" or "];")
  end_idxs <- grep("^\\s*(\\};|\\];)", lines, perl = TRUE)
  end_idx  <- end_idxs[end_idxs > start_idx][1]
  if (is.na(end_idx)) {
    stop(paste("Could not find the end of the", block_name, "block"))
  }

  # Extract and clean lines within the block
  block_lines <- lines[(start_idx + 1):(end_idx - 1)]
  block_lines <- gsub("#.*", "", block_lines)
  block_lines <- trimws(block_lines)
  block_lines <- block_lines[block_lines != ""] # Remove empty lines

  # Remove `inherit (...)` statements as they don't list actual package names
  # directly but rather groups of packages from Nixpkgs.
  # e.g., "inherit (pkgs.rPackages);" or "inherit (pkgs.python312Packages);".
  # The actual package names are expected to be listed individually if used.
  inherit_pattern <- "inherit\\s*\\(pkgs\\.[a-zA-Z0-9]+\\)(?:\\s*;)?|inherit\\s*pkgs\\s*;"
  block_lines <- gsub(inherit_pattern, "", block_lines)

  block_lines <- gsub(";", "", block_lines) # Remove any remaining semicolons

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

  # Pattern to find the start of a `pkgs.rPackages.buildRPackage` definition.
  # e.g., `myPkg = (pkgs.rPackages.buildRPackage { ... });`
  # This captures the assigned variable name (e.g., `myPkg`), which isn't directly
  # used for the R package name but helps identify the block.
  # The actual R package name is typically specified by a `name = "ActualRPackageName";`
  # attribute inside this block if it's a git source.
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

    # Check if this block contains a 'src' attribute pointing to a git fetch operation
    # (e.g., pkgs.fetchgit, pkgs.fetchFromGitHub, pkgs.fetchFromGitLab).
    fetch_pattern <- "^\\s*src\\s*=\\s*pkgs\\.(fetchgit|fetchFromGitHub|fetchFromGitLab|fetchGit)\\s*\\{"
    if (any(grepl(fetch_pattern, block_lines))) {
      # If it's identified as a git source, look for the 'name' attribute
      # which specifies the actual R package name.
      # e.g., `name = "myPackageName";`
      name_pattern <- "^\\s*name\\s*=\\s*\"([a-zA-Z0-9_.-]+)\"\\s*(?:;)?\\s*$"
      name_lines_match <- grep(name_pattern, block_lines, value = TRUE)

      if (length(name_lines_match) > 0) {
        # Extract the R package name (captured group 1 from name_pattern).
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

#' Generate a script with import statements from a default.nix file
#'
#' @param nix_file Defaults to "default.nix", path to the default.nix file.
#' @param additional_files Character vector of additional files to include (e.g., "functions.R").
#' @param project_path Path to root of project, typically ".".
#' @param lang_config A list containing language-specific configurations. Expected fields:
#'   - `language_name`: Character (e.g., "R", "Python", "Julia").
#'   - `block_name`: Character, name of the package block in Nix file (e.g., "rpkgs", "pyconf").
#'   - `transform_func`: Function to transform package names from Nix to language-native.
#'   - `adjust_func`: Function to adjust the list of packages (e.g., add/remove specific ones).
#'   - `import_formatter`: Function to format package names into import statements.
#'   - `additional_file_pattern`: Regex to identify language-specific function files.
#'   - `extension`: Character, file extension for the generated library script (e.g., "R", "py").
#'   - `parse_git_pkgs_func`: Optional function to parse packages from Git sources (e.g., `parse_rpkgs_git` for R, `NULL` otherwise).
#' @return Path to the generated script file, or `NULL` if no packages are found or script generation is skipped.
#' @noRd
generate_r_or_py_libraries_from_nix <- function(
  nix_file,
  additional_files = "",
  project_path,
  lang_config  # Expects a list with all configurations
) {
  all_parsed_packages <- c()

  # Parse packages from the main block (e.g., rpkgs, pyconf, jlconf)
  packages_from_block <- parse_packages(
    nix_file = nix_file,
    project_path = project_path,
    block_name = lang_config$block_name,
    transform = lang_config$transform_func
  )
  if (!is.null(packages_from_block)) {
    all_parsed_packages <- c(all_parsed_packages, packages_from_block)
  }

  # Special handling for R packages from Git (or other similar future cases)
  if (!is.null(lang_config$parse_git_pkgs_func)) {
    packages_from_git <- lang_config$parse_git_pkgs_func(
      nix_file = nix_file,
      project_path = project_path,
      transform = lang_config$transform_func # Git pkgs also use the main transform
    )
    if (!is.null(packages_from_git)) {
      all_parsed_packages <- c(all_parsed_packages, packages_from_git)
    }
  }

  if (length(all_parsed_packages) == 0) {
    return(NULL)
  }

  packages <- unique(all_parsed_packages)
  packages <- lang_config$adjust_func(packages)
  packages <- sort(packages)

  nix_file_name <- gsub("[^a-zA-Z0-9]", "_", nix_file)
  nix_file_name <- sub("_nix$", "", nix_file_name)

  outfile_dir <- file.path(project_path, "_rixpress")

  outfile <- file.path(
    outfile_dir,
    paste0(nix_file_name, "_libraries.", lang_config$extension)
  )

  generate_libraries_script(
    packages,
    additional_files,
    outfile,
    lang_config$import_formatter,
    lang_config$additional_file_pattern
  )

  return(outfile)
}

#' @noRd
generate_r_libraries_from_nix <- function(
  nix_file,
  additional_files = "",
  project_path
) {
  r_config <- list(
    language_name = "R",
    block_name = "rpkgs",
    transform_func = transform_r,
    adjust_func = adjust_r_packages,
    import_formatter = import_formatter_r,
    additional_file_pattern = "functions\\.[Rr]",
    extension = "R",
    parse_git_pkgs_func = parse_rpkgs_git
  )
  generate_r_or_py_libraries_from_nix(
    nix_file,
    additional_files,
    project_path,
    lang_config = r_config
  )
}

#' @noRd
generate_py_libraries_from_nix <- function(
  nix_file,
  additional_files = "",
  project_path
) {
  py_config <- list(
    language_name = "Python",
    block_name = "pyconf",
    transform_func = identity,
    adjust_func = adjust_py_packages,
    import_formatter = import_formatter_py,
    additional_file_pattern = "functions\\.py",
    extension = "py",
    parse_git_pkgs_func = NULL
  )
  generate_r_or_py_libraries_from_nix(
    nix_file,
    additional_files,
    project_path,
    lang_config = py_config
  )
}

#' @noRd
generate_jl_libraries_from_nix <- function(
  nix_file,
  additional_files = "",
  project_path
) {
  jl_config <- list(
    language_name = "Julia",
    block_name = "jlconf",
    transform_func = transform_jl,
    adjust_func = adjust_jl_packages,
    import_formatter = import_formatter_jl,
    additional_file_pattern = "functions\\.jl",
    extension = "jl",
    parse_git_pkgs_func = NULL
  )
  generate_r_or_py_libraries_from_nix(
    nix_file,
    additional_files,
    project_path,
    lang_config = jl_config
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
