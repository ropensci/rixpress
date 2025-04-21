#' Generate a Python script with import statements from a default.nix file
#'
#' @param nix_file Path to the default.nix file (default: "default.nix")
#' @param additional_files Character vector, additional files to include. Custom
#'   functions must go into a script called "functions.py", and additional files
#'   that need to be accessible during the build process can be named anything.
#' @param project_path Path to root of project, typically "."
#' @return A Python script to import the libraries inside of derivations
#' @noRd
generate_py_libraries_from_nix <- function(
  nix_file,
  additional_files = "",
  project_path
) {
  packages <- parse_pypkgs(nix_file, project_path)
  if (is.null(packages)) {
    return(NULL)
  }

  nix_file_name <- gsub("[^a-zA-Z0-9]", "_", nix_file)
  nix_file_name <- sub("_nix$", "", nix_file_name)

  generate_py_libraries_script(
    packages,
    additional_files,
    file.path(
      project_path,
      "/_rixpress/",
      paste0(nix_file_name, "_libraries.py")
    )
  )
}

#' Generate a Python script with import statements for Python packages
#'
#' @param packages List of Python package names
#' @param additional_files Character vector, additional files to include. Custom
#'   functions must go into a script called "functions.py", and additional files
#'   that need to be accessible during the build process can be named anything.
#' @param outfile Path to the output file, we recommend to leave the
#'   default `"_rixpress/libraries.py"`
#' @return A Python script with import statements for the specified packages
#' @noRd
generate_py_libraries_script <- function(
  packages,
  additional_files = "",
  outfile
) {
  # pip and ipykernel are added automatically by rix, because
  # they're needed for Positron, and likely other editors
  # but in the context of building derivations non-interactively
  # with rixpress, these are not needed
  packages <- packages[!(packages %in% c("pip", "ipykernel"))]
  # Include pickle to serialize and unserialize objects
  packages <- sort(c("pickle", packages))
  packages <- gsub("scikit-learn", "sklearn", packages)
  import_lines <- paste0("import ", packages)

  functions_py <- any(grepl("functions.py", additional_files))

  if (!functions_py) {
    output <- import_lines
  } else if (functions_py) {
    functions_py_scripts <- Filter(
      \(x) (grepl("functions.py", x)),
      x = additional_files
    )

    list_functions_py_content <- lapply(functions_py_scripts, readLines)
    functions_py_content <- Reduce(f = append, x = list_functions_py_content)
    output <- append(import_lines, functions_py_content)
  }

  writeLines(output, outfile)
}

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
  start_idx <- grep("^\\s*pyconf\\s*=\\s*builtins\\.attrValues\\s*\\{", lines)
  if (length(start_idx) == 0) {
    return(NULL)
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
  block_lines <- gsub(
    "inherit \\(pkgs\\.python[0-9]+Packages\\)",
    "",
    block_lines
  )

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

#' Adjust Python import statements
#'
#' When calling `rixpress()`, a file containing Python import statements is
#' automatically generated inside the `_rixpress` folder. For example, if the
#' `numpy` package is needed, the file will include a line like `"import numpy"`.
#' However, Python programmers often write `"import numpy as np"` instead.
#'
#' In some cases, the correct import statement is entirely different. For example,
#' for the `pillow` package, the generated file will contain `"import pillow"`,
#' which is incorrect—Python code should import from the `PIL` namespace instead,
#' e.g., `"from PIL import Image"`.
#'
#' Because these adjustments cannot be automated reliably, the `adjust_imports()`
#' function allows you to search and replace import statements programmatically.
#' It reads each file in the `_rixpress` folder, performs the replacement,
#' and writes the modified content back to the file.
#' @param old_import A character string representing the import statement to
#'   be replaced. For example, `"import pillow"`.
#' @param new_import A character string representing the new import statement
#'   to replace with. For example, `"from PIL import Image"`.
#'
#' @return No return value; the function performs in-place
#'   modifications of the files.
#'
#' @examples
#' \dontrun{
#' adjust_imports("import pillow", "from PIL import Image")
#' }
#' @export
adjust_imports <- function(old_import, new_import) {
  files <- list.files(path = "_rixpress", full.names = TRUE, recursive = TRUE)

  for (file in files) {
    content <- readLines(file, warn = FALSE)

    new_content <- gsub(old_import, new_import, content, fixed = TRUE)

    writeLines(new_content, con = file)
  }
}
