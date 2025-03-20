#' Generate an R script with library calls from a default.nix file
#'
#' @param nix_file Path to the default.nix file (default: "default.nix")
#' @param additional_files Character vector, additional files to include. These
#'   are the files that contain custom functions required for this derivation.
#' @param project_path Path to root of project, typically "."
#' @return An script to load the libraries inside of derivations.
#' @noRd
generate_r_libraries_from_nix <- function(
  nix_file,
  additional_files = "",
  project_path
) {
  packages <- parse_rpkgs(nix_file, project_path)
  if (is.null(packages)) {
    return(NULL)
  }
  nix_file_name <- gsub("\\.nix", "", nix_file)

  generate_r_libraries_script(
    packages,
    additional_files,
    file.path(
      project_path,
      "/_rixpress/",
      paste0(nix_file_name, "_libraries.R")
    )
  )
}

#' Helper function to add 'library()' to packages.
#'
#' @param nix_file Path to the default.nix file (default: "default.nix")
#' @param additional_files Character vector, additional files to include. These
#'   are the files that contain custom functions required for this derivation.
#' @param outfile Path to the output file, we recommend to leave the
#'   default `"_rixpress/libraries.R"`
#' @return A script to load the libraries inside of derivations.
#' @noRd
generate_r_libraries_script <- function(
  packages,
  additional_files = "",
  outfile
) {
  library_lines <- paste0("library(", packages, ")")
  additional_files_content <- unlist(
    sapply(additional_files, readLines),
    use.names = FALSE
  )

  output <- append(library_lines, additional_files_content)

  writeLines(output, outfile)
}

#' Generate an R script with library calls from a default.nix file
#'
#' @param nix_file Path to the default.nix file (default: "default.nix")
#' @param project_path Path to root of project, typically "."
#' @return List of packages defined in the rpkgs block of a default.nix file
#' @noRd
parse_rpkgs <- function(nix_file, project_path) {
  # Read the file as lines
  lines <- readLines(file.path(project_path, nix_file))

  # Find the starting index of the rpkgs block
  start_idx <- grep("^\\s*rpkgs\\s*=\\s*builtins\\.attrValues\\s*\\{", lines)
  if (length(start_idx) == 0) {
    return(NULL)
  }
  start_idx <- start_idx[1]

  # Find the end of the rpkgs block (a line that starts with "};")
  end_idx <- grep("^\\s*\\};", lines)
  end_idx <- end_idx[end_idx > start_idx][1]
  if (is.na(end_idx)) {
    stop("Could not find the end of the rpkgs block")
  }

  # Extract lines within the block
  block_lines <- lines[(start_idx + 1):(end_idx - 1)]

  # Remove comments and trim white spaces
  block_lines <- gsub("#.*", "", block_lines)
  block_lines <- trimws(block_lines)

  # Remove any empty lines
  block_lines <- block_lines[block_lines != ""]

  # Remove the "inherit (pkgs.rPackages)" phrase if present
  block_lines <- gsub("inherit \\(pkgs\\.rPackages\\)", "", block_lines)

  # Remove semicolon characters
  block_lines <- gsub(";", "", block_lines)

  # Combine all lines into one string and split by whitespace
  packages <- unlist(strsplit(paste(block_lines, collapse = " "), "\\s+"))

  # In Nix, R packages use _ instead of ., so data_table needs to become
  # data.table

  packages <- gsub("_", ".", packages)

  # Remove empty strings if any
  packages[packages != ""]
}
