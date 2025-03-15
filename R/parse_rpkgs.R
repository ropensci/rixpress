#' Generate an R script with library calls from a default.nix file
#'
#' @param nix_file Path to the default.nix file (default: "default.nix")
#' @return A script to load the libraries inside of derivations.
#' @noRd
generate_libraries_from_nix <- function(nix_file) {
  packages <- parse_rpkgs(nix_file)
  nix_file_name <- gsub("\\.nix", "", nix_file)
  generate_libraries_script(
    packages,
    paste0("_rixpress/", nix_file_name, "_libraries.R")
  )
}

#' Helper function to add 'library()' to packages.
#'
#' @param nix_file Path to the default.nix file (default: "default.nix")
#' @param outfile Path to the output file, we recommend to leave the
#'   default `"_rixpress/libraries.R"`
#' @return A script to load the libraries inside of derivations.
#' @noRd
generate_libraries_script <- function(
  packages,
  outfile
) {
  library_lines <- paste0("library(", packages, ")")
  writeLines(library_lines, outfile)
}

#' Generate an R script with library calls from a default.nix file
#'
#' @param nix_file Path to the default.nix file (default: "default.nix")
#' @return List of packages defined in the rpkgs block of a default.nix file
#' @noRd
parse_rpkgs <- function(nix_file) {
  # Read the file as lines
  lines <- readLines(nix_file)

  # Find the starting index of the rpkgs block
  start_idx <- grep("^\\s*rpkgs\\s*=\\s*builtins\\.attrValues\\s*\\{", lines)
  if (length(start_idx) == 0) {
    stop("rpkgs block not found in the file")
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
