#' @title Find matching files for a derivation
#' @description Searches for files in the "result" directory that
#'   match the given derivation name. If any matching file has a .pickle extension,
#'   the function checks whether the reticulate package is installed.
#' @param derivation_name The name of the derivation.
#' @return A character vector of paths to the matching files.
#' @noRd
rxp_common <- function(derivation_name) {
  # if derivation_name is a full path to the /nix/store simply return it
  # this is useful for Quarto documents
  if (grepl("^/nix/store/", derivation_name)) {
    return(derivation_name)
  }

  build_log <- rxp_inspect()

  derivation <- subset(build_log, subset = derivation == derivation_name)

  files <- unlist(derivation$output)

  if (length(files) == 0) {
    stop(paste(
      "No derivation called",
      derivation_name,
      "found. Run `rxp_inspect()` to check if",
      derivation_name,
      "was built successfully."
    ))
  }

  file_paths <- paste0(derivation$path, "/", files)

  # If any matching file is a .pickle, check if reticulate is installed
  if (
    any(grepl(".*\\.pickle$", file_paths)) &&
      !requireNamespace("reticulate", quietly = TRUE)
  ) {
    stop(
      "The 'reticulate' package is required to load .pickle files.\nPlease install it to use this feature."
    )
  }
  file_paths
}

#' @title Read output of a derivation
#' @param derivation_name Character, the name of the derivation.
#' @return The derivation's output.
#' @export
rxp_read <- function(derivation_name) {
  matching_files <- rxp_common(derivation_name)

  # if there’s only one path, read it
  if (length(matching_files) == 1) {
    if (grepl(".*\\.rds$", matching_files)) {
      readRDS(matching_files)
    } else if (grepl(".*\\.pickle$", matching_files)) {
      reticulate::py_load_object(matching_files)
    } else {
      matching_files
    }
    # if there’s more than one path, return all paths and let user choose
  } else {
    matching_files
  }
}

#' @title Load a derivation's output into the global environment
#' @param derivation_name Character, the name of the derivation.
#' @return None. The derivation object is assigned to the
#'   global environment with the name `derivation_name`.
#' @export
rxp_load <- function(derivation_name) {
  matching_files <- rxp_common(derivation_name)
  if (length(matching_files) == 1) {
    if (grepl(".*\\.rds$", matching_files)) {
      assign(derivation_name, readRDS(matching_files), envir = .GlobalEnv)
    } else if (grepl(".*\\.pickle$", matching_files)) {
      assign(
        derivation_name,
        reticulate::py_load_object(matching_files),
        envir = .GlobalEnv
      )
    } else {
      matching_files
    }
  } else {
    matching_files
  }
}
