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
    files <- list.files(derivation_name, full.names = TRUE)

    if (length(files) == 1) {
      return(files)
    } else {
      return(derivation_name)
    }
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
  files <- rxp_common(derivation_name)

  if (length(files) != 1) {
    return(files)
  }

  path <- files

  # Try RDS, then pickle (checking for reticulate), else return the path
  tryCatch(
    readRDS(path),
    error = function(e1) {
      if (!requireNamespace("reticulate", quietly = TRUE)) {
        message(
          "If you're trying to load a pickle'd object,
you need to install the '{reticulate}' package."
        )
        return(path)
      }
      tryCatch(
        reticulate::py_load_object(path),
        error = function(e2) path
      )
    }
  )
}

#' @title Load a derivation's output into the global environment
#' @param derivation_name Character, the name of the derivation.
#' @return None. The derivation object is assigned to the
#'   global environment with the name `derivation_name`.
#' @export
rxp_load <- function(derivation_name) {
  files <- rxp_common(derivation_name)
  if (length(files) != 1) {
    return(files)
  }
  path <- files

  # Attempt to read as RDS, else fall back to pickle (with reticulate check)
  value <- tryCatch(
    readRDS(path),
    error = function(e1) {
      # If it wasn't an RDS, try pickle
      if (!requireNamespace("reticulate", quietly = TRUE)) {
        message(
          "If you're trying to load a pickle'd object, you need to install the 'reticulate' package."
        )
        return(path)
      }
      tryCatch(
        reticulate::py_load_object(path),
        error = function(e2) {
          message("Failed to load file '", path, "': ", e2$message)
          return(path)
        }
      )
    }
  )

  # If we got back a path (i.e. both readers failed), just return it
  if (is.character(value) && length(value) == 1 && value == path) {
    return(path)
  }

  # Otherwise assign into global env and invisibly return it
  assign(derivation_name, value, envir = .GlobalEnv)
  invisible(value)
}
