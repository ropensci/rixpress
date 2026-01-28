#' Core setup function for rxp_read() and rxp_load()
#' @param derivation_name The name of the derivation.
#' @param which_log Character, defaults to NULL. If NULL the most recent
#'   build log is used. If a string is provided, it's used as a
#'   regular expression to match against available log files.
#' @param project_path Character, defaults to ".".
#'   Path to the root directory of the project.
#' @details If a path to the store is passed, return it, otherwise
#'   returns the path to the object(s).
#' @return A character vector of paths to the matching files.
#' @noRd
rxp_read_load_setup <- function(
  derivation_name,
  which_log = NULL,
  project_path = "."
) {
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

  build_log <- rxp_inspect(project_path = project_path, which_log = which_log)

  derivation <- build_log[build_log$derivation == derivation_name, ]

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

#' Read Output of a Derivation
#' @family utilities
#' @description Reads the output of derivations in the current session,
#'   returns a path if reading directly is not possible.
#' @details When `derivation_name` points to a single R object,
#'   it gets read in the current session using `readRDS()`.
#'   If it's a Python object and `{reticulate}` is available,
#'   `reticulate::py_load_object()` is used. In case
#'   the derivation is pointing to several outputs (which can
#'   happen when building a Quarto document for example) or
#'   neither `readRDS()` nor `reticulate::py_load_object()`
#'   successfully read the object, the path to the object is
#'   returned instead.
#' @param derivation_name Character, the name of the derivation.
#' @param which_log Character, defaults to NULL. If NULL the most recent
#'   build log is used. If a string is provided, it's used as a
#'   regular expression to match against available log files.
#' @param project_path Character, defaults to ".".
#'   Path to the root directory of the project.
#' @return The derivation's output.
#' @examples \dontrun{
#'   mtcars <- rxp_read("mtcars")
#'
#'   # Read from a specific build log
#'   mtcars <- rxp_read("mtcars", which_log = "2025-05-10")
#' }
#' @export
rxp_read <- function(derivation_name, which_log = NULL, project_path = ".") {
  files <- rxp_read_load_setup(derivation_name, which_log, project_path)

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
          "If you're trying to load a pickle'd object,\nyou need to install the '{reticulate}' package."
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

#' Load Output of a Derivation

#' @family utilities
#' @description Loads the output of derivations in the parent frame of the
#'   current session, returns a path if reading directly is not possible.
#' @details When `derivation_name` points to a single R object, it gets loaded
#'   in the current session using `assign(..., envir = parent.frame())`, which
#'   corresponds to the global environment in a regular interactive session. If
#'   you're trying to load a Python object and `{reticulate}` is available,
#'   `reticulate::py_load_object()` is used and then the object gets loaded into
#'   the global environment. In case the derivation is pointing to several
#'   outputs (which can happen when building a Quarto document for example) or
#'   loading fails, the path to the object is returned instead.
#' @param derivation_name Character, the name of the derivation.
#' @param which_log Character, defaults to NULL. If NULL the most recent
#'   build log is used. If a string is provided, it's used as a
#'   regular expression to match against available log files.
#' @param project_path Character, defaults to ".".
#'   Path to the root directory of the project.
#' @return Nothing, this function has the side effect of loading objects into
#'   the parent frame.

#' @examples
#' \dontrun{
#'   # Load an R object
#'   rxp_load("mtcars")
#'
#'   # Load a Python object
#'   rxp_load("my_python_model")
#'
#'   # Load from a specific build log
#'   rxp_load("mtcars", which_log = "2025-05-10")
#' }
#' @export
rxp_load <- function(derivation_name, which_log = NULL, project_path = ".") {
  files <- rxp_read_load_setup(derivation_name, which_log, project_path)
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
    message("Note: Returning file path instead of loaded object.")
    return(path)
  }

  assign(derivation_name, value, envir = parent.frame())
  invisible(value)
}
