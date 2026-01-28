#' @title Copy Derivations From the Nix Store to Current Working Directory
#' @family utilities
#' @description When Nix builds a derivation, its output is saved in the
#'   Nix store located under `/nix/store/`. Even though you can import the
#'   derivations into the current R session using `rxp_read()` or `rxp_load()`,
#'   it can be useful to copy the outputs to the current working directory. This
#'   is especially useful for Quarto documents, where there can be more than one
#'   input, as is the case for `html` output.
#' @param derivation_name The name of the derivation to copy. If empty, then all
#'   the derivations are copied.
#' @param dir_mode Character, default "0755". POSIX permission mode to apply to
#'   directories under the copied output (including the top-level output
#'   directory).
#' @param file_mode Character, default "0644". POSIX permission mode to apply to
#'   files under the copied output.
#' @return Nothing, the contents of the Nix store are copied to the current
#'   working directory.
#' @examples
#' \dontrun{
#'   # Copy all derivations to the current working directory
#'   rxp_copy()
#'
#'   # Copy a specific derivation
#'   rxp_copy("mtcars")
#'
#'   # Copy with custom permissions (e.g., make scripts executable)
#'   rxp_copy("my_deriv", dir_mode = "0755", file_mode = "0644")
#'
#'   # Copy a Quarto document output with multiple files
#'   rxp_copy("my_quarto_doc")
#' }
#' @export
rxp_copy <- function(
  derivation_name = NULL,
  dir_mode = "0755",
  file_mode = "0644"
) {
  # Validate permission modes: accept 3 or 4 octal digits as characters
  valid_mode <- function(x) is.character(x) && grepl("^[0-7]{3,4}$", x)
  if (!valid_mode(dir_mode)) {
    stop(
      "Invalid dir_mode: provide a character octal like \"0755\" or \"755\"."
    )
  }
  if (!valid_mode(file_mode)) {
    stop(
      "Invalid file_mode: provide a character octal like \"0644\" or \"644\"."
    )
  }

  build_log <- rxp_inspect()
  output_dir <- file.path(getwd(), "pipeline-output")

  if (isFALSE(dir.exists(output_dir))) {
    dir.create(path = output_dir, recursive = TRUE)
  }

  if (
    !is.null(derivation_name) && !(derivation_name %in% build_log$derivation)
  ) {
    stop(
      "No derivation ",
      derivation_name,
      " found in the Nix store.\n  Did you build the pipeline with rxp_make()?"
    )
  }

  # Helper to apply permissions safely (POSIX only)
  apply_permissions <- function(root_dir, dir_mode, file_mode) {
    if (!dir.exists(root_dir)) {
      return(invisible(NULL))
    }

    # Directories (include root_dir itself)
    dirs <- unique(c(
      root_dir,
      list.dirs(root_dir, recursive = TRUE, full.names = TRUE)
    ))
    if (length(dirs)) {
      try(
        suppressWarnings(Sys.chmod(dirs, mode = dir_mode, use_umask = FALSE)),
        silent = TRUE
      )
    }

    # Files
    files <- list.files(
      root_dir,
      recursive = TRUE,
      full.names = TRUE,
      include.dirs = FALSE
    )
    if (length(files)) {
      try(
        suppressWarnings(Sys.chmod(files, mode = file_mode, use_umask = FALSE)),
        silent = TRUE
      )
    }
  }

  status <- 0L

  if (!is.null(derivation_name)) {
    derivation_path <- subset(
      build_log,
      subset = derivation == derivation_name
    )$path
    path_to_deriv <- list.files(
      derivation_path,
      full.names = TRUE
    )
    file.copy(from = path_to_deriv, to = output_dir, recursive = TRUE)
    apply_permissions(output_dir, dir_mode = dir_mode, file_mode = file_mode)
    status <- 1L
  } else {
    derivation_name <- "all-derivations"
    derivation_path <- subset(
      build_log,
      subset = derivation == derivation_name
    )$path
    files <- list.files(derivation_path, full.names = TRUE)
    file.copy(from = files, to = output_dir, recursive = TRUE)
    apply_permissions(output_dir, dir_mode = dir_mode, file_mode = file_mode)
    status <- 1L
  }

  if (status == 1L) {
    message("Copy successful, check out ", output_dir)
  } else {
    stop("Copy unsuccessful: did you build the pipeline?")
  }
}
