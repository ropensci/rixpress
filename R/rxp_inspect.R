#' @title Inspect the build result of a pipeline.
#' @description Returns a data frame with four columns:
#'     - derivation: the name of the derivation
#'     - build_success: whether the build was successful or not
#'     - path: the path of this derivation in the Nix store
#'     - output: the output, if this derivation was built successfully.
#'               Empty outputs mean that this derivation was not built
#'               successfully. Several outputs for a single derivation
#'               are possible.
#'
#' @param project_path Character, path to the project directory where the _rixpress folder is located.
#'   Defaults to the current directory.
#' @param which_log Character or NULL, if NULL the most recent build log is used.
#'   If a string is provided, it's used as a regular expression to match against available log files.
#'
#' @return A data frame with derivation names, if their build was successful,
#'   their paths in the /nix/store, and their build outputs.
#' @export
rxp_inspect <- function(project_path = ".", which_log = NULL) {
  rixpress_dir <- file.path(project_path, "_rixpress")

  if (is.null(which_log)) {
    # Use the standard build log
    log_path <- file.path(rixpress_dir, "build_log.rds")
    if (!file.exists(log_path)) {
      stop("Build log not found, did you build the pipeline?")
    }
  } else {
    # Find logs matching the pattern
    log_files <- list.files(
      path = rixpress_dir,
      pattern = paste0("build_log_.*", which_log, ".*\\.rds$"),
      full.names = TRUE
    )

    if (length(log_files) == 0) {
      # Try a more general search if no matches
      log_files <- list.files(
        path = rixpress_dir,
        pattern = paste0("build_log_.*\\.rds$"),
        full.names = TRUE
      )

      if (length(log_files) == 0) {
        stop("No build logs found matching the pattern: ", which_log)
      }

      # Filter using grep
      matches <- grep(which_log, log_files, value = TRUE)
      if (length(matches) == 0) {
        stop("No build logs found matching the pattern: ", which_log)
      }
      log_files <- matches
    }

    # Sort by modification time (most recent first)
    file_info <- file.info(log_files)
    log_files <- log_files[order(file_info$mtime, decreasing = TRUE)]

    # Use the most recent matching log
    log_path <- log_files[1]
    message("Using log file: ", basename(log_path))
  }

  readRDS(log_path)
}
