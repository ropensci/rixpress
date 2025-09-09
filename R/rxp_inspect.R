#' @title List all available build logs
#' @family utilities
#' @description Returns a data frame with information about all build logs
#'     in the project's _rixpress directory.
#'
#' @param project_path Character, defaults to ".".
#'   Path to the root directory of the project.
#'
#' @return A data frame with log filenames, modification times, and file sizes.
#' @examples
#' \dontrun{
#'   # List all build logs in the current project
#'   logs <- rxp_list_logs()
#'
#'   # List logs from a specific project directory
#'   logs <- rxp_list_logs("path/to/project")
#' }
#' @export
rxp_list_logs <- function(project_path = ".") {
  rixpress_dir <- file.path(project_path, "_rixpress")

  if (!dir.exists(rixpress_dir)) {
    stop("_rixpress directory not found. Did you initialise the project?")
  }

  # Find all build log files (now JSON instead of RDS)
  log_files <- list.files(
    path = rixpress_dir,
    pattern = "build_log.*\\.json$",
    full.names = TRUE
  )

  if (length(log_files) == 0) {
    message("No build logs found in ", rixpress_dir)
    return(data.frame(
      filename = character(0),
      modification_time = character(0),
      size_kb = numeric(0)
    ))
  }

  # Get file information
  file_info <- file.info(log_files)

  # Create data frame with relevant information
  logs_df <- data.frame(
    filename = basename(log_files),
    modification_time = file_info$mtime,
    size_kb = round(file_info$size / 1024, 2)
  )

  # Sort by modification time (most recent first)
  logs_df <- logs_df[order(logs_df$modification_time, decreasing = TRUE), ]

  # Reset row names
  rownames(logs_df) <- NULL

  logs_df
}

#' @title Inspect the build result of a pipeline.
#' @family utilities
#' @description Returns a data frame with four columns:
#'     - derivation: the name of the derivation
#'     - build_success: whether the build was successful or not
#'     - path: the path of this derivation in the Nix store
#'     - output: the output, if this derivation was built successfully.
#'               Empty outputs mean that this derivation was not built
#'               successfully. Several outputs for a single derivation
#'               are possible.
#'   In the `derivation` column you will find an object called `all-derivations`.
#'   This object is generated automatically for internal purposes, and you can
#'   safely ignore it.
#'
#' @param project_path Character, defaults to ".".
#'   Path to the root directory of the project.
#' @param which_log Character, defaults to NULL. If NULL the most recent
#'   build log is used. If a string is provided, it's used as a
#'   regular expression to match against available log files.
#'
#' @return A data frame with derivation names, if their build was successful,
#'   their paths in the /nix/store, and their build outputs.
#' @examples
#' \dontrun{
#'   # Inspect the most recent build
#'   build_results <- rxp_inspect()
#'
#'   # Inspect a specific build log
#'   build_results <- rxp_inspect(which_log = "20250510")
#'
#'   # Check which derivations failed
#'   failed <- subset(build_results, !build_success)
#' }
#' @export
rxp_inspect <- function(project_path = ".", which_log = NULL) {
  rixpress_dir <- file.path(project_path, "_rixpress")

  # Get all available logs
  logs_df <- rxp_list_logs(project_path)

  if (nrow(logs_df) == 0) {
    stop("No build logs found, did you build the pipeline?")
  }

  if (is.null(which_log)) {
    # Use the most recent log
    log_path <- file.path(rixpress_dir, logs_df$filename[1])
  } else {
    # Find logs matching the pattern
    matches <- grep(which_log, logs_df$filename, value = TRUE)

    if (length(matches) == 0) {
      stop("No build logs found matching the pattern: ", which_log)
    }

    # Get the full path of the most recent matching log
    match_idx <- match(matches[1], logs_df$filename)
    log_path <- file.path(rixpress_dir, logs_df$filename[match_idx])
    message("Using log file: ", basename(log_path))
  }

  # Read JSON log and convert back into data frame
  log_data <- jsonlite::read_json(log_path, simplifyVector = TRUE)
  as.data.frame(log_data, stringsAsFactors = FALSE)
}
