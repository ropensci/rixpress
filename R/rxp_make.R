#' Build pipeline using Nix
#'
#' Runs `nix-build` with a quiet flag, outputting to `_rixpress/result`.
#' @family pipeline
#' @param verbose Logical, defaults to FALSE. Set to TRUE to see nix's
#'   standard output, can be useful to check what is happening if the
#'   build process takes long.
#' @param max_jobs Integer, number of derivations to be built in parallel.
#' @param cores Integer, number of cores a derivation can use during build.
#' @importFrom processx run
#' @importFrom utils capture.output
#' @return A character vector of paths to the built outputs.
#' @examples
#' \dontrun{
#'   # Build the pipeline with default settings
#'   rxp_make()
#'
#'   # Build with verbose output and parallel execution
#'   rxp_make(verbose = TRUE, max_jobs = 4, cores = 2)
#' }
#' @export
rxp_make <- function(verbose = FALSE, max_jobs = 1, cores = 1) {
  message("Build process started...\n", "\n")

  instantiate <- processx::run(
    command = "nix-instantiate",
    args = "pipeline.nix",
    error_on_status = FALSE,
    spinner = TRUE
  )

  # Check for instantiation failure
  if (instantiate$status != 0) {
    cat(instantiate$stderr)
    stop("nix-instantiate failed")
  }

  # Extract derivation paths from stdout (split into lines)
  drv_paths <- strsplit(instantiate$stdout, "\n")[[1]]

  if (verbose) {
    cb <- function(line, proc) cat(line, "\n")
  } else {
    cb <- NULL
  }

  build_process <- processx::run(
    command = "nix-store",
    args = c(
      "--realise",
      "--verbose",
      "--keep-going",
      "--max-jobs",
      max_jobs,
      "--cores",
      cores,
      drv_paths
    ),
    stdout_line_callback = cb,
    stderr_line_callback = cb,
    error_on_status = FALSE,
    spinner = TRUE
  )

  build_log <- lapply(drv_paths, function(drv_path) {
    # Get the output paths for this derivation
    output_result <- processx::run(
      command = "nix-store",
      args = c("-q", "--outputs", drv_path),
      error_on_status = FALSE
    )
    output_paths <- strsplit(output_result$stdout, "\n")[[1]]

    output_checks <- lapply(output_paths, function(path) {
      files <- list.files(path, all.files = FALSE)
      list(path = path, files = files, build_success = length(files) > 0)
    })

    data.frame(
      derivation = gsub("\\.drv$", "", gsub("^[^-]*-", "", drv_path)),
      build_success = vapply(output_checks, `[[`, logical(1), "build_success"),
      path = vapply(output_checks, `[[`, character(1), "path"),
      output = I(lapply(output_checks, `[[`, "files"))
    )
  })

  build_log <- do.call(rbind, build_log)

  # Get timestamp
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

  # Extract hash of all-derivations from the path
  all_derivs_row <- which(build_log$derivation == "all-derivations")
  all_derivs_hash <- ""
  if (length(all_derivs_row) > 0) {
    path <- build_log$path[all_derivs_row[1]]
    # Extract hash from path (format: /nix/store/HASH-all-derivations)
    all_derivs_hash <- gsub(
      "^/nix/store/([^-]+)-all-derivations.*$",
      "\\1",
      path
    )
    if (all_derivs_hash == path) {
      all_derivs_hash <- "" # If regex didn't match
    }
  }

  # Save with timestamp and hash
  log_filename <- sprintf(
    "_rixpress/build_log_%s%s.rds",
    timestamp,
    ifelse(all_derivs_hash != "", paste0("_", all_derivs_hash), "")
  )

  # Save both the timestamped version and the standard version
  saveRDS(build_log, log_filename)

  failures <- subset(build_log, subset = !build_success)
  if (nrow(failures) > 0) {
    warning(
      "Build failures:\n",
      paste(capture.output(print(failures)), collapse = "\n")
    )
  }

  if (build_process$status != 0) {
    cat(build_process$stderr)
    stop("Build failed! Check the log above for hints\nor run `rxp_inspect()`.")
  }

  if (build_process$status == 0) {
    message(
      "Build successful! Run `rxp_inspect()` for a summary.\n",
      "Use `rxp_read(\"derivation_name\")` to read objects or\n",
      "`rxp_load(\"derivation_name\")` to load them into the global environment."
    )
  }

  invisible(build_log)
}

#' Export Nix store paths to an archive
#'
#' Creates a single archive file containing the specified Nix store paths
#'   and their dependencies.
#' This archive can be transferred to another machine and imported into
#'   its Nix store.
#'
#' @family pipeline
#' @param archive_file Character, path to the archive, defaults to
#'   "_rixpress/pipeline-outputs.nar"
#' @param which_log Character or NULL, regex pattern to match a specific log file.
#'   If NULL (default), the most recent log file will be used.
#' @param project_path Character, defaults to ".".
#'   Path to the root directory of the project.
#' @return Nothing, creates an archive file at the specified location.
#' @examples
#' \dontrun{
#'   # Export the most recent build to the default location
#'   export_nix_archive()
#'
#'   # Export a specific build to a custom location
#'   export_nix_archive(
#'     archive_file = "my_archive.nar",
#'     which_log = "20250510"
#'   )
#' }
#' @export
export_nix_archive <- function(
  archive_file = "_rixpress/pipeline_outputs.nar",
  which_log = NULL,
  project_path = "."
) {
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

  message("Using build log: ", basename(log_path))
  store_paths <- readRDS(log_path)$path

  message("Exporting store paths to ", archive_file)
  system2("nix-store", args = c("--export", store_paths), stdout = archive_file)
  message("Export completed")
}

#' Import Nix store paths from an archive
#'
#' Imports the store paths contained in an archive file into the local Nix store.
#' Useful for transferring built outputs between machines.
#'
#' @family pipeline
#' @param archive_file Character, path to the archive, defaults to
#'   "_rixpress/pipeline-outputs.nar"
#' @return Nothing, imports the archive contents into the local Nix store.
#' @examples
#' \dontrun{
#'   # Import from the default archive location
#'   import_nix_archive()
#'
#'   # Import from a custom archive file
#'   import_nix_archive("path/to/my_archive.nar")
#' }
#' @export
import_nix_archive <- function(
  archive_file = "_rixpress/pipeline_outputs.nar"
) {
  if (!is.character(archive_file) || length(archive_file) != 1) {
    stop("archive_file must be a single character string")
  }
  if (!file.exists(archive_file)) {
    stop("Archive file does not exist")
  }
  message("Importing store paths from ", archive_file)
  system2("nix-store", args = "--import", stdin = archive_file)
  message("Import completed")
}
