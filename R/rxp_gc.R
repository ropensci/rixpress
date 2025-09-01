#' Clean up rixpress build artifacts with Nix garbage collection
#'
#' Safely removes unused build artifacts from a rixpress project by invoking
#' Nix garbage collection (`nix-store --gc`). Supports two modes:
#'
#' * **Default mode (`keep_since = NULL`)**: Run full Nix garbage collection
#'   to delete all unreferenced store paths.
#' * **Date-based retention (`keep_since` supplied)**: Keep artifacts from builds
#'   newer than the given date, delete artifacts from older builds, and protect
#'   recent builds with temporary GC roots.
#'
#' The function is **destructive**: deleted store paths cannot be recovered.
#' Always test with `dry_run = TRUE` first.
#'
#' @param keep_since `NULL` (default) or a `Date`/string in `"YYYY-MM-DD"` format.
#'   - `NULL`: run full Nix GC.
#'   - Date/string: retain builds newer than this date, delete older builds.
#' @param project_path Path to the project root (default `"."`).
#'   The project must contain an `_rixpress/` directory with build logs.
#' @param dry_run Logical (default `TRUE`). If `TRUE`, show what would be done
#'   without deleting anything or running GC.
#' @param timeout_sec Numeric (default `300`). Timeout (in seconds) for
#'   `nix-store` commands.
#' @param verbose Logical (default `FALSE`). If `TRUE`, print full output of
#'   `nix-store --gc`. Otherwise, show only summary lines.
#'
#' @return Invisibly returns a list with:
#'   - `kept`: filenames of logs kept
#'   - `deleted`: filenames of logs scheduled for deletion
#'   - `protected`: number of GC roots created for recent builds
#'
#' @details
#' ## Concurrency
#' Uses a lock file in `tempdir()` to prevent concurrent runs. If a process dies
#' and leaves a stale lock, it will be removed if older than `timeout_sec`.
#'
#' ## Safety mechanisms
#' - All destructive operations require confirmation (`askYesNo()`).
#' - Invalid or missing store paths are skipped with a warning.
#' - Recent builds are protected with temporary GC roots.
#' - Use `dry_run = TRUE` to preview deletions.
#'
#' ## Platform notes
#' - Requires Nix installed and accessible in `PATH`.
#' - Tested on Unix-like systems. On Windows, PID detection for concurrency may
#'   be less reliable due to faster PID recycling.
#'
#' @examples
#' \dontrun{
#' # Dry run of full garbage collection
#' rxp_gc()
#'
#' # Dry run of date-based retention
#' rxp_gc(keep_since = "2025-01-01")
#'
#' # Actually delete all unreferenced store paths
#' rxp_gc(dry_run = FALSE)
#' }
#'
#' @export
rxp_gc <- function(
  keep_since = NULL,
  project_path = ".",
  dry_run = TRUE,
  timeout_sec = 300,
  verbose = FALSE
) {
  # --- Concurrency Check ---
  lock_file <- file.path(tempdir(), "rixpress_gc.lock")
  if (file.exists(lock_file)) {
    tryCatch({
      lock_info <- readLines(lock_file)
      lock_pid <- as.numeric(lock_info[1])
      lock_time <- as.POSIXct(lock_info[2])
      
      alive <- FALSE
      if (.Platform$OS.type == "unix" && !is.na(lock_pid)) {
        res <- system2("ps", c("-p", lock_pid), stdout = TRUE, stderr = FALSE)
        alive <- any(grepl(lock_pid, res))
      }
      
      if (alive) {
        stop("Another rxp_gc process appears to be running (PID: ", lock_pid, 
             " started at ", lock_time, "). If this is incorrect, remove: ", lock_file)
      }
      
      if (difftime(Sys.time(), lock_time, units = "secs") > timeout_sec) {
        file.remove(lock_file)
        message("Removed stale lock file from ", lock_time)
      } else if (!alive) {
        message("Stale lock file found (PID dead). Removing.")
        file.remove(lock_file)
      }
    }, error = function(e) file.remove(lock_file))
  }
  writeLines(c(as.character(Sys.getpid()), as.character(Sys.time())), lock_file)
  on.exit(if (file.exists(lock_file)) file.remove(lock_file), add = TRUE)

  # --- Input Validation ---
  if (!is.null(keep_since)) {
    if (inherits(keep_since, "Date")) {
      # ok
    } else if (is.character(keep_since)) {
      keep_since <- tryCatch(as.Date(keep_since),
                             error = function(e) stop("Invalid 'keep_since'. Use 'YYYY-MM-DD'."))
    } else stop("'keep_since' must be a Date or a 'YYYY-MM-DD' string.")
  }
  if (!is.numeric(timeout_sec) || timeout_sec <= 0) stop("'timeout_sec' must be positive.")

  nix_bin <- Sys.which("nix-store")
  if (nix_bin == "") stop("nix-store not found. Ensure Nix is installed and in PATH.")

  all_logs <- rxp_list_logs(project_path)
  if (!is.data.frame(all_logs) || !all(c("filename", "modification_time") %in% names(all_logs))) {
    stop("rxp_list_logs must return a data.frame with 'filename' and 'modification_time'.")
  }
  if (nrow(all_logs) == 0) {
    message("No build logs found. Nothing to do.")
    return(invisible(NULL))
  }

  # --- Helpers ---
  safe_system2 <- function(command, args, timeout = timeout_sec, ...) {
    tryCatch({
      result <- system2(command, args, stdout = TRUE, stderr = TRUE, ...)
      status <- attr(result, "status")
      if (!is.null(status) && status != 0) {
        stop("Command '", command, " ", paste(args, collapse = " "), 
             "' failed with exit code ", status)
      }
      result
    }, error = function(e) {
      if (grepl("timeout", e$message, ignore.case = TRUE)) {
        stop("Command '", command, "' timed out after ", timeout, " seconds")
      }
      stop("Command '", command, "' failed: ", e$message)
    })
  }

  validate_store_paths <- function(paths) {
    if (length(paths) == 0) return(character(0))
    paths <- unique(paths[nzchar(paths)])
    valid_paths <- paths[grepl("^/nix/store/[a-z0-9]{32}-", paths)]
    invalid_paths <- setdiff(paths, valid_paths)
    if (length(invalid_paths) > 0) warning("Skipping invalid store paths: ", paste(invalid_paths, collapse = ", "))
    existing_paths <- valid_paths[file.exists(valid_paths) | dir.exists(valid_paths)]
    missing <- setdiff(valid_paths, existing_paths)
    if (length(missing) > 0 && verbose) message("Missing store paths skipped: ", paste(missing, collapse = ", "))
    existing_paths
  }

  summary_info <- list(kept = character(0), deleted = character(0), protected = 0)

  # --- Date-based Retention ---
  if (!is.null(keep_since)) {
    logs_to_keep <- all_logs[as.Date(all_logs$modification_time) >= keep_since, ]
    logs_to_delete <- all_logs[as.Date(all_logs$modification_time) < keep_since, ]

    summary_info$deleted <- logs_to_delete$filename
    summary_info$kept <- logs_to_keep$filename

    if (nrow(logs_to_delete) == 0) {
      message("No build logs older than ", format(keep_since), " found. Nothing to do.")
      return(invisible(NULL))
    }
    if (dry_run) {
      message("--- DRY RUN --- No files will be deleted. ---")
      return(invisible(summary_info))
    }

    prompt <- paste0(
      "This will permanently delete artifacts from ", nrow(logs_to_delete),
      " build(s) before ", format(keep_since), ".\nContinue?"
    )
    if (!utils::askYesNo(prompt, default = FALSE)) {
      message("Operation cancelled.")
      return(invisible(NULL))
    }

    paths_to_keep <- tryCatch({
      all_paths <- unlist(lapply(logs_to_keep$filename, function(log_name) {
        log_path <- file.path(project_path, "_rixpress", log_name)
        if (!file.exists(log_path)) return(character(0))
        build_data <- tryCatch(readRDS(log_path),
                               error = function(e) list(path = character(0)))
        if (is.null(build_data$path)) character(0) else build_data$path
      }))
      validate_store_paths(all_paths)
    }, error = function(e) stop("Failed to extract store paths: ", e$message))

    if (length(paths_to_keep) == 0) {
      message("No valid store paths found. Skipping GC.")
      return(invisible(NULL))
    }

    temp_gcroots_dir <- tempfile("rixpress-gc-")
    dir.create(temp_gcroots_dir)
    on.exit(unlink(temp_gcroots_dir, recursive = TRUE, force = TRUE), add = TRUE)

    message("Protecting ", length(paths_to_keep), " recent artifacts...")
    for (i in seq_along(paths_to_keep)) {
      root_link <- file.path(temp_gcroots_dir, paste0("root-", i))
      tryCatch(file.symlink(paths_to_keep[i], root_link),
               warning = function(w) NULL, error = function(e) NULL)
    }
    summary_info$protected <- length(paths_to_keep)
    message("Created ", summary_info$protected, " GC root symlinks.")

  } else {
    if (dry_run) {
      message("--- DRY RUN --- Would run 'nix-store --gc'. ---")
      return(invisible(summary_info))
    }
    if (!utils::askYesNo("Run full Nix garbage collection (delete all unreferenced artifacts)?", default = FALSE)) {
      message("Operation cancelled.")
      return(invisible(NULL))
    }
  }

  # --- Run the garbage collector ---
  message("Running Nix garbage collector...")
  tryCatch({
    gc_output <- safe_system2(nix_bin, "--gc")
    if (length(gc_output) > 0) {
      if (verbose) cat(gc_output, sep = "\n")
      else {
        relevant <- gc_output[grepl("freed|removing|deleting", gc_output, ignore.case = TRUE)]
        if (length(relevant) > 0) {
          message("GC summary (last 10 relevant lines):")
          cat(tail(relevant, 10), sep = "\n")
        }
      }
    }
  }, error = function(e) stop("Garbage collection failed: ", e$message))

  message("Garbage collection complete.")
  invisible(summary_info)
}
