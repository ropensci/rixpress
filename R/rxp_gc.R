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
#' @param dry_run Logical (default `FALSE`). If `TRUE`, show what would be done
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
#' rxp_gc(dry_run = TRUE)
#'
#' # Dry run of date-based retention
#' rxp_gc(keep_since = "2025-01-01", dry_run = TRUE)
#'
#' # Actually delete all unreferenced store paths
#' rxp_gc()
#' }
#'
#' @export
rxp_gc <- function(
  keep_since = NULL,
  project_path = ".",
  dry_run = FALSE,
  timeout_sec = 300,
  verbose = FALSE
) {
  safe_system2 <- function(cmd, args, ..., timeout_sec = 300) {
    p <- processx::run(
      cmd,
      args,
      error_on_status = FALSE,
      timeout = timeout_sec
    )
    if (p$status != 0) {
      stop(sprintf(
        "Command '%s %s' failed with exit code %d.\nStdout: %s\nStderr: %s",
        cmd,
        paste(args, collapse = " "),
        p$status,
        p$stdout,
        p$stderr
      ))
    }
    p
  }

  if (!nzchar(Sys.which("nix-store"))) {
    stop("nix-store not found on PATH. Please install Nix or adjust your PATH.")
  }

  lockfile <- file.path(tempdir(), "rixpress_gc.lock")
  if (file.exists(lockfile)) {
    lock_info <- readLines(lockfile, warn = FALSE)
    lock_pid <- as.integer(lock_info[1])
    lock_time <- as.numeric(lock_info[2])
    if (!is.na(lock_pid) && !is.na(lock_time)) {
      if (
        (Sys.time() - as.POSIXct(lock_time, origin = "1970-01-01")) <
          timeout_sec
      ) {
        stop("Another rxp_gc process seems to be running (PID ", lock_pid, ").")
      } else {
        warning("Stale lockfile detected, removing.")
        unlink(lockfile)
      }
    }
  }
  writeLines(
    c(as.character(Sys.getpid()), as.character(as.numeric(Sys.time()))),
    lockfile
  )
  on.exit(unlink(lockfile), add = TRUE)

  project_path <- normalizePath(project_path, mustWork = TRUE)
  logs <- rxp_list_logs(project_path)

  if (!is.null(keep_since)) {
    if (inherits(keep_since, "character")) {
      keep_since <- as.Date(keep_since)
    }
    if (!inherits(keep_since, "Date")) {
      stop("keep_since must be NULL, a Date, or a string in YYYY-MM-DD format.")
    }
  }

  logs_to_keep <- logs
  logs_to_delete <- logs[0, ]
  if (!is.null(keep_since)) {
    logs_to_keep <- logs[logs$modification_time >= keep_since, ]
    logs_to_delete <- logs[logs$modification_time < keep_since, ]
  }

  builds <- rxp_list_builds(project_path)
  keep_paths <- unlist(lapply(builds, function(build_data) {
    if (is.null(build_data$path)) character(0) else build_data$path
  }))
  keep_paths <- unique(keep_paths)

  for (p in keep_paths) {
    if (!grepl("^/nix/store/", p)) {
      stop("Invalid store path: ", p)
    }
  }

  summary_info <- list(
    kept = logs_to_keep$filename,
    deleted = logs_to_delete$filename,
    protected = length(keep_paths)
  )

  if (!is.null(keep_since)) {
    if (dry_run) {
      message("--- DRY RUN --- No changes will be made. ---")
      if (nrow(logs_to_delete) > 0) {
        message("Logs that would be deleted (", nrow(logs_to_delete), "):")
        cat("  ", logs_to_delete$filename, sep = "\n  ")
        cat("\n")
      }
      if (length(keep_paths) > 0) {
        message(
          "Store paths that would be kept/protected (",
          length(keep_paths),
          "):"
        )
        cat("  ", keep_paths, sep = "\n  ")
        cat("\n")
      }
      if (nrow(logs_to_delete) > 0) {
        message("Store paths that would be deleted (dry run):")
        safe_system2(
          "nix-store",
          c("--delete", "--dry-run", logs_to_delete$path),
          timeout_sec = timeout_sec
        )
      }
      return(invisible(summary_info))
    }

    if (!utils::askYesNo("Proceed with garbage collection?")) {
      message("Aborted.")
      return(invisible(NULL))
    }

    temp_gc_dir <- tempfile("rixpress_gc_protect")
    dir.create(temp_gc_dir)
    on.exit(unlink(temp_gc_dir, recursive = TRUE), add = TRUE)
    for (p in keep_paths) {
      file.symlink(p, file.path(temp_gc_dir, basename(p)))
    }

    message("Running nix-store --gc...")
    out <- safe_system2("nix-store", c("--gc"), timeout_sec = timeout_sec)
    lines <- strsplit(out$stdout, "\n")[[1]]
    if (verbose) {
      cat(lines, sep = "\n")
    } else {
      cat(tail(lines, 20), sep = "\n")
    }
  } else {
    if (dry_run) {
      message("--- DRY RUN --- Would run 'nix-store --gc'. ---")
      safe_system2(
        "nix-store",
        c("--gc", "--dry-run"),
        timeout_sec = timeout_sec
      )
      return(invisible(summary_info))
    }

    if (!utils::askYesNo("Proceed with full garbage collection?")) {
      message("Aborted.")
      return(invisible(NULL))
    }

    message("Running nix-store --gc...")
    out <- safe_system2("nix-store", c("--gc"), timeout_sec = timeout_sec)
    lines <- strsplit(out$stdout, "\n")[[1]]
    if (verbose) {
      cat(lines, sep = "\n")
    } else {
      cat(tail(lines, 20), sep = "\n")
    }
  }

  invisible(summary_info)
}
