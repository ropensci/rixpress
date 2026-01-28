#' Validate verbose parameter and handle backward compatibility
#'
#' @param verbose The verbose parameter value to validate
#' @return A single non-negative integer
#' @keywords internal
#' @noRd
.rxp_validate_verbose <- function(verbose) {
  if (!is.numeric(verbose) || length(verbose) != 1) {
    stop("verbose must be a single numeric or integer value", call. = FALSE)
  }

  verbose <- as.integer(verbose)

  if (verbose < 0) {
    stop("rxp_make(): verbose must be a non-negative integer", call. = FALSE)
  }

  if (verbose > 5L) {
    warning(
      sprintf(
        "Argument 'verbose' (%d) exceeds the maximum of 5; using 5.",
        verbose
      ),
      call. = FALSE
    )
    verbose <- 5L
  }

  verbose
}

#' Parse Nix build output to track derivation progress
#'
#' @param line Output line from nix-store
#' @param derivation_names Vector of derivation names to track
#' @return List with status info, or NULL if not a relevant line
#' @keywords internal
#' @noRd
.rxp_parse_build_line <- function(line, derivation_names) {
  # Handle final store paths (when derivations are already built)
  if (grepl("^/nix/store/.*-[^/]+$", line)) {
    for (deriv_name in derivation_names) {
      if (grepl(paste0("-", deriv_name, "$"), line)) {
        # Skip internal derivations from progress display
        if (deriv_name == "all-derivations") {
          return(NULL)
        }
        return(list(
          derivation = deriv_name,
          status = "completed",
          message = line,
          from_store = TRUE
        ))
      }
    }
  }

  # Handle dispatch: "building '/nix/store/...-derivation_name.drv'..."
  if (grepl("building '/nix/store/.*\\.drv'\\.\\.\\.", line)) {
    for (deriv_name in derivation_names) {
      if (grepl(paste0("-", deriv_name, "\\.drv"), line)) {
        # Skip internal derivations from progress display
        if (deriv_name == "all-derivations") {
          return(NULL)
        }
        return(list(
          derivation = deriv_name,
          status = "dispatched",
          message = line,
          from_store = FALSE
        ))
      }
    }
  }

  # Handle direct build failures: "error: builder for '/nix/store/...-derivation_name.drv' failed"
  if (grepl("error: builder for.*failed", line)) {
    for (deriv_name in derivation_names) {
      if (grepl(paste0("-", deriv_name, "\\.drv"), line)) {
        return(list(
          derivation = deriv_name,
          status = "errored",
          message = line,
          from_store = FALSE
        ))
      }
    }
  }

  # Handle dependency failures: "error: X dependencies of derivation '/nix/store/...-derivation_name.drv' failed"
  if (grepl("error: .* dependencies of derivation.*failed", line)) {
    for (deriv_name in derivation_names) {
      if (grepl(paste0("-", deriv_name, "\\.drv"), line)) {
        return(list(
          derivation = deriv_name,
          status = "errored",
          message = line,
          from_store = FALSE
        ))
      }
    }
  }

  # Capture actual R/Python/etc errors (these are the useful error messages)
  if (grepl("^Error in.*:|^Error:|^Fatal error", line)) {
    return(list(
      derivation = "RUNTIME_ERROR", # Special marker
      status = "runtime_error",
      message = line,
      from_store = FALSE
    ))
  }

  return(NULL)
}

#' Prepare nix-store command arguments
#'
#' @param max_jobs Integer, number of derivations to be built in parallel
#' @param cores Integer, number of cores a derivation can use during build
#' @param drv_paths Character vector of derivation paths
#' @param verbose Integer, verbosity level (0 = silent, >=1 = verbose)
#' @return Character vector of command arguments
#' @keywords internal
#' @noRd
.rxp_prepare_nix_store_args <- function(max_jobs, cores, drv_paths, verbose) {
  args <- c(
    "--realise",
    "--keep-going",
    "--max-jobs",
    max_jobs,
    "--cores",
    cores
  )

  # Add --verbose flags based on verbosity level
  if (verbose > 0) {
    verbose_flags <- rep("--verbose", verbose)
    args <- c(args[1], verbose_flags, args[-1])
  }

  # Add derivation paths at the end
  c(args, drv_paths)
}

#' Build Pipeline Using Nix
#'
#' Runs `nix-build` with a quiet flag, outputting to `_rixpress/result`.
#' @family pipeline functions
#' @param verbose Integer, defaults to 0L. Verbosity level: 0 = show progress
#'   indicators only, 1+ = show nix output with increasing verbosity. 0:
#'   "Progress only", 1: "Informational", 2: "Talkative", 3: "Chatty", 4: "Debug",
#'   5: "Vomit". Values higher than 5 are capped to 5.
#'   Each level adds one --verbose flag to nix-store command.
#' @param max_jobs Integer, number of derivations to be built in parallel.
#' @param cores Integer, number of cores a derivation can use during build.
#' @importFrom processx run
#' @importFrom utils capture.output
#' @return A character vector of paths to the built outputs.
#' @examples
#' \dontrun{
#'   # Build the pipeline with progress indicators (default)
#'   rxp_make()
#'
#'   # Build with verbose output and parallel execution
#'   rxp_make(verbose = 2, max_jobs = 4, cores = 2)
#'
#'   # Maximum verbosity
#'   rxp_make(verbose = 3)
#' }
#' @export
rxp_make <- function(verbose = 0L, max_jobs = 1, cores = 1) {
  # [existing validation code]
  verbose <- .rxp_validate_verbose(verbose)

  message("Build process started...\n")

  # [existing instantiation code]
  instantiate <- processx::run(
    command = "nix-instantiate",
    args = "pipeline.nix",
    error_on_status = FALSE,
    spinner = TRUE
  )

  if (instantiate$status != 0) {
    cat(instantiate$stderr)
    stop("nix-instantiate failed")
  }
  drv_paths <- strsplit(instantiate$stdout, "\n")[[1]]
  # Extract derivation names: everything after hash- and before .drv
  derivation_names <- gsub("^/nix/store/[^-]+-(.+)\\.drv$", "\\1", drv_paths)
  # Filter out only the internal "all-derivations"
  derivation_names <- derivation_names[derivation_names != "all-derivations"]

  # Initialize progress tracking
  build_status <- setNames(
    rep("pending", length(derivation_names)),
    derivation_names
  )
  error_messages <- setNames(
    rep(as.character(NA), length(derivation_names)),
    derivation_names
  )
  currently_building <- NULL # Track which derivation is currently building

  # Set up callbacks based on verbosity level
  if (verbose >= 1) {
    cb_stdout <- function(line, proc) cat(line, "\n")
    cb_stderr <- function(line, proc) cat(line, "\n")
  } else {
    # verbose == 0: show progress only
    cb_stdout <- function(line, proc) {
      # Skip the garbage collector warning
      if (grepl("warning.*garbage collector", line)) {
        return()
      }

      parsed <- .rxp_parse_build_line(line, derivation_names)
      if (!is.null(parsed)) {
        if (parsed$status == "completed" && parsed$from_store) {
          cat(paste0("\u2713 ", parsed$derivation, " built\n"))
          build_status[[parsed$derivation]] <<- "completed"
        }
      }
    }

    cb_stderr <- function(line, proc) {
      # Skip garbage collector warnings
      if (grepl("warning.*garbage collector", line)) {
        return()
      }

      parsed <- .rxp_parse_build_line(line, derivation_names)
      if (!is.null(parsed)) {
        if (parsed$status == "dispatched") {
          cat(paste0("+ ", parsed$derivation, " building\n"))
          build_status[[parsed$derivation]] <<- "dispatched"
          currently_building <<- parsed$derivation # Track current build
        } else if (parsed$status == "completed" && !parsed$from_store) {
          cat(paste0("\u2713 ", parsed$derivation, " built\n"))
          build_status[[parsed$derivation]] <<- "completed"
          currently_building <<- NULL
        } else if (parsed$status == "errored") {
          cat(paste0("\u2716 ", parsed$derivation, " errored\n"))
          build_status[[parsed$derivation]] <<- "errored"
          # Only store the Nix error message if we don't already have a runtime error
          if (is.na(error_messages[[parsed$derivation]])) {
            error_messages[[parsed$derivation]] <<- parsed$message
          }
          currently_building <<- NULL
        } else if (
          parsed$status == "runtime_error" && !is.null(currently_building)
        ) {
          # Associate runtime errors with currently building derivation
          # Runtime errors are more informative, so always store them
          error_messages[[currently_building]] <<- parsed$message
        }
      }
    }
  }

  # [rest of the existing build process code]
  nix_store_args <- .rxp_prepare_nix_store_args(
    max_jobs,
    cores,
    drv_paths,
    verbose
  )

  build_process <- processx::run(
    command = "nix-store",
    args = nix_store_args,
    stdout_line_callback = cb_stdout,
    stderr_line_callback = cb_stderr,
    error_on_status = FALSE,
    spinner = ifelse(verbose >= 1, TRUE, FALSE)
  )

  # Show final status for verbose = 0
  if (verbose == 0) {
    completed_count <- sum(build_status == "completed")
    errored_count <- sum(build_status == "errored")

    if (build_process$status == 0) {
      cat(paste0(
        "\u2713 pipeline completed [",
        completed_count,
        " completed, 0 errored]\n"
      ))
    } else {
      cat(paste0(
        "\u2716 errored pipeline [",
        completed_count,
        " completed, ",
        errored_count,
        " errored]\n"
      ))
    }
  }

  # [rest of existing build log creation code - unchanged]
  build_log <- lapply(drv_paths, function(drv_path) {
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

    deriv_name <- gsub("\\.drv$", "", gsub("^[^-]*-", "", drv_path))

    data.frame(
      derivation = deriv_name,
      build_success = vapply(output_checks, `[[`, logical(1), "build_success"),
      path = vapply(output_checks, `[[`, character(1), "path"),
      output = I(lapply(output_checks, `[[`, "files")),
      error_message = ifelse(
        deriv_name %in%
          names(error_messages) &&
          !is.na(error_messages[[deriv_name]]),
        error_messages[[deriv_name]],
        NA_character_
      ),
      stringsAsFactors = FALSE
    )
  })

  build_log <- do.call(rbind, build_log)

  # [rest of existing code unchanged...]
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

  all_derivs_row <- which(build_log$derivation == "all-derivations")
  all_derivs_hash <- ""
  if (length(all_derivs_row) > 0) {
    path <- build_log$path[all_derivs_row[1]]
    all_derivs_hash <- gsub(
      "^/nix/store/([^-]+)-all-derivations.*$",
      "\\1",
      path
    )
    if (all_derivs_hash == path) {
      all_derivs_hash <- ""
    }
  }

  # choose filename with .json instead of .rds
  log_filename <- sprintf(
    "_rixpress/build_log_%s%s.json",
    timestamp,
    ifelse(all_derivs_hash != "", paste0("_", all_derivs_hash), "")
  )

  # save JSON instead of RDS
  jsonlite::write_json(
    x = build_log,
    path = log_filename,
    pretty = TRUE,
    auto_unbox = TRUE,
    dataframe = "rows" # each row as a JSON object
  )

  failures <- subset(build_log, subset = !build_success)
  if (nrow(failures) > 0) {
    warning(
      "Build failures:\n",
      paste(capture.output(print(failures)), collapse = "\n")
    )
  }

  if (build_process$status != 0) {
    if (verbose >= 1) {
      cat(build_process$stderr)
    }
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
#' @family archive caching functions
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
#'   rxp_export_artifacts()
#'
#'   # Export a specific build to a custom location
#'   rxp_export_artifacts(
#'     archive_file = "my_archive.nar",
#'     which_log = "20250510"
#'   )
#' }
#' @export
rxp_export_artifacts <- function(
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

  # Read JSON instead of RDS
  build_log <- jsonlite::read_json(log_path, simplifyVector = TRUE)
  store_paths <- build_log$path

  message("Exporting store paths to ", archive_file)
  system2("nix-store", args = c("--export", store_paths), stdout = archive_file)
  message("Export completed")
}

#' Import Nix store paths from an archive
#'
#' Imports the store paths contained in an archive file into the local Nix store.
#' Useful for transferring built outputs between machines.
#'
#' @family archive caching functions
#' @param archive_file Character, path to the archive, defaults to
#'   "_rixpress/pipeline-outputs.nar"
#' @return Nothing, imports the archive contents into the local Nix store.
#' @examples
#' \dontrun{
#'   # Import from the default archive location
#'   rxp_import_artifacts()
#'
#'   # Import from a custom archive file
#'   rxp_import_artifacts("path/to/my_archive.nar")
#' }
#' @export
rxp_import_artifacts <- function(
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
