#' Build pipeline using Nix
#'
#' Runs `nix-build` with a quiet flag, outputting to `_rixpress/result`.
#' @param verbose Logical, defaults to FALSE. Set to TRUE to see nix's
#'   standard output, can be useful to check what is happening if the
#'   build process takes long.
#' @importFrom processx run
#' @return A character vector of paths to the built outputs.
#' @export
rxp_make <- function(verbose = FALSE) {
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
      "--realize",
      "--verbose",
      "--keep-going",
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
      build_success = sapply(output_checks, `[[`, "build_success"),
      path = sapply(output_checks, `[[`, "path"),
      output = I(lapply(output_checks, `[[`, "files"))
    )
  })

  build_log <- do.call(rbind, build_log)

  saveRDS(build_log, "_rixpress/build_log.rds")

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
      "Read individual derivations using `rxp_read()` or\n",
      "load them into the global environment using `rxp_load()`."
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
#' @param archive_file Character, path to the archive, defaults to
#'   "_rixpress/pipeline-outputs.nar"
#'
#' @export
export_nix_archive <- function(
  archive_file = "_rixpress/pipeline-outputs.nar"
) {
  if (!file.exists("_rixpress/build_log.rds")) {
    stop("Build the pipeline before exporting archive.")
  }

  store_paths <- readRDS("_rixpress/build_log.rds")$path

  message("Exporting store paths to ", archive_file)
  system2("nix-store", args = c("--export", store_paths), stdout = archive_file)
  message("Export completed")
}

#' Import Nix store paths from an archive
#'
#' Imports the store paths contained in an archive file into the local Nix store.
#' Useful for transferring built outputs between machines.
#'
#' @param archive_file Character, path to the archive, defaults to
#'   "_rixpress/pipeline-outputs.nar"
#'
#' @export
import_nix_archive <- function(
  archive_file = "_rixpress/pipeline-outputs.nar"
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
