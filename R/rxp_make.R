#' Build pipeline using Nix
#'
#' Runs `nix-build` with a quiet flag, outputting to `_rixpress/result`.
#' @importFrom rix nix_build
#' @return A character vector of paths to the built outputs.
#' @export
rxp_make <- function() {
  rix::nix_build(
    args = c(
      "--log-format",
      "bar",
      "--no-out-link",
      "--quiet",
      "pipeline.nix"
    )
  )

  build_log <- rxp_inspect_internal()

  saveRDS(build_log, "_rixpress/build_log.rds")

  failures <- subset(build_log, subset = !build_success)
  if (nrow(failures) > 0) {
    warning(
      "Build failures:\n",
      paste(capture.output(print(failures)), collapse = "\n")
    )
  }
}

#' Export Nix store paths to an archive
#'
#' Creates a single archive file containing the specified Nix store paths
#'   and their dependencies.
#' This archive can be transferred to another machine and imported into
#'   its Nix store.
#'
#' @param store_paths Character, defaults to "_rixpress", the folder holding the
#'   result symlinks.
#' @param archive_file Character, path to the archive, defaults to
#'   "_rixpress/pipeline-outputs.nar"
#'
#' @export
export_nix_archive <- function(
  store_paths = "_rixpress",
  archive_file = "_rixpress/pipeline-outputs.nar"
) {
  if (!is.character(store_paths) || length(store_paths) == 0) {
    stop("store_paths must be a non-empty character vector")
  }
  if (!is.character(archive_file) || length(archive_file) != 1) {
    stop("archive_file must be a single character string")
  }
  store_paths <- Sys.readlink(list.files(
    "_rixpress/",
    "result*",
    full.names = TRUE
  ))
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
