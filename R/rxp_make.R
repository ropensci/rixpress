#' Build pipeline using Nix
#'
#' Runs `nix-build` with a quiet flag, outputting to `_rixpress/result`.
#' @importFrom rix nix_build
#' @return A character vector of paths to the built outputs.
#' @export
rxp_make <- function() {
  rix::nix_build(args = c("--quiet", "-o", "_rixpress/result", "pipeline.nix"))
}

#' Export Nix store paths to an archive
#'
#' Creates a single archive file containing the specified Nix store paths
#'   and their dependencies.
#' This archive can be transferred to another machine and imported into
#'   its Nix store.
#'
#' @param store_paths Character, defaults to "_rixpress", the folder holding the
#'   result syslinks.
#' @param archive_file The file name for the output archive (e.g., "my_archive.nar").
#'
#' @export
export_nix_archive <- function(store_paths = "_rixpress", archive_file) {
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
#' @param archive_file The file name of the archive to import
#'   (e.g., "my_archive.nar").
#'
#' @export
import_nix_archive <- function(archive_file) {
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
