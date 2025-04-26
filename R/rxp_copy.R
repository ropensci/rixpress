#' @title Copy derivations from the Nix store to current working directory
#' @description When Nix builds a derivation, its output is saved in the
#'   Nix store located under `/nix/store/`. Even though you can import the
#'   derivations into the current R session using `rxp_read()` or `rxp_load()`,
#'   it can be useful to copy the outputs to the current working directory. This
#'   is especially useful for Quarto documents, where there can be more than one
#'   input, as is the case for `html` output.
#' @param derivation_name The name of the derivation to copy. If empty, then all
#'   the derivations are copied.
#' @return Nothing, the contents of the Nix store are copied to the current
#'   working directory.
#' @export
rxp_copy <- function(derivation_name = NULL) {
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
    all_files <- list.files(output_dir, full.names = TRUE)
    Sys.chmod(all_files, mode = "777")
    status <- 1
  } else {
    derivation_name <- "all-derivations"
    derivation_path <- subset(
      build_log,
      subset = derivation == derivation_name
    )$path
    files <- list.files(derivation_path, full.names = TRUE)
    file.copy(from = files, to = output_dir, recursive = TRUE)
    all_files <- list.files(output_dir, full.names = TRUE)
    Sys.chmod(all_files, mode = "777")
    status <- 1
  }

  if (status == 1) {
    message("Copy successful, check out ", output_dir)
  } else {
    stop("Copy unsuccessful: did you build the pipeline?")
  }
}
