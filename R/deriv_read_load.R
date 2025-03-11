#' @title Find matching RDS files for a derivation
#' @description Searches for RDS files in the "result" directory that
#'   match the given derivation name.
#' @param derivation_name The name of the derivation.
#' @param result_path The folder containing the pipelines' outputs.
#' @return A character vector of paths to the matching RDS files.
#' @noRd
rxp_common <- function(derivation_name, result_path = "_rixpress") {
  if (grepl("^/nix/store/", derivation_name)) {
    return(derivation_name)
  }
  files <- list.files(paste0(result_path, "/result"), full.names = TRUE)
  matching_files <- files[grepl(derivation_name, basename(files))]
  if (length(matching_files) == 0) {
    stop(paste(
      "No derivation called ",
      derivation_name,
      " found. Did you build the pipeline?"
    ))
  } else {
    matching_files
  }
}

#' @title Read output of a derivation
#' @param derivation_name Character, the name of the derivation.
#' @return The derivation's output.
#' @export
rxp_read <- function(derivation_name, result_path = "_rixpress") {
  matching_files <- rxp_common(derivation_name, result_path = "_rixpress")
  if (all(grepl(".*\\.rds$", matching_files) & length(matching_files) == 1)) {
    readRDS(matching_files)
  } else {
    matching_files
  }
}

#' @title Load a derivation's output into global environment
#' @param derivation_name Character, the name of the derivation.
#' @return None. The derivation object is assigned to the
#'   global environment with the name `derivation_name`.
#' @export
rxp_load <- function(derivation_name, result_path = "_rixpress") {
  matching_files <- rxp_common(derivation_name, result_path = "_rixpress")
  if (all(grepl(".*\\.rds$", matching_files) & length(matching_files) == 1)) {
    assign(derivation_name, readRDS(matching_files), envir = .GlobalEnv)
  } else {
    matching_files
  }
}
