#' @title Find matching RDS files for a derivation
#' @description Searches for RDS files in the "result" directory that
#'   match the given derivation name.
#' @param derivation_name The name of the derivation.
#' @return A character vector of paths to the matching RDS files.
#' @noRd
deriv_common <- function(derivation_name, result_path = "_rixpress") {
  files <- list.files(paste0(result_path, "/result"), full.names = TRUE)
  pattern <- paste0(derivation_name, ".*\\.rds$")
  matching_files <- files[grepl(pattern, basename(files))]
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
deriv_read <- function(derivation_name) {
  matching_files <- deriv_common(derivation_name)
  readRDS(matching_files)
}

#' @title Load a derivation's output into global environment
#' @param derivation_name Character, the name of the derivation.
#' @return None. The derivation object is assigned to the
#'   global environment with the name `derivation_name`.
deriv_load <- function(derivation_name) {
  matching_files <- deriv_common(derivation_name)
  assign(derivation_name, readRDS(matching_files), envir = .GlobalEnv)
}
