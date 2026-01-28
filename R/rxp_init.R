#' Initialize Rixpress Project
#'
#' @family utilities
#' @description
#' Generates `gen-env.R` and `gen-pipeline.R` scripts in the specified project
#'   directory, after asking the user for confirmation. If the user declines, no
#'   changes are made.
#'
#' @param project_path Character string specifying the project's path.
#' @param skip_prompt Logical. If TRUE, skips all confirmation prompts and proceeds
#'   with initialization, useful on continuous integration. Defaults to FALSE.
#'
#' @details
#' Creates (overwriting if they already exist):
#' - `gen-env.R`: Script to define an execution environment with `{rix}`.
#' - `gen-pipeline.R`: Defines a data pipeline with `{rixpress}`.
#'
#' @return Logical. Returns TRUE if initialization was successful, FALSE if the
#'   operation was cancelled by the user.
#'
#' @examples
#' # Default usage (will prompt before any action)
#' \dontrun{
#'   rxp_init()
#' }
#'
#' @export
rxp_init <- function(project_path = ".", skip_prompt = FALSE) {
  confirm <- function(question) {
    if (skip_prompt) {
      return(TRUE)
    }
    ans <- readline(paste0(question, " [y/n]: "))
    tolower(substr(ans, 1, 1)) == "y" # just pick the first letter and lower it
  }

  # Initial confirmation before any action
  if (!confirm(paste0("Initialize project at '", project_path, "'?"))) {
    message(
      "Operation cancelled by user. No files or directories were created."
    )
    return(invisible(FALSE))
  }

  # Ensure project_path exists, create it if it doesn't
  if (!dir.exists(project_path)) {
    dir.create(project_path, recursive = TRUE)
  }

  # Define file paths
  env_file <- file.path(project_path, "gen-env.R")
  pipeline_file <- file.path(project_path, "gen-pipeline.R")

  gen_env_lines <- c(
    "# This script defines the default environment the pipeline runs in.",
    "# Add the required packages to execute the code necessary for each derivation.",
    "# If you want to create visual representations of the pipeline, consider adding",
    "# `{visNetwork}` and `{ggdag}` to the list of R packages.",
    "library(rix)",
    "",
    "# Define execution environment",
    "rix(",
    "  date = NULL,",
    "  r_pkgs = NULL,",
    "  py_conf = NULL,",
    "  git_pkgs = list(",
    "    \"package_name\" = \"rixpress\",",
    "    \"repo_url\" = \"https://github.com/ropensci/rixpress\",",
    "    \"commit\" = \"4d5da094c2dc33d13f9b4966b9c502b328086714\",",
    "  ),",
    "  ide = \"none\",",
    "  project_path = \".\"",
    ")"
  )

  gen_pipeline_lines <- c(
    "library(rixpress)",
    "library(igraph)",
    "",
    "list(",
    "  rxp_r_file(",
    "    name = NULL,",
    "    path = NULL,",
    "    read_function = \\(x) read.csv(file = x, sep = \",\")",
    "  ),",
    "  rxp_r(",
    "    name = NULL,",
    "    expr = NULL",
    "  )",
    ") |>",
    "  rxp_populate(build = FALSE)"
  )

  writeLines(gen_env_lines, env_file)
  message("File ", env_file, " has been written.")
  writeLines(gen_pipeline_lines, pipeline_file)
  message("File ", pipeline_file, " has been written.")

  # Skip Git initialization when on CRAN, CI, or during testing
  if (!interactive()) {
    message(
      "Skipping Git initialization (non-interactive session, CRAN, CI, or test environment detected)."
    )
    return(invisible(TRUE))
  }

  if (confirm("Would you like to initialise a Git repository here?")) {
    if (!requireNamespace("usethis", quietly = TRUE)) {
      message(
        "The 'usethis' package is not installed. ",
        "Please install it and then run `usethis::use_git()` manually."
      )
    } else {
      usethis::use_git()
    }
  } else {
    message("Skipping Git initialization.")
  }

  invisible(TRUE)
}
