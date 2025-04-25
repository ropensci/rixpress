#' Initialize project
#'
#' @description
#' Generates `gen-env.R` and `gen-pipeline.R` scripts in the specified project
#'   directory.
#'
#' @param project_path Character string specifying the project's path.
#' @param overwrite Logical, defaults to FALSE, indicating whether to overwrite
#'   existing scripts.
#'
#' @details
#' Creates:
#' - `gen-env.R`: Script to define an execution environment with `{rix}`.
#' - `gen-pipeline.R`: Defines a data pipeline with `{rixpress}`.
#'
#' @examples
#' # Default usage
#' \dontrun{
#'   rxp_init()
#' }
#'
#' @export
rxp_init <- function(project_path, overwrite = FALSE) {
  # Ensure project_path exists, create it if it doesn't
  if (!dir.exists(project_path)) {
    dir.create(project_path, recursive = TRUE)
  }

  gen_env_lines <- c(
    "library(rix)",
    "",
    "# Define execution environment",
    "rix(",
    "  date = NULL,",
    "  r_pkgs = NULL,",
    "  git_pkgs = list(",
    "    package_name = \"rixpress\"",
    "    repo_url = \"https://github.com/b-rodrigues/rixpress\"",
    "    commit = \"HEAD\"",
    "  )",
    "  ide = \"none\",",
    "  project_path = \".\",",
    "  overwrite = TRUE",
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
    "    read_function = \\(x) (read.csv(file = x, sep = \",\"))",
    "  ),",
    "",
    "  rxp_r(",
    "    name = NULL,",
    "    expr = NULL",
    "  )",
    ") |>",
    "  rixpress(project_path = \".\")"
  )
  env_file <- file.path(project_path, "gen-env.R")
  pipeline_file <- file.path(project_path, "gen-pipeline.R")

  if (!file.exists(env_file) || overwrite) {
    writeLines(gen_env_lines, env_file)
    message("File ", env_file, " has been written.")
  } else {
    message(
      "File ",
      env_file,
      " already exists and `overwrite` is FALSE. Skipping."
    )
  }

  if (!file.exists(pipeline_file) || overwrite) {
    writeLines(gen_pipeline_lines, pipeline_file)
    message("File ", pipeline_file, " has been written.")
  } else {
    message(
      "File ",
      pipeline_file,
      " already exists and `overwrite` is FALSE. Skipping."
    )
  }
}
