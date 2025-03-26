#' @noRd
rxp_inspect_internal <- function() {
  json_output <- system2(
    "nix-instantiate",
    args = c("--eval", "--strict", "--json", "pipeline.nix"),
    stdout = TRUE
  )

  json_output <- paste(json_output, collapse = "")

  build_paths_json <- jsonlite::fromJSON(json_output)

  build_paths_df <- data.frame(
    derivation = names(build_paths_json),
    paths = unlist(build_paths_json, use.names = FALSE)
  )

  build_paths_df$output <- I(sapply(
    build_paths_df$paths,
    \(x) list(list.files(x, all.files = FALSE))
  ))

  build_paths_df$build_success <- sapply(
    build_paths_df$output,
    \(x) (!identical(x, character(0))),
    USE.NAMES = FALSE
  )

  build_paths_df[, c("derivation", "build_success", "output", "paths")]
}


#' @title Inspect a buggy pipeline
#' @description Returns a data frame with three columns:
#'     - derivation: the name of the derivation
#'     - build_success: whether the build was successful or not
#'     - paths: the path of this derivation in the Nix store
#'     - output: the output, if this derivation was built successfully.
#'               Empty outputs mean that this derivation was not built
#'               successfully.
#'
#' @importFrom jsonlite fromJSON
#' @return A data frame with derivation names, if their build was successful,
#'   their paths in the /nix/store, and their build outputs.
#' @export
rxp_inspect <- function() {
  readRDS("_rixpress/build_log.rds")
}
