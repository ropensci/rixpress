#' @title Inspect the build result of a pipeline.
#' @description Returns a data frame with four columns:
#'     - derivation: the name of the derivation
#'     - build_success: whether the build was successful or not
#'     - path: the path of this derivation in the Nix store
#'     - output: the output, if this derivation was built successfully.
#'               Empty outputs mean that this derivation was not built
#'               successfully. Several outputs for a single derivation
#'               are possible.
#'
#' @return A data frame with derivation names, if their build was successful,
#'   their paths in the /nix/store, and their build outputs.
#' @export
rxp_inspect <- function() {
  readRDS("_rixpress/build_log.rds")
}
