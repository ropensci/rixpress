library(jsonlite)  # For parsing JSON
library(stringr)   # For string manipulation

#' Update pipeline.nix with readRDS calls based on dag.json dependencies
#' @param dag_file Path to dag.json
#' @param nix_file Path to pipeline.nix
# Example usage
# gen_pipeline("dag.json", "flat_pipeline.nix")
