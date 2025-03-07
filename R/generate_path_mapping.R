#' Generate a JSON file with derivation paths and dependencies from a Nix file
#'
#' @param nix_file Path to the Nix file (default: "pipeline.nix")
#' @param attributes Vector of derivation attributes (e.g., c("mtcars_am", "mtcars_head"))
#' @param output_file Path to the output JSON file (default: "pathMapping.json")
#' @importFrom jsonlite write_json
#' @return Creates the JSON file and invisibly returns the JSON structure as a list
# generate_path_mapping("pipeline.nix", c("mtcars_am", "mtcars_head"))
generate_path_mapping <- function(
  nix_file = "pipeline.nix",
  attributes,
  output_file = "pathMapping.json"
) {
  # Check if attributes are provided
  if (missing(attributes)) {
    stop(
      "Please provide a vector of derivation attributes (e.g., c('mtcars_am', 'mtcars_head'))"
    )
  }

  # Step 1: Get .drv paths for each attribute using nix-instantiate
  drv_paths <- list()
  for (attr in attributes) {
    cmd <- sprintf("nix-instantiate %s -A %s", nix_file, attr)
    drv_path <- system(cmd, intern = TRUE)
    if (length(drv_path) == 0) {
      stop(sprintf("Failed to instantiate %s for attribute %s", nix_file, attr))
    }
    drv_paths[[attr]] <- drv_path[1] # Take the first line (the .drv path)
  }

  # Step 2: Get dependencies for each .drv using nix-store --query --references
  dependencies <- list()
  for (attr in attributes) {
    drv <- drv_paths[[attr]]
    cmd <- sprintf("nix-store --query --references %s", drv)
    refs <- system(cmd, intern = TRUE)
    # Filter to dependencies that are in our attributes list
    deps <- names(drv_paths)[sapply(drv_paths, function(d) d %in% refs)]
    dependencies[[attr]] <- deps
  }

  # Step 3: Get output paths using nix-store --query --outputs
  output_paths <- list()
  for (attr in attributes) {
    drv <- drv_paths[[attr]]
    cmd <- sprintf("nix-store --query --outputs %s", drv)
    out_path <- system(cmd, intern = TRUE)[1] # Take the first output path
    output_paths[[attr]] <- paste0(out_path, "/", attr, ".rds") # Append output file name
  }

  # Step 4: Build the JSON structure
  path_mapping <- list()
  for (attr in attributes) {
    path_mapping[[attr]] <- list(
      path = output_paths[[attr]],
      depends = as.list(dependencies[[attr]])
    )
  }

  # Step 5: Write to JSON file
  jsonlite::write_json(path_mapping, output_file, pretty = TRUE)

  # Notify the user
  cat(sprintf("Generated %s with paths and dependencies\n", output_file))

  # Return the structure invisibly for further use if needed
  invisible(path_mapping)
}
