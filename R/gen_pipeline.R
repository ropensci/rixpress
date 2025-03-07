library(jsonlite)  # For parsing JSON
library(stringr)   # For string manipulation

#' Update pipeline.nix with readRDS calls based on dag.json dependencies
#' @param dag_file Path to dag.json
#' @param nix_file Path to pipeline.nix
gen_pipeline <- function(dag_file = "dag.json", flat_pipeline = "flat_pipeline.nix") {
  # Step 1: Read and parse dag.json
  dag <- fromJSON(dag_file)
  derivations <- dag$derivations
  
  # Step 2: Read pipeline.nix as lines
  flat_pipeline_lines <- readLines(flat_pipeline)
  
  # Step 3: Process each derivation
  for (deriv in derivations) {
    # Skip if no dependencies
    if (length(deriv$depends) == 0) next
    
    deriv_name <- deriv$deriv_name
    deps <- deriv$depends
    
    # Find the derivation definition start
    pattern <- paste0("  ", deriv_name, " = makeRDerivation \\{")
    start_idx <- grep(pattern, flat_pipeline_lines)
    if (length(start_idx) == 0) {
      warning(paste("Derivation", deriv_name, "not found in", nix_file))
      next
    }
    
    # Find the buildPhase line within this derivation
    build_phase_idx <- grep("    buildPhase = ''", flat_pipeline_lines)
    build_phase_idx <- build_phase_idx[build_phase_idx > start_idx][1]
    if (is.na(build_phase_idx)) {
      warning(paste("buildPhase not found for", deriv_name))
      next
    }
    
    # Find the Rscript line within the buildPhase
    rscript_idx <- grep("      Rscript -e \"", flat_pipeline_lines, fixed = TRUE)
    rscript_idx <- rscript_idx[rscript_idx > build_phase_idx][1]
    if (is.na(rscript_idx)) {
      warning(paste("Rscript not found in buildPhase for", deriv_name))
      next
    }
    
    # Determine the indentation of the R code lines
    first_r_line_idx <- rscript_idx + 1
    first_r_line <- flat_pipeline_lines[first_r_line_idx]
    indentation <- str_extract(first_r_line, "^\\s*")
    
    # Generate readRDS lines for each dependency
    readRDS_lines <- paste0(
      indentation,
      deps,
      " <- readRDS('${",
      deps,
      "}/",
      deps,
      ".rds')"
    )
    
    # Insert the readRDS lines right after Rscript -e "
    flat_pipeline_lines <- append(flat_pipeline_lines, readRDS_lines, after = rscript_idx)
  }
  
  # Step 4: Write the updated lines back to pipeline.nix
  writeLines(flat_pipeline_lines, nix_file)
  cat("Updated", nix_file, "with readRDS calls based on", dag_file, "\n")
}

# Example usage
# gen_pipeline("dag.json", "flat_pipeline.nix")
