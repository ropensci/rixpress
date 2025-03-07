library(jsonlite)

generate_dag <- function(deriv_list, output_file = "dag.json") {
  # Determine the number of derivations
  n <- length(deriv_list)
  
  # Pre-allocate dag as a list of length n
  dag <- vector("list", n)
  
  # Pre-allocate defined as a character vector of length n
  defined <- character(n)
  
  # Iterate over the derivations using an index
  for (i in seq_along(deriv_list)) {
    d <- deriv_list[[i]]
    name <- d$name
    
    # Extract the expression from the snippet
    expr <- gsub(".*<-\\s*([^\\n]+).*", "\\1", d$snippet)
    
    # Find dependencies by intersecting expression symbols with previously defined names
    deps <- intersect(all.names(parse(text = expr)), defined[1:(i-1)])
    
    # Assign the derivation object directly to dag[[i]]
    dag[[i]] <- list(deriv_name = name, depends = deps)
    
    # Assign the current name to defined[i]
    defined[i] <- name
  }
  
  # Wrap the list of derivations in a "derivations" key
  final_dag <- list(derivations = dag)
  
  # Write to JSON file
  write_json(final_dag, output_file, pretty = TRUE)
  cat("Wrote", output_file, "\n")
}

# Example usage
d1 <- list(name = "mtcars_am", snippet = "mtcars_am <- filter(mtcars, am == 1)\nsaveRDS(mtcars_am, 'mtcars_am.rds')")
d2 <- list(name = "mtcars_head", snippet = "mtcars_head <- head(mtcars_am)\nsaveRDS(mtcars_head, 'mtcars_head.rds')")
generate_dag(list(d1, d2))
