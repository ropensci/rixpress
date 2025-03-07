library(jsonlite)

generate_dag <- function(deriv_list, output_file = "dag.json") {
  dag <- list()  # Initialize an empty list for derivation objects
  defined <- c()  # Track defined names
  
  for (d in deriv_list) {
    name <- d$name
    expr <- gsub(".*<-\\s*([^\\n]+).*", "\\1", d$snippet)  # Grab the expression
    deps <- intersect(all.names(parse(text = expr)), defined)
    # Append a list representing the derivation object to dag
    dag <- c(dag, list(list(deriv_name = name, depends = deps)))
    defined <- c(defined, name)
  }
  
  # Wrap the list of derivations in a "derivations" key
  final_dag <- list(derivations = dag)
  write_json(final_dag, output_file, pretty = TRUE)
  cat("Wrote", output_file, "\n")
}

# Example usage
d1 <- list(name = "mtcars_am", snippet = "mtcars_am <- filter(mtcars, am == 1)\nsaveRDS(mtcars_am, 'mtcars_am.rds')")
d2 <- list(name = "mtcars_head", snippet = "mtcars_head <- head(mtcars_am)\nsaveRDS(mtcars_head, 'mtcars_head.rds')")
generate_dag(list(d1, d2))
