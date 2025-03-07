#' Generate a DAG from a list of derivations
#'
#' Creates a JSON representation of a directed acyclic graph (DAG) based on dependencies
#' between derivations.
#'
#' @param deriv_list A list of derivations, each with a `name` and `snippet`, output of mk_r().
#' @param output_file Path to the output JSON file. Defaults to "_rixpress/dag.json".
#' @importFrom jsonlite write_json
#' @return Writes a JSON file representing the DAG.
#' @export
generate_dag <- function(deriv_list, output_file = "_rixpress/dag.json") {
  dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)

  # Number of derivations
  n <- length(deriv_list)

  # Initialize DAG and tracking of defined names
  dag <- vector("list", n)
  defined <- character(n)

  # Process each derivation
  for (i in seq_along(deriv_list)) {
    d <- deriv_list[[i]]
    name <- d$name

    # Extract expression from snippet
    expr <- gsub(".*<-\\s*([^\\n]+).*", "\\1", d$snippet)

    # Identify dependencies
    deps <- intersect(all.names(parse(text = expr)), defined[1:(i - 1)])

    # Store in DAG
    dag[[i]] <- list(deriv_name = name, depends = deps)
    defined[i] <- name
  }

  # Write DAG to JSON
  jsonlite::write_json(list(derivations = dag), output_file, pretty = TRUE)

  # Return path only if testing
  if (identical(Sys.getenv("TESTTHAT"), "true")) {
    output_file
  }
}
