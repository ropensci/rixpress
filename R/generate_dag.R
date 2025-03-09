#' Generate a DAG from a list of derivations
#'
#' Creates a JSON representation of a directed acyclic graph (DAG) based on dependencies
#' between derivations.
#'
#' @param drv_list A list of derivations, each with a `name` and `snippet`, output of drv_r().
#' @param output_file Path to the output JSON file. Defaults to "_rixpress/dag.json".
#' @importFrom jsonlite write_json
#' @return Writes a JSON file representing the DAG.
#' @export
generate_dag <- function(drv_list, output_file = "_rixpress/dag.json") {
  dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)

  n <- length(drv_list)
  dag <- vector("list", n)
  defined <- character(n)

  for (i in seq_along(drv_list)) {
    d <- drv_list[[i]]
    name <- d$name
    snippet <- d$snippet

    # Extract the content inside the Rscript -e quotes (allowing for multiple lines)
    m <- regexec('Rscript -e \\"([\\s\\S]*?)\\"', snippet, perl = TRUE)
    match <- regmatches(snippet, m)
    block <- if (length(match[[1]]) > 1) match[[1]][2] else ""

    # Split the block into lines and find the first assignment line
    lines <- unlist(strsplit(block, "\n"))
    assignment_lines <- grep("<-", lines, value = TRUE)
    expr <- if (length(assignment_lines) > 0) trimws(assignment_lines[1]) else
      ""

    # Identify dependencies by finding all names in the expression that match previously defined derivations
    deps <- intersect(all.names(parse(text = expr)), defined[1:(i - 1)])

    dag[[i]] <- list(deriv_name = name, depends = deps)
    defined[i] <- name
  }

  jsonlite::write_json(list(derivations = dag), output_file, pretty = TRUE)

  if (identical(Sys.getenv("TESTTHAT"), "true")) {
    output_file
  }
}
