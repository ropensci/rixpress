#' @title Create a Directed Acyclic Graph (DAG) representing the pipeline
#' @description Reads JSON derivation data, extracts nodes and edges, and plots a DAG using igraph.
#' @param json_path Character string specifying the JSON file path.
#' @details Expects JSON with `deriv_name` and `depends` fields per derivation. Plots the DAG with default styling.
#' @return None. Plots the DAG directly.
#' @importFrom jsonlite read_json
#' @importFrom igraph graph_from_data_frame
#' @examples \dontrun{create_dag()}
create_dag <- function(json_path = "_rixpress/dag.json") {
  data <- jsonlite::read_json(json_path)

  nodes <- unique(unlist(lapply(data$derivations, function(x) x$deriv_name)))
  edges <- do.call(
    rbind,
    lapply(data$derivations, function(x) {
      if (length(x$depends) > 0) {
        data.frame(
          from = x$depends,
          to = x$deriv_name,
          stringsAsFactors = FALSE
        )
      } else {
        NULL
      }
    })
  )

  g <- igraph::graph_from_data_frame(
    edges,
    vertices = data.frame(name = nodes),
    directed = TRUE
  )

  plot(
    g,
    vertex.color = "lightblue",
    vertex.size = 15,
    edge.arrow.size = 0.5,
    vertex.label.color = "black"
  )
}
