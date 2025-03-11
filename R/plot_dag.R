#' @title Create a Directed Acyclic Graph (DAG) representing the pipeline
#' @description Reads JSON derivation data, extracts nodes and edges, and plots a DAG using igraph.
#' @param json_path Character string specifying the JSON file path.
#' @details Expects JSON with `deriv_name` and `depends` fields per derivation. Plots the DAG with default styling.
#' @return None. Plots the DAG directly.
#' @importFrom jsonlite read_json
#' @importFrom igraph graph_from_data_frame
#' @examples \dontrun{create_dag()}
plot_dag <- function(json_path = "_rixpress/dag.json") {
  json_data <- jsonlite::read_json(json_path)

  make_df <- function(one_derivation) {
    data.frame(
      depends = if (identical(one_derivation$depends, list())) "" else
        unlist(one_derivation$depends),
      deriv_name = unlist(one_derivation$deriv_name),
      type = unlist(one_derivation$type)
    )
  }

  df_data <- do.call(
    rbind,
    lapply(json_data$derivations, make_df)
  )

  g <- igraph::graph_from_data_frame(
    df_data,
    directed = TRUE,
  )

  # Set shape attribute
  shape_mapping <- c("drv_r" = "circle", "drv_quarto" = "square")
  g <- set_vertex_attr(g, "shape", value =  shape_mapping[df_data$type])

  plot(
    g,
    vertex.color = "lightblue",
    vertex.size = 15,
    edge.arrow.size = 0.5,
    vertex.label.color = "black"
  )
}
