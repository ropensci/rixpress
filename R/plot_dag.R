#' @title Create a Directed Acyclic Graph (DAG) representing the pipeline
#' @description Reads the json of the DAG under `_rixpress/dag.json`, and
#'   uses `{igraph}` to generate the plot
#' @param json_path Character string specifying the JSON file path.
#' @param return_igraph Logical, defaults to FALSE. Should the underlying
#'   igraph object be returned? This can be useful to further process it
#'   or plot it using other tools.
#' @return None by default, only prints the plot. If `return_igraph` is set
#'   to `TRUE`, returns the underlying igraph object.
#' @importFrom jsonlite read_json
#' @importFrom igraph graph_from_data_frame V shape_noclip set_vertex_attr
#' @examples \dontrun{plot_dag()}
#' @export
plot_dag <- function(json_path = "_rixpress/dag.json", return_igraph = FALSE) {
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

  df_data$depends <- ifelse(df_data$depends == "", "root", df_data$depends)

  g <- igraph::graph_from_data_frame(
    df_data,
    directed = TRUE,
  )

  shape_mapping <- c("rxp_r" = "circle", "rxp_quarto" = "square")
  df_data$shape <- shape_mapping[df_data$type]
  shapes_df <- unique(df_data[, c("deriv_name", "shape")])
  # Always add root vertex
  shapes_df <- rbind(
    shapes_df,
    data.frame("deriv_name" = "root", "shape" = "star")
  )
  shapes_vector <- shapes_df$shape
  shapes_vector <- setNames(shapes_vector, shapes_df$deriv_name)

  igraph::add_shape(
    "star",
    clip = igraph::shape_noclip,
    plot = mystar,
    parameters = list(vertex.norays = 5)
  )

  g <- igraph::set_vertex_attr(
    g,
    "shape",
    value = shapes_vector[igraph::V(g)$name]
  )

  if (!return_igraph) {
    plot(
      g,
      vertex.color = "lightblue",
      vertex.size = 15,
      edge.arrow.size = 0.5,
      vertex.label.color = "black"
    )
  }

  if (return_igraph) {
    g
  }
}


#' @title Create a star shape
#' @description Source: https://igraph.org/r/doc/shapes.html
#' @param coords Coordinates
#' @param v Logical.
#' @param params List of parameters.
#' @return A star I guess
#' @noRd
mystar <- function(coords, v = NULL, params) {
  vertex.color <- params("vertex", "color")
  if (length(vertex.color) != 1 && !is.null(v)) {
    vertex.color <- vertex.color[v]
  }
  vertex.size <- 1 / 200 * params("vertex", "size")
  if (length(vertex.size) != 1 && !is.null(v)) {
    vertex.size <- vertex.size[v]
  }
  norays <- params("vertex", "norays")
  if (length(norays) != 1 && !is.null(v)) {
    norays <- norays[v]
  }

  mapply(
    coords[, 1],
    coords[, 2],
    vertex.color,
    vertex.size,
    norays,
    FUN = function(x, y, bg, size, nor) {
      symbols(
        x = x,
        y = y,
        bg = bg,
        stars = matrix(c(size, size / 2), nrow = 1, ncol = nor * 2),
        add = TRUE,
        inches = FALSE
      )
    }
  )
}
