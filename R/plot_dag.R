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
#' @importFrom graphics symbols
#' @importFrom stats setNames
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

  shape_mapping <- c(
    "rxp_py" = "triangle",
    "rxp_py2r" = "triangle",
    "rxp_r" = "circle",
    "rxp_r2py" = "circle",
    "rxp_quarto" = "square"
  )
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

  igraph::add_shape(
    "triangle",
    clip = igraph::shape_noclip,
    plot = myinverted_triangle,
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

#' @title Create a Triangle Shape
#' @param coords A two-column matrix or data frame with x and y coordinates.
#' @param v Logical vector or index used to subset parameters (if applicable).
#' @param params A function used to retrieve plotting parameters.
#' @importFrom graphics polygon
#' @return Invisibly returns the polygon objects created.
#' @noRd
myinverted_triangle <- function(coords, v = NULL, params) {
  # Retrieve vertex color and size
  vertex.color <- params("vertex", "color")
  if (length(vertex.color) != 1 && !is.null(v)) {
    vertex.color <- vertex.color[v]
  }
  # Scale the vertex size; we enlarge the triangle by a factor of 2 here.
  vertex.size <- 1 / 200 * params("vertex", "size")
  if (length(vertex.size) != 1 && !is.null(v)) {
    vertex.size <- vertex.size[v]
  }

  # Increase the size by a factor, here we double the side length
  side <- vertex.size * 2
  height <- sqrt(3) / 2 * side # height of an equilateral triangle

  mapply(
    coords[, 1],
    coords[, 2],
    vertex.color,
    FUN = function(x, y, col) {
      # Compute the vertices for an inverted triangle:
      xs <- c(x - side / 2, x + side / 2, x)
      ys <- c(y - height / 3, y - height / 3, y + 2 * height / 3)
      polygon(xs, ys, col = col, border = "black")
    }
  )
}

#' @title Export DAG of pipeline and prepare it for rendering on CI
#' @description This function is called automatically by rxp_ga(), but
#'   you can also run it yourself if you want to visualize the DAG on CI
#'   with your own workflow definition.
#' @return Nothing, writes `dag.dot` in `_rixpress/`.
#' @importFrom igraph  V set_vertex_attr delete_vertex_attr
#' @examples \dontrun{dag_for_ga()}
#' @export
dag_for_ci <- function() {
  dag_obj <- plot_dag(return_igraph = TRUE)

  dag_obj <- igraph::set_vertex_attr(
    dag_obj,
    "label",
    value = igraph::V(dag_obj)$name
  )

  # Step 2: Delete the "name" attribute
  dag_obj <- igraph::delete_vertex_attr(dag_obj, "name")

  igraph::write_graph(dag_obj, file = "_rixpress/dag.dot", format = "dot")
}
