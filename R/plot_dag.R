#' @title Export DAG of pipeline and prepare it for rendering on CI

#' @family ci utilities
#' @description This function generates a DOT file representation of the
#'   pipeline DAG, suitable for visualization, potentially on CI platforms. It
#'   is called by `rxp_ga()`.
#' @param nodes_and_edges List, output of `get_nodes_edges()`. Defaults to
#'   calling `get_nodes_edges()`.
#' @param output_file Character, the path where the DOT file should be saved.
#'   Defaults to `"_rixpress/dag.dot"`. The directory will be created if it
#'   doesn't exist.
#' @return Nothing, writes the DOT file to the specified `output_file`.
#' @importFrom igraph graph_from_data_frame V set_vertex_attr delete_vertex_attr
#'   write_graph
#' @examples
#' \dontrun{
#'   # Generate the default _rixpress/dag.dot
#'   rxp_dag_for_ci()
#'
#' }
#' @export
rxp_dag_for_ci <- function(
  nodes_and_edges = get_nodes_edges(),
  output_file = "_rixpress/dag.dot"
) {
  edges <- nodes_and_edges$edges
  edges <- edges[, c("from", "to")]

  dag_obj <- igraph::graph_from_data_frame(
    edges,
    directed = TRUE,
  )

  dag_obj <- igraph::set_vertex_attr(
    dag_obj,
    "label",
    value = igraph::V(dag_obj)$name
  )

  dag_obj <- igraph::delete_vertex_attr(dag_obj, "name")

  igraph::write_graph(dag_obj, file = output_file, format = "dot")
}

#' @title Prepare data for plotting the DAG of the pipeline.
#' @param path_dag Character, defaults to `"_rixpress/dag.json"`.
#' @return A list of two datasets, `nodes` and `edges`.
#' @examples \dontrun{
#'   get_nodes_edges()
#' }
#' @importFrom jsonlite fromJSON
#' @noRd
get_nodes_edges <- function(path_dag = "_rixpress/dag.json") {
  # Check if the DAG file exists
  if (!file.exists(path_dag)) {
    stop("dag.json missing! Did you run 'rxp_populate()'?")
  }

  json_data <- fromJSON(path_dag)

  derivations <- json_data$derivations

  n <- derivations[, c("deriv_name", "type")]
  nn <- unnest_all_columns(n)

  nodes_df <- data.frame(
    "id" = nn$deriv_name,
    "label" = nn$deriv_name,
    "group" = nn$type
  )
  nodes <- unique(nodes_df)

  e <- derivations[, c("deriv_name", "depends")]
  ee <- unnest_all_columns(e)
  edges <- data.frame(
    "from" = ee$depends,
    "to" = ee$deriv_name,
    "arrows" = "to"
  )

  list(
    "nodes" = nodes,
    "edges" = edges
  )
}


#' @title Create a Directed Acyclic Graph (DAG) representing the pipeline
#'   using `{ggplot2}`
#' @family visualisation functions
#' @description Uses `{ggdag}` to generate the plot. `{ggdag}` is a soft
#'   dependency of `{rixpress}` so you need to install it to use this
#'   function.
#' @param nodes_and_edges List, output of `get_nodes_edges()`.
#' @return A `{ggplot2}` object.
#' @examples \dontrun{
#'   rxp_ggdag()
#' }
#' @export
rxp_ggdag <- function(nodes_and_edges = get_nodes_edges()) {
  if (!requireNamespace("ggdag", quietly = TRUE)) {
    stop("You need to install {ggdag} to use this feature.")
  }

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("You need to install {ggplot2} to use this feature.")
  }

  nodes <- nodes_and_edges$nodes
  nodes$name <- nodes$id
  nodes <- nodes[, c("name", "group")]

  edges <- nodes_and_edges$edges
  edges$name <- edges$from
  edges <- edges[, c("name", "to")]

  dag_df <- ggdag::as_tidy_dagitty(edges) |>
    merge(nodes, by = "name")

  rxp_scale <- ggplot2::scale_fill_manual(
    values = c(
      "rxp_r" = "#246ABF",
      "rxp_r2py" = "#FFD343",
      "rxp_py" = "#FFD343",
      "rxp_py2r" = "#246ABF",
      "rxp_jl" = "#9558b2",
      "rxp_qmd" = "#4F789E"
    )
  )

  rxp_shapes <- ggplot2::scale_shape_manual(
    values = c(
      "rxp_r" = 23,
      "rxp_r2py" = 23,
      "rxp_py" = 23,
      "rxp_py2r" = 23,
      "rxp_jl" = 23,
      "rxp_qmd" = 22
    )
  )

  ggplot2::ggplot(
    dag_df,
    ggplot2::aes(x = x, y = y, xend = xend, yend = yend)
  ) +
    ggdag::geom_dag_edges() +
    rxp_scale +
    rxp_shapes +
    ggdag::geom_dag_node(ggplot2::aes(fill = group, shape = group)) +
    ggdag::geom_dag_text(
      ggplot2::aes(label = name),
      col = "black",
      nudge_y = -.3
    ) +
    ggdag::theme_dag()
}

#' @title Create a Directed Acyclic Graph (DAG) representing the pipeline
#'   using `{visNetwork}`
#' @family visualisation functions
#' @description Uses `{visNetwork}` to generate the plot. `{visNetwork}` is a
#'   soft dependency of `{rixpress}` so you need to install it to use this
#'   function.
#' @param nodes_and_edges List, output of `get_nodes_edges()`.
#' @return Nothing, this function opens a new tab in your browser with
#'   the DAG generated using `{visNetwork}`.
#' @examples \dontrun{
#'   rxp_visnetwork()
#' }
#' @export
#' @importFrom jsonlite fromJSON
rxp_visnetwork <- function(nodes_and_edges = get_nodes_edges()) {
  if (!requireNamespace("visNetwork", quietly = TRUE)) {
    stop("You need to install {visNetwork} to use this feature.")
  }

  nodes <- nodes_and_edges$nodes
  edges <- nodes_and_edges$edges

  visNetwork::visNetwork(nodes, edges) |>
    visNetwork::visGroups(
      groupname = "rxp_r",
      shape = "diamond",
      color = "#246ABF",
      font = list(align = "top", size = 14),
      size = 20
    ) |>
    visNetwork::visGroups(
      groupname = "rxp_jl",
      shape = "diamond",
      color = "#9558b2",
      font = list(align = "top", size = 14),
      size = 20
    ) |>
    visNetwork::visGroups(
      groupname = "rxp_r2py",
      shape = "triangleDown",
      color = "#FFD343",
      font = list(align = "top", size = 14),
      size = 10
    ) |>
    visNetwork::visGroups(
      groupname = "rxp_py",
      shape = "diamond",
      color = "#FFD343",
      font = list(align = "top", size = 14),
      size = 20
    ) |>
    visNetwork::visGroups(
      groupname = "rxp_py2r",
      shape = "triangleDown",
      color = "#246ABF",
      font = list(align = "top", size = 14),
      size = 10
    ) |>
    visNetwork::visGroups(
      groupname = "rxp_qmd",
      shape = "box",
      color = "#4F789E",
      font = list(align = "top", size = 40)
    ) |>
    visNetwork::visLegend(
      position = "right",
      main = "Derivation Types"
    ) |>
    visNetwork::visExport(
      type = "png",
      name = "export-network",
      float = "left",
      label = "Save DAG",
      background = "white",
      style = ""
    )
}

#' @noRd
unnest_all_columns <- function(df) {
  # Identify list-columns
  list_cols <- sapply(df, is.list)

  # If no list-columns, return original data frame
  if (!any(list_cols)) {
    return(df)
  }

  # Initialize result with non-list columns
  result <- df[!list_cols]

  # Process each list-column
  for (col_name in names(df)[list_cols]) {
    col_data <- df[[col_name]]

    # Determine the type of the first non-NULL element
    first_non_null <- Filter(Negate(is.null), col_data)[[1]]

    if (is.data.frame(first_non_null)) {
      # Unnest list of data frames
      unnested <- do.call(
        rbind,
        lapply(col_data, function(x) {
          if (is.null(x)) {
            # Replace NULL with NA row
            as.data.frame(matrix(
              NA,
              nrow = 1,
              ncol = ncol(first_non_null),
              dimnames = list(NULL, names(first_non_null))
            ))
          } else {
            x
          }
        })
      )
      # Combine with result
      result <- cbind(result, unnested)
    } else if (is.atomic(first_non_null)) {
      # Unnest list of atomic vectors
      lengths <- sapply(col_data, length)
      repeated_rows <- rep(1:nrow(df), lengths)
      values <- unlist(col_data)
      result <- result[repeated_rows, , drop = FALSE]
      result[[col_name]] <- values
    } else {
      warning(paste("Column", col_name, "contains unsupported data types."))
    }
  }

  result
}
