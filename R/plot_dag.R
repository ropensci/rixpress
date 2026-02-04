# Suppress R CMD check notes for non-standard evaluation
utils::globalVariables("color_group")

#' @title Export DAG of Pipeline and Prepare It for Rendering on CI

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

#' @title Prepare Data for Plotting the DAG of the Pipeline
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

  # Extract node information including pipeline metadata
  n <- derivations[, c("deriv_name", "type")]
  nn <- unnest_all_columns(n)

  # Get the number of rows (after unnesting)
  n_rows <- nrow(nn)

  # Extract pipeline_group (default to "default" if not present or null)
  if (
    "pipeline_group" %in%
      names(derivations) &&
      !is.null(derivations$pipeline_group)
  ) {
    pipeline_group <- vapply(
      derivations$pipeline_group,
      function(x) {
        if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) {
          "default"
        } else {
          as.character(x)
        }
      },
      character(1)
    )
  } else {
    pipeline_group <- rep("default", n_rows)
  }

  # Extract pipeline_color (can be NULL)
  if (
    "pipeline_color" %in%
      names(derivations) &&
      !is.null(derivations$pipeline_color)
  ) {
    pipeline_color <- vapply(
      derivations$pipeline_color,
      function(x) {
        if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) {
          NA_character_
        } else {
          as.character(x)
        }
      },
      character(1)
    )
  } else {
    pipeline_color <- rep(NA_character_, n_rows)
  }

  # Ensure lengths match (after unnesting, there might be differences)
  if (length(pipeline_group) != n_rows) {
    pipeline_group <- rep("default", n_rows)
  }
  if (length(pipeline_color) != n_rows) {
    pipeline_color <- rep(NA_character_, n_rows)
  }

  nodes_df <- data.frame(
    "id" = nn$deriv_name,
    "label" = nn$deriv_name,
    "group" = nn$type,
    "pipeline_group" = pipeline_group,
    "pipeline_color" = pipeline_color,
    stringsAsFactors = FALSE
  )
  nodes <- unique(nodes_df)

  e <- derivations[, c("deriv_name", "depends")]
  ee <- unnest_all_columns(e)
  edges <- data.frame(
    "from" = ee$depends,
    "to" = ee$deriv_name,
    "arrows" = "to",
    stringsAsFactors = FALSE
  )

  list(
    "nodes" = nodes,
    "edges" = edges
  )
}


#' @title Create a Directed Acyclic Graph (DAG) Representing the Pipeline
#'   Using `{ggplot2}`
#' @family visualisation functions
#' @description Uses `{ggdag}` to generate the plot. `{ggdag}` is a soft
#'   dependency of `{rixpress}` so you need to install it to use this
#'   function. When derivations are organized into pipelines using
#'   `rxp_pipeline()`, nodes use a dual-encoding approach: the interior fill
#'   shows the derivation type (R, Python, etc.) while a thick border shows
#'   the pipeline group colour.
#' @param nodes_and_edges List, output of `get_nodes_edges()`.
#' @param color_by Character, either "pipeline" (default) or "type".
#'   When "pipeline", nodes show type as fill colour and pipeline as border.
#'   When "type", nodes are coloured entirely by derivation type (rxp_r, rxp_py, etc.).
#' @param colour_by Character, alias for `color_by`.
#' @return A `{ggplot2}` object.
#' @examples \dontrun{
#'   rxp_ggdag()  # Dual encoding: fill = type, border = pipeline
#'   rxp_ggdag(colour_by = "type")  # Color entirely by derivation type
#' }
#' @export
rxp_ggdag <- function(
  nodes_and_edges = get_nodes_edges(),
  color_by = c("pipeline", "type"),
  colour_by = NULL
) {
  if (!requireNamespace("ggdag", quietly = TRUE)) {
    stop("You need to install {ggdag} to use this feature.")
  }

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("You need to install {ggplot2} to use this feature.")
  }

  if (!is.null(colour_by)) {
    color_by <- colour_by
  }

  color_by <- match.arg(color_by)

  nodes <- nodes_and_edges$nodes
  nodes$name <- nodes$id

  # Default color palette for pipeline groups
  default_pipeline_colors <- c(
    "#E69F00",
    "#56B4E9",
    "#009E73",
    "#F0E442",
    "#0072B2",
    "#D55E00",
    "#CC79A7",
    "#999999"
  )

  # Check if we have any non-default pipeline groups
  has_pipelines <- "pipeline_group" %in%
    names(nodes) &&
    any(nodes$pipeline_group != "default")

  if (color_by == "pipeline" && has_pipelines) {
    # Color by pipeline group
    unique_groups <- unique(nodes$pipeline_group)

    # Create a color mapping for groups
    group_colors <- c()
    color_idx <- 1
    for (grp in unique_groups) {
      # Get the first color specified for this group (if any)
      grp_nodes <- nodes[nodes$pipeline_group == grp, ]
      explicit_color <- grp_nodes$pipeline_color[
        !is.na(grp_nodes$pipeline_color)
      ][1]

      if (!is.na(explicit_color)) {
        group_colors[grp] <- explicit_color
      } else {
        # Assign a default color
        group_colors[grp] <- default_pipeline_colors[
          ((color_idx - 1) %% length(default_pipeline_colors)) + 1
        ]
        color_idx <- color_idx + 1
      }
    }

    # Use pipeline_group for coloring
    nodes$color_group <- nodes$pipeline_group
    nodes <- nodes[, c("name", "color_group", "group")] # Keep type for shape

    edges <- nodes_and_edges$edges
    edges$name <- edges$from
    edges <- edges[, c("name", "to")]

    dag_df <- ggdag::as_tidy_dagitty(edges) |>
      merge(nodes, by = "name")

    # Derivation type colors for node fill (interior)
    type_colors <- c(
      "rxp_r" = "#246ABF",
      "rxp_r2py" = "#FFD343",
      "rxp_py" = "#FFD343",
      "rxp_py2r" = "#246ABF",
      "rxp_jl" = "#9558b2",
      "rxp_qmd" = "#4F789E",
      "rxp_rmd" = "#4F789E"
    )

    # Scale for node fill (type)
    rxp_fill_scale <- ggplot2::scale_fill_manual(
      values = type_colors,
      name = "Type"
    )

    # Scale for node border/stroke (pipeline)
    rxp_colour_scale <- ggplot2::scale_colour_manual(
      values = group_colors,
      name = "Pipeline"
    )

    # Use derivation type for shapes
    rxp_shapes <- ggplot2::scale_shape_manual(
      values = c(
        "rxp_r" = 23,
        "rxp_r2py" = 23,
        "rxp_py" = 23,
        "rxp_py2r" = 23,
        "rxp_jl" = 23,
        "rxp_qmd" = 22,
        "rxp_rmd" = 22
      ),
      name = "Type"
    )

    p <- ggplot2::ggplot(
      dag_df,
      ggplot2::aes(x = x, y = y, xend = xend, yend = yend)
    ) +
      ggdag::geom_dag_edges() +
      rxp_fill_scale +
      rxp_colour_scale +
      rxp_shapes +
      # Nodes: fill = type color, colour (border) = pipeline color, thick stroke
      ggdag::geom_dag_node(
        ggplot2::aes(fill = group, colour = color_group, shape = group),
        size = 20,
        stroke = 2.5
      ) +
      ggdag::geom_dag_text(
        ggplot2::aes(label = name),
        col = "black",
        nudge_y = -.3
      ) +
      ggdag::theme_dag()

    p
  } else {
    # Color by derivation type (original behavior)
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
        "rxp_qmd" = "#4F789E",
        "rxp_rmd" = "#4F789E"
      ),
      name = "Type"
    )

    rxp_shapes <- ggplot2::scale_shape_manual(
      values = c(
        "rxp_r" = 23,
        "rxp_r2py" = 23,
        "rxp_py" = 23,
        "rxp_py2r" = 23,
        "rxp_jl" = 23,
        "rxp_qmd" = 22,
        "rxp_rmd" = 22
      ),
      name = "Type"
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
}

#' @title Create a Directed Acyclic Graph (DAG) Representing the Pipeline
#'   Using `{visNetwork}`
#' @family visualisation functions
#' @description Uses `{visNetwork}` to generate the plot. `{visNetwork}` is a
#'   soft dependency of `{rixpress}` so you need to install it to use this
#'   function. When derivations are organized into pipelines using
#'   `rxp_pipeline()`, nodes use a dual-encoding approach: the interior fill
#'   shows the derivation type (R, Python, etc.) while the border shows
#'   the pipeline group colour.
#' @param nodes_and_edges List, output of `get_nodes_edges()`.
#' @param color_by Character, either "pipeline" (default) or "type".
#'   When "pipeline", nodes show type as fill colour and pipeline as border.
#'   When "type", nodes are colored by their derivation type (rxp_r, rxp_py, etc.).
#' @param colour_by Character, alias for `color_by`.
#' @return Nothing, this function opens a new tab in your browser with
#'   the DAG generated using `{visNetwork}`.
#' @examples \dontrun{
#'   rxp_visnetwork()
#'   rxp_visnetwork(colour_by = "type")  # Color by derivation type instead
#' }
#' @export
#' @importFrom jsonlite fromJSON
rxp_visnetwork <- function(
  nodes_and_edges = get_nodes_edges(),
  color_by = c("pipeline", "type"),
  colour_by = NULL
) {
  if (!requireNamespace("visNetwork", quietly = TRUE)) {
    stop("You need to install {visNetwork} to use this feature.")
  }

  if (!is.null(colour_by)) {
    color_by <- colour_by
  }

  color_by <- match.arg(color_by)

  nodes <- nodes_and_edges$nodes
  edges <- nodes_and_edges$edges

  # Default color palette for pipeline groups (used when no color specified)
  default_pipeline_colors <- c(
    "#E69F00",
    "#56B4E9",
    "#009E73",
    "#F0E442",
    "#0072B2",
    "#D55E00",
    "#CC79A7",
    "#999999"
  )

  # Derivation type colors (original colors)
  type_colors <- c(
    "rxp_r" = "#246ABF",
    "rxp_jl" = "#9558b2",
    "rxp_r2py" = "#FFD343",
    "rxp_py" = "#FFD343",
    "rxp_py2r" = "#246ABF",
    "rxp_qmd" = "#4F789E",
    "rxp_rmd" = "#4F789E"
  )

  # Derivation type shapes
  type_shapes <- c(
    "rxp_r" = "diamond",
    "rxp_jl" = "diamond",
    "rxp_r2py" = "triangleDown",
    "rxp_py" = "diamond",
    "rxp_py2r" = "triangleDown",
    "rxp_qmd" = "box",
    "rxp_rmd" = "box"
  )

  # Check if we have any non-default pipeline groups
  has_pipelines <- "pipeline_group" %in%
    names(nodes) &&
    any(nodes$pipeline_group != "default")

  if (color_by == "pipeline" && has_pipelines) {
    # Color by pipeline group
    unique_groups <- unique(nodes$pipeline_group)

    # Create a color mapping for groups without explicit colors
    group_colors <- list()
    color_idx <- 1
    for (grp in unique_groups) {
      # Get the first color specified for this group (if any)
      grp_nodes <- nodes[nodes$pipeline_group == grp, ]
      explicit_color <- grp_nodes$pipeline_color[
        !is.na(grp_nodes$pipeline_color)
      ][1]

      if (!is.na(explicit_color)) {
        group_colors[[grp]] <- explicit_color
      } else {
        # Assign a default color
        group_colors[[grp]] <- default_pipeline_colors[
          ((color_idx - 1) %% length(default_pipeline_colors)) + 1
        ]
        color_idx <- color_idx + 1
      }
    }

    # Assign colors to nodes based on their pipeline group and derivation type
    nodes$color <- lapply(
      seq_len(nrow(nodes)),
      function(i) {
        pipeline_grp <- nodes$pipeline_group[i]
        deriv_type <- nodes$group[i]

        border_col <- group_colors[[pipeline_grp]]
        # Fallback for type color if not found
        bg_col <- if (deriv_type %in% names(type_colors)) {
          type_colors[[deriv_type]]
        } else {
          "#97C2FC" # Standard visNetwork blue
        }

        list(
          background = bg_col,
          border = border_col,
          highlight = list(
            background = bg_col,
            border = border_col
          ),
          hover = list(
            background = bg_col,
            border = border_col
          )
        )
      }
    )

    # Set border width to make it visible
    nodes$borderWidth <- 3

    # Use derivation type for shape
    nodes$shape <- vapply(
      nodes$group,
      function(t) {
        if (t %in% names(type_shapes)) type_shapes[[t]] else "dot"
      },
      character(1)
    )

    # Change group to pipeline_group for legend
    nodes$group <- nodes$pipeline_group

    # Build the network with dynamic groups
    net <- visNetwork::visNetwork(nodes, edges)

    for (grp in unique_groups) {
      net <- visNetwork::visGroups(
        net,
        groupname = grp,
        color = group_colors[[grp]],
        font = list(align = "top", size = 14),
        size = 20
      )
    }

    net <- net |>
      visNetwork::visLegend(
        position = "right",
        main = "Pipeline Groups"
      ) |>
      visNetwork::visExport(
        type = "png",
        name = "export-network",
        float = "left",
        label = "Save DAG",
        background = "white",
        style = ""
      )

    return(net)
  } else {
    # Color by derivation type (original behavior)
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
      visNetwork::visGroups(
        groupname = "rxp_rmd",
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
