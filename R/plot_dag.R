create_dag <- function(json_path) {
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

  igraph::graph_from_data_frame(
    edges,
    vertices = data.frame(name = nodes),
    directed = TRUE
  )
}

#g <- create_dag("_rixpress/dag.json")
#plot(g, vertex.color = "lightblue", vertex.size = 15, edge.arrow.size = 0.5, vertex.label.color = "black")
