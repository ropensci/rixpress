#' Trace Lineage of Derivations
#'
#' @family utilities
#' @param name Character, defaults to NULL. Name of the derivation to inspect.
#'   If NULL, the function prints the whole pipeline (inverted global view).
#' @param dag_file Character, defaults to "_rixpress/dag.json". Path to dag.json.
#' @param transitive Logical, defaults to TRUE. If TRUE, show transitive closure and
#'   mark transitive-only nodes with "*". If FALSE, show immediate neighbours only.
#' @param include_self Logical, defaults to FALSE. If TRUE, include `name` itself in the results.
#' @importFrom utils head
#' @return Invisibly, a named list mapping each inspected derivation name to a
#'   list with elements:
#'     - dependencies
#'     - reverse_dependencies
#'   The function also prints a tree representation to the console.
#' @export
rxp_trace <- function(
  name = NULL,
  dag_file = file.path("_rixpress", "dag.json"),
  transitive = TRUE,
  include_self = FALSE
) {
  if (!file.exists(dag_file)) {
    stop(
      "Could not find dag file at: ",
      dag_file,
      ". By default rxp_trace expects '_rixpress/dag.json'. ",
      "If your dag.json is elsewhere, pass dag_file explicitly."
    )
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop(
      "Package 'jsonlite' is required to read dag.json. Install it and retry."
    )
  }

  load_dag <- function(path) {
    dag <- jsonlite::fromJSON(path, simplifyVector = FALSE)
    if (
      !is.list(dag) || is.null(dag$derivations) || length(dag$derivations) == 0
    ) {
      stop("Invalid dag.json: no derivations found.")
    }
    dag$derivations
  }
  derivs <- load_dag(dag_file)

  extract_name <- function(d) {
    dn <- d$deriv_name
    if (is.null(dn)) {
      return(NA_character_)
    }
    if (is.list(dn) || length(dn) > 1) {
      for (el in dn) {
        if (!is.null(el) && nzchar(as.character(el))) return(as.character(el))
      }
      return(NA_character_)
    }
    as.character(dn)
  }

  extract_color <- function(d) {
    pc <- d$pipeline_color
    if (is.null(pc) || length(pc) == 0 || is.na(pc)) {
      return(NA_character_)
    }
    # Handle list wrapping from jsonlite
    if (is.list(pc)) pc <- unlist(pc)
    as.character(pc)
  }

  all_names <- vapply(derivs, extract_name, character(1))
  all_colors <- vapply(derivs, extract_color, character(1))
  names(all_colors) <- all_names

  if (anyNA(all_names)) {
    stop("Found derivations with missing or unparsable names in dag.json.")
  }

  if (!is.null(name) && !name %in% all_names) {
    stop(
      "Derivation '",
      name,
      "' not found in dag.json (available: ",
      paste(head(all_names, 20), collapse = ", "),
      if (length(all_names) > 20) ", ..." else "",
      ")."
    )
  }

  # Helper to colorize text
  colorize_name <- function(nm) {
    col <- all_colors[nm]
    if (!is.na(col) && requireNamespace("cli", quietly = TRUE)) {
      tryCatch(
        cli::make_ansi_style(col)(nm),
        error = function(e) nm
      )
    } else {
      nm
    }
  }

  make_depends_map <- function(derivs, names) {
    out <- setNames(vector("list", length(names)), names)
    for (i in seq_along(derivs)) {
      deps <- derivs[[i]]$depends
      if (is.null(deps)) {
        deps <- character(0)
      } else {
        deps <- as.character(unlist(deps, use.names = FALSE))
        deps <- deps[nzchar(deps)]
      }
      # only keep DAG-internal deps, and drop self-loops
      deps <- deps[deps %in% names & deps != names[i]]
      out[[names[i]]] <- unique(deps)
    }
    out
  }

  depends_map <- make_depends_map(derivs, all_names)
  reverse_map <- lapply(all_names, function(n) character(0))
  names(reverse_map) <- all_names
  for (src in names(depends_map)) {
    for (dep in depends_map[[src]]) {
      reverse_map[[dep]] <- unique(c(reverse_map[[dep]], src))
    }
  }

  traverse <- function(start, graph) {
    visited <- character()
    stack <- graph[[start]]
    if (is.null(stack)) {
      stack <- character()
    }
    while (length(stack)) {
      node <- stack[[1]]
      stack <- stack[-1]
      if (node %in% visited) {
        next
      }
      visited <- c(visited, node)
      nb <- graph[[node]]
      if (is.null(nb)) {
        nb <- character()
      }
      stack <- c(stack, setdiff(nb, c(visited, stack)))
    }
    visited
  }

  marked_vec <- function(target, graph, transitive) {
    imm <- graph[[target]]
    if (is.null(imm)) {
      imm <- character()
    }
    if (!transitive) {
      return(unique(imm))
    }
    full <- traverse(target, graph)
    trans_only <- setdiff(full, imm)
    c(imm, paste0(trans_only, "*"))
  }

  print_single <- function(target) {
    cat("==== Lineage for:", colorize_name(target), "====\n")

    cat("Dependencies (ancestors):\n")
    visited <- character(0)
    rec_dep <- function(node, depth) {
      parents <- depends_map[[node]]
      if (is.null(parents) || length(parents) == 0) {
        if (depth == 0) {
          cat("  - <none>\n")
        }
        return()
      }
      for (p in parents) {
        cat(strrep("  ", depth + 1), "- ", colorize_name(p), sep = "")
        if (transitive && depth >= 1) cat("*")
        cat("\n")
        if (!(p %in% visited)) {
          visited <<- c(visited, p)
          rec_dep(p, depth + 1)
        }
      }
    }
    rec_dep(target, 0)

    cat("\nReverse dependencies (children):\n")
    visited <- character(0)
    rec_rev <- function(node, depth) {
      kids <- reverse_map[[node]]
      if (is.null(kids) || length(kids) == 0) {
        if (depth == 0) {
          cat("  - <none>\n")
        }
        return()
      }
      for (k in kids) {
        cat(strrep("  ", depth + 1), "- ", colorize_name(k), sep = "")
        if (transitive && depth >= 1) cat("*")
        cat("\n")
        if (!(k %in% visited)) {
          visited <<- c(visited, k)
          rec_rev(k, depth + 1)
        }
      }
    }
    rec_rev(target, 0)

    if (transitive) {
      cat("\nNote: '*' marks transitive dependencies (depth >= 2).\n\n")
    }
    invisible(NULL)
  }

  print_forest_once <- function(roots, graph, transitive) {
    visited <- character(0)
    rec <- function(node, depth) {
      cat(strrep("  ", depth), "- ", colorize_name(node), sep = "")
      if (transitive && depth >= 2) cat("*")
      cat("\n")

      if (node %in% visited) {
        return()
      }
      visited <<- c(visited, node)
      kids <- graph[[node]]
      if (is.null(kids) || length(kids) == 0) {
        return()
      }
      for (k in kids) {
        rec(k, depth + 1)
      }
    }
    for (r in roots) {
      rec(r, 0)
    }
    invisible(NULL)
  }

  sinks <- function() {
    no_children <- names(Filter(function(x) length(x) == 0, reverse_map))
    if (length(no_children)) {
      return(no_children)
    }
    outdeg <- vapply(reverse_map, length, integer(1))
    names(outdeg)[outdeg == min(outdeg)]
  }

  results <- setNames(
    lapply(all_names, function(nm) {
      deps <- marked_vec(nm, depends_map, transitive)
      rdeps <- marked_vec(nm, reverse_map, transitive)
      if (include_self) {
        deps <- unique(c(nm, deps))
        rdeps <- unique(c(nm, rdeps))
      }
      list(dependencies = deps, reverse_dependencies = rdeps)
    }),
    all_names
  )

  if (is.null(name)) {
    cat(
      "==== Pipeline dependency tree (outputs \u2192 inputs) ====\n"
    )
    for (r in sinks()) {
      print_forest_once(r, depends_map, transitive)
    }
    if (transitive) {
      cat("\nNote: '*' marks transitive dependencies (depth >= 2).\n\n")
    }
    invisible(results)
  } else {
    print_single(name)
    invisible(results[name])
  }
}
