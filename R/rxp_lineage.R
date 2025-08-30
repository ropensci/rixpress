#' Compute lineage for a derivation using _rixpress/dag.json
#'
#' rxp_lineage reads the DAG produced by rxp_populate (stored by default in
#' _rixpress/dag.json at the project root) and returns a list with dependencies
#' and reverse dependencies (derivations that depend on the target).
#'
#' By default the function expects the dag file at "_rixpress/dag.json". If your
#' project uses a different path for the dag file, provide it via the dag_file
#' argument.
#'
#' Transitive dependencies/reverse dependencies are marked with a "*" suffix so
#' they can be visually distinguished from immediate neighbors.
#'
#' @param name Character scalar. Name of the derivation to inspect.
#' @param dag_file Character scalar. Path to dag.json. Defaults to "_rixpress/dag.json".
#' @param transitive Logical, default TRUE. If TRUE, return transitive closure and
#'   mark transitive-only nodes with "*". If FALSE, returns only immediate neighbors.
#' @param include_self Logical, default FALSE. If TRUE, include `name` in results.
#' @return A list with elements:
#'   - dependencies: character vector of derivation names this derivation depends on
#'     (transitive-only entries have a trailing "*")
#'   - reverse_dependencies: character vector of derivations that depend on this derivation
#'     (transitive-only entries have a trailing "*")
#' @family utilities
#' @examples
#' \dontrun{
#' rxp_lineage("mtcars_mpg")
#' rxp_lineage("mtcars_head", transitive = FALSE)
#' rxp_lineage("page", dag_file = "/path/to/project/_rixpress/dag.json")
#' }
#' @export
rxp_lineage <- function(
  name,
  dag_file = file.path("_rixpress", "dag.json"),
  transitive = TRUE,
  include_self = FALSE
) {
  if (missing(name) || !is.character(name) || length(name) != 1) {
    stop("`name` must be a single character string with the derivation name.")
  }
  if (!is.character(dag_file) || length(dag_file) != 1) {
    stop("`dag_file` must be a single character string path to dag.json.")
  }

  if (!file.exists(dag_file)) {
    stop(
      "Could not find dag file at: ",
      dag_file,
      ". By default rxp_lineage expects '_rixpress/dag.json'. ",
      "If your dag.json is elsewhere, pass dag_file explicitly."
    )
  }

  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop(
      "Package 'jsonlite' is required to read dag.json. Install it and retry."
    )
  }

  dag <- jsonlite::fromJSON(dag_file, simplifyVector = FALSE)

  if (
    !is.list(dag) || is.null(dag$derivations) || length(dag$derivations) == 0
  ) {
    stop("Invalid dag.json: no derivations found.")
  }

  derivs <- dag$derivations

  # Helper to extract the string name from derivation$deriv_name
  extract_name <- function(d) {
    dn <- d$deriv_name
    if (is.null(dn)) {
      return(NA_character_)
    }
    # deriv_name in the JSON might be an array/list; take the first non-null element
    if (is.list(dn) || length(dn) > 1) {
      for (el in dn) {
        if (!is.null(el) && nzchar(as.character(el))) return(as.character(el))
      }
      return(NA_character_)
    }
    as.character(dn)
  }

  all_names <- vapply(derivs, extract_name, character(1))
  if (any(is.na(all_names))) {
    stop(
      "Found derivation entries with missing or unparsable names in dag.json."
    )
  }

  if (!(name %in% all_names)) {
    stop(
      "Derivation '",
      name,
      "' not found in dag.json (available: ",
      paste(head(all_names, 20), collapse = ", "),
      if (length(all_names) > 20) ", ..." else "",
      ")."
    )
  }

  # Build forward adjacency: depends_map[name] -> immediate dependencies (character vector)
  depends_map <- setNames(vector("list", length(all_names)), all_names)
  for (i in seq_along(derivs)) {
    d <- derivs[[i]]
    dn <- all_names[i]
    deps <- d$depends
    deps_vec <- character(0)
    if (!is.null(deps)) {
      # deps may be a list, vector, or empty object; coerce robustly
      if (is.list(deps)) {
        deps_vec <- unlist(deps, use.names = FALSE)
      } else {
        deps_vec <- as.character(deps)
      }
      deps_vec <- deps_vec[!is.na(deps_vec) & nzchar(deps_vec)]
    }
    # only keep names that are in the dag (ignore external dependencies)
    deps_vec <- deps_vec[deps_vec %in% all_names]
    depends_map[[dn]] <- unique(as.character(deps_vec))
  }

  # Build reverse adjacency (who depends on me)
  reverse_map <- setNames(vector("list", length(all_names)), all_names)
  for (src in names(depends_map)) {
    for (dep in depends_map[[src]]) {
      reverse_map[[dep]] <- unique(c(reverse_map[[dep]], src))
    }
  }

  # Traversal helper: BFS/DFS traversal that returns reachable nodes (excluding start)
  traverse <- function(start, graph) {
    visited <- character(0)
    stack <- graph[[start]]
    if (is.null(stack)) {
      stack <- character(0)
    }
    while (length(stack) > 0) {
      node <- stack[[1]]
      stack <- if (length(stack) > 1) stack[-1] else character(0)
      if (node %in% visited) {
        next
      }
      visited <- c(visited, node)
      nb <- graph[[node]]
      if (!is.null(nb) && length(nb) > 0) {
        to_add <- setdiff(nb, c(visited, stack))
        if (length(to_add) > 0) stack <- c(stack, to_add)
      }
    }
    unique(as.character(visited))
  }

  # Prepare dependencies result, marking transitive-only entries with "*"
  immediate_deps <- depends_map[[name]]
  if (is.null(immediate_deps)) {
    immediate_deps <- character(0)
  }

  if (!transitive) {
    deps_out <- unique(as.character(immediate_deps))
  } else {
    full_deps <- traverse(name, depends_map)
    # Ensure order: immediate first (in their existing order), then transitive-only in discovery order
    transitive_only <- setdiff(full_deps, immediate_deps)
    # preserve discovery order for transitive_only by filtering full_deps
    transitive_only <- full_deps[full_deps %in% transitive_only]
    deps_out <- c(
      unique(as.character(immediate_deps)),
      if (length(transitive_only) > 0) {
        paste0(transitive_only, "*")
      } else {
        character(0)
      }
    )
  }

  # Prepare reverse dependencies result, marking transitive-only entries with "*"
  immediate_rdeps <- reverse_map[[name]]
  if (is.null(immediate_rdeps)) {
    immediate_rdeps <- character(0)
  }

  if (!transitive) {
    rdeps_out <- unique(as.character(immediate_rdeps))
  } else {
    full_rdeps <- traverse(name, reverse_map)
    transitive_only_r <- setdiff(full_rdeps, immediate_rdeps)
    transitive_only_r <- full_rdeps[full_rdeps %in% transitive_only_r]
    rdeps_out <- c(
      unique(as.character(immediate_rdeps)),
      if (length(transitive_only_r) > 0) {
        paste0(transitive_only_r, "*")
      } else {
        character(0)
      }
    )
  }

  if (include_self) {
    deps_out <- unique(c(name, deps_out))
    rdeps_out <- unique(c(name, rdeps_out))
  }

  out <- list(dependencies = deps_out, reverse_dependencies = rdeps_out)
  class(out) <- "rxp_lineage"
  out
}

#' Print method for rxp_lineage summary
#' @param x An object of class "rxp_lineage"
#' @param ... Additional arguments passed to print methods
#' @return Nothing, prints a summary of the derivation object to the console.
#' @examples
#' \dontrun{
#' # d0 is a previously defined derivation
#'   print(rxp_lineage(d0))
#' }
#' @family utilities
#' @export
print.rxp_lineage <- function(x, ...) {
  if (!inherits(x, "rxp_lineage")) {
    NextMethod()
    return(invisible(x))
  }
  cat(
    "  dependencies (",
    length(x$dependencies),
    "): ",
    if (length(x$dependencies) > 0) {
      paste(head(x$dependencies, 20), collapse = ", ")
    } else {
      "<none>"
    },
    if (length(x$dependencies) > 20) ", ..." else "",
    "\n",
    sep = ""
  )
  cat(
    "  reverse_dependencies (",
    length(x$reverse_dependencies),
    "): ",
    if (length(x$reverse_dependencies) > 0) {
      paste(head(x$reverse_dependencies, 20), collapse = ", ")
    } else {
      "<none>"
    },
    if (length(x$reverse_dependencies) > 20) ", ..." else "",
    "\n",
    sep = ""
  )
  invisible(x)
}
