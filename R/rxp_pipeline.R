#' Create a Named Pipeline of Derivations
#'
#' Groups multiple derivations into a named pipeline for organizational
#' purposes. This allows you to structure large projects into logical
#' sub-pipelines (e.g., "ETL", "Model", "Report") that are visually
#' distinguished in DAG visualizations.
#'
#' @family pipeline functions
#' @param name Character, the name of the pipeline (e.g., "ETL", "Model").
#' @param path Character path to an R script returning a list of derivations, OR
#'   a list of derivation objects created by `rxp_r()`, `rxp_py()`, etc.
#' @param color Character, optional. A CSS color name (e.g., "darkorange") or
#'   hex code (e.g., "#FF5733") to use for this pipeline's nodes in DAG
#'   visualizations. If NULL, a default color will be assigned.
#' @param ... Additional arguments (currently unused, reserved for future use).
#'
#' @return An object of class `rxp_pipeline` containing the derivations with
#'   pipeline metadata attached.
#'
#' @details
#' The `rxp_pipeline()` function is used to organize derivations into logical
#' groups. When passed to `rxp_populate()`, the derivations are flattened but
#' retain their group and color metadata, which is then used in DAG
#' visualizations (`rxp_visnetwork()` and `rxp_ggdag()`) to distinguish
#' different parts of your workflow.
#'
#' This pattern enables a "Master Script" workflow where you can define
#' sub-pipelines in separate R scripts that each return a list of derivations.
#' You then pass the paths to these scripts to `rxp_pipeline()`:
#'
#' @examples
#' \dontrun{
#'   # Define derivations in separate scripts
#'   # pipelines/01_etl.R returns: list(rxp_r(...), rxp_r(...))
#'   # pipelines/02_model.R returns: list(rxp_r(...), rxp_r(...))
#'
#'   # Master script (run.R):
#'
#'   # Create named pipelines with colors by pointing to the files
#'   pipe_etl <- rxp_pipeline("ETL", "pipelines/01_etl.R", color = "darkorange")
#'   pipe_model <- rxp_pipeline("Model", "pipelines/02_model.R", color = "dodgerblue")
#'
#'   # Build the combined pipeline
#'   rxp_populate(list(pipe_etl, pipe_model))
#'   rxp_make()
#'
#'   # Visualize - ETL nodes will be orange, Model nodes will be blue
#'   rxp_visnetwork()
#' }
#'
#' @export
rxp_pipeline <- function(name, path, color = NULL, ...) {
  # Validate inputs

  if (!is.character(name) || length(name) != 1 || nchar(name) == 0) {
    stop("'name' must be a non-empty character string")
  }

  derivs <- NULL

  if (is.character(path)) {
    if (length(path) != 1) {
      stop("'path' must be a single file path")
    }
    if (!file.exists(path)) {
      stop("Pipeline file not found: ", path)
    }
    # Source the file in a new environment to avoid side effects
    # and capture the return value (result of last expression)
    res <- source(path, local = new.env())
    derivs <- res$value
  } else if (inherits(path, "rxp_derivation")) {
    stop(
      "'path' must be a list of derivation objects or a file path, not a single derivation."
    )
  } else if (is.list(path)) {
    # Accept a direct list for testing or specialized use cases
    derivs <- path
  } else {
    stop("'path' must be a file path (character) or a list of derivations")
  }

  if (!is.list(derivs)) {
    stop(
      "The pipeline script must return a list of derivations, but returned: ",
      class(derivs)[1]
    )
  }

  # Validate that all elements are derivation objects
  for (i in seq_along(derivs)) {
    if (!inherits(derivs[[i]], "rxp_derivation")) {
      stop(
        "Element ",
        i,
        " of pipeline derivations is not an rxp_derivation object. ",
        "All elements must be created by rxp_r(), rxp_py(), etc."
      )
    }
  }

  # Validate color if provided
  if (!is.null(color)) {
    if (!is.character(color) || length(color) != 1) {
      stop(
        "'color' must be a single character string (CSS color name or hex code)"
      )
    }
  }

  # Attach metadata to each derivation
  derivs <- lapply(derivs, function(deriv) {
    deriv$pipeline_group <- name
    deriv$pipeline_color <- color
    deriv
  })

  # Create the pipeline object
  structure(
    list(
      name = name,
      color = color,
      derivs = derivs
    ),
    class = c("rxp_pipeline", "list")
  )
}


#' Flatten a list of derivations and pipelines
#'
#' Takes a potentially nested list containing both `rxp_derivation` objects
#' and `rxp_pipeline` objects and returns a flat list of derivations with
#' metadata preserved.
#'
#' @param derivs A list that may contain `rxp_derivation` objects,
#'   `rxp_pipeline` objects, or a mix of both.
#' @return A flat list of `rxp_derivation` objects.
#' @keywords internal
flatten_derivations <- function(derivs) {
  result <- list()

  for (item in derivs) {
    if (inherits(item, "rxp_pipeline")) {
      # It's a pipeline - extract its derivations (already have metadata attached)
      result <- c(result, item$derivs)
    } else if (inherits(item, "rxp_derivation")) {
      # It's a plain derivation - add default metadata if missing
      if (is.null(item$pipeline_group)) {
        item$pipeline_group <- "default"
      }
      if (is.null(item$pipeline_color)) {
        item$pipeline_color <- NULL # Will be assigned a default in visualization
      }
      result <- c(result, list(item))
    } else if (is.list(item)) {
      # Recursively flatten nested lists
      result <- c(result, flatten_derivations(item))
    } else {
      stop(
        "Invalid element in derivations list. Expected rxp_derivation or rxp_pipeline, got: ",
        class(item)[1]
      )
    }
  }

  result
}


#' Print Method for rxp_pipeline Objects
#'
#' @param x An object of class "rxp_pipeline"
#' @param ... Additional arguments passed to print methods
#' @return Nothing, prints a summary of the pipeline object to the console.
#' @export
print.rxp_pipeline <- function(x, ...) {
  cat("rixpress pipeline:", x$name, "\n")
  if (!is.null(x$color)) {
    cat("  Color:", x$color, "\n")
  }
  cat("  Derivations:", length(x$derivs), "\n")
  if (length(x$derivs) > 0) {
    deriv_names <- sapply(x$derivs, function(d) d$name)
    cat("    -", paste(deriv_names, collapse = "\n    - "), "\n")
  }
  invisible(x)
}
