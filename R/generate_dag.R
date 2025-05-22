#' Generate a DAG from a list of derivations
#'
#' Creates a JSON representation of a directed acyclic graph (DAG)
#' based on dependencies between derivations. Is automatically called
#' by `rixpress()`.
#'
#' @family ci utilities
#' @param rxp_list A list of derivations.
#' @param output_file Path to the output JSON file.
#'   Defaults to "_rixpress/dag.json".
#' @importFrom jsonlite write_json
#' @return Nothing, writes a JSON file representing the DAG.
#' @examples
#' \dontrun{
#'   generate_dag(rxp_list)
#' }
#' @export
generate_dag <- function(rxp_list, output_file = "_rixpress/dag.json") {
  # Validate inputs
  if (!is.list(rxp_list)) {
    stop("rxp_list must be a list of derivations")
  }

  # Create output directory if it doesn't exist
  dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)

  # Extract all derivation names for dependency checking
  all_derivs_names <- sapply(rxp_list, function(x) (x$name))

  n <- length(rxp_list)
  dag <- vector("list", n)
  defined <- character(n)

  # Process each derivation
  for (i in seq_along(rxp_list)) {
    deriv <- rxp_list[[i]]
    name <- deriv$name
    type <- deriv$type
    unserialize_function <- deriv$unserialize_function

    # Extract dependencies based on derivation type
    deps <- extract_dependencies(
      deriv,
      type,
      name,
      all_derivs_names,
      defined[1:(i - 1)]
    )
    # Add the derivation to the DAG
    dag[[i]] <- list(
      deriv_name = name,
      depends = deps,
      unserialize_function = unserialize_function,
      type = type
    )
    defined[i] <- name
  }

  # Write the DAG to a JSON file
  jsonlite::write_json(list(derivations = dag), output_file, pretty = TRUE)

  # Return the output file path when running tests
  if (identical(Sys.getenv("TESTTHAT"), "true")) {
    output_file
  }
}

#' Extract dependencies for a derivation
#'
#' @param deriv The derivation object
#' @param type The type of derivation
#' @param name The name of the derivation
#' @param all_derivs_names All derivation names
#' @param defined_derivs Previously defined derivations
#' @return A character vector of dependency names
extract_dependencies <- function(
  deriv,
  type,
  name,
  all_derivs_names,
  defined_derivs
) {
  if (type %in% c("rxp_r", "rxp_py2r", "rxp_r2py")) {
    extract_r_dependencies(deriv, name, all_derivs_names)
  } else if (type %in% c("rxp_qmd", "rxp_rmd")) {
    extract_markdown_dependencies(deriv, type, name, defined_derivs)
  } else if (type %in% c("rxp_py", "rxp_r2py")) {
    extract_python_dependencies(deriv, name, all_derivs_names)
  } else if (type == "rxp_jl") {
    extract_julia_dependencies(deriv, name, all_derivs_names)
  } else {
    stop("Unknown derivation type: ", type)
  }
}

#' Extract dependencies from R script derivations
#'
#' @param deriv The derivation object
#' @param name The name of the derivation
#' @param all_derivs_names All derivation names
#' @return A character vector of dependency names
extract_r_dependencies <- function(deriv, name, all_derivs_names) {
  snippet <- deriv$snippet

  # Extract the content inside the Rscript -e quotes (allowing for multiple lines)
  m <- regexec('Rscript -e \\"([\\s\\S]*?)\\"', snippet, perl = TRUE)
  match <- regmatches(snippet, m)

  # Filter out the current derivation from potential dependencies
  derivs_to_consider <- Filter(
    function(x) `!=`(x, name),
    all_derivs_names
  )

  # Check which derivations are referenced in the script
  deps <- sapply(
    derivs_to_consider,
    function(name) any(grepl(paste0("\\b", name, "\\b"), unlist(match)))
  )

  # Only keep dependencies that are TRUE
  deps <- Filter(isTRUE, deps)
  names(deps)
}

#' Extract dependencies from markdown (Quarto/RMarkdown) derivations
#'
#' @param deriv The derivation object
#' @param type The type of derivation ("rxp_qmd" or "rxp_rmd")
#' @param name The name of the derivation
#' @param defined_derivs Previously defined derivations
#' @return A character vector of dependency names
extract_markdown_dependencies <- function(deriv, type, name, defined_derivs) {
  # Determine file path and extension based on type
  if (type == "rxp_qmd") {
    # Try both .qmd and .Qmd extensions
    doc_file <- deriv$qmd_file
    if (!file.exists(doc_file)) {
      doc_file <- paste0(name, ".Qmd")
      if (!file.exists(doc_file)) {
        stop("Quarto file not found: ", name, ".qmd or ", name, ".Qmd")
      }
    }
  } else {
    # rxp_rmd
    # Try .rmd and .Rmd extensions
    doc_file <- deriv$rmd_file
    if (!file.exists(doc_file)) {
      doc_file <- paste0(name, ".Rmd")
      if (!file.exists(doc_file)) {
        stop("R Markdown file not found: ", name, ".rmd or ", name, ".Rmd")
      }
    }
  }

  # Read the document file
  doc_content <- readLines(doc_file, warn = FALSE)
  doc_text <- paste(doc_content, collapse = "\n")

  # Extract R code chunks (between ```{r} and ```)
  chunk_pattern <- "```\\{r\\}[\\s\\S]*?```"
  chunks <- regmatches(
    doc_text,
    gregexpr(chunk_pattern, doc_text, perl = TRUE)
  )[[1]]

  # Process each chunk to find rxp_read() or rxp_load() calls
  deps <- character(0)
  for (chunk in chunks) {
    # Remove the ```{r} and ``` delimiters
    code <- sub("```\\{r\\}\\s*", "", chunk)
    code <- sub("```\\s*$", "", code)

    # Match rxp_read("name") or rxp_load("name"), with or without rixpress::
    pattern <- "(rixpress::)?rxp_(read|load)\\s*\\(\\s*['\"](\\w+)['\"]\\s*\\)"
    matches <- regmatches(code, gregexpr(pattern, code, perl = TRUE))[[1]]

    # Extract the dependency names (group 3 in the pattern)
    if (length(matches) > 0) {
      dep_names <- vapply(
        matches,
        function(m) {
          regmatches(m, regexec(pattern, m, perl = TRUE))[[1]][4]
        },
        character(1)
      )
      deps <- union(deps, dep_names)
    }
  }

  # Filter dependencies to only those previously defined
  intersect(deps, defined_derivs)
}

#' Extract dependencies from Python script derivations
#'
#' @param deriv The derivation object
#' @param name The name of the derivation
#' @param all_derivs_names All derivation names
#' @return A character vector of dependency names
extract_python_dependencies <- function(deriv, name, all_derivs_names) {
  snippet <- deriv$snippet

  # Extract the content inside the python -c quotes (allowing for multiple lines)
  m <- regexec('python -c \\"([\\s\\S]*?)\\"', snippet, perl = TRUE)
  match <- regmatches(snippet, m)

  # Filter out the current derivation from potential dependencies
  derivs_to_consider <- Filter(
    function(x) `!=`(x, name),
    all_derivs_names
  )

  # Check which derivations are referenced in the script
  deps <- sapply(
    derivs_to_consider,
    function(name) any(grepl(paste0("\\b", name, "\\b"), unlist(match)))
  )

  # Only keep dependencies that are TRUE
  deps <- Filter(isTRUE, deps)
  names(deps)
}

#' Extract dependencies from Julia script derivations
#'
#' @param deriv The derivation object
#' @param name The name of the derivation
#' @param all_derivs_names All derivation names
#' @return A character vector of dependency names
extract_julia_dependencies <- function(deriv, name, all_derivs_names) {
  snippet <- deriv$snippet

  # Extract everything between `julia -e "` and the following `'';` (dot‐all mode)
  pat <- '(?s)julia\\s+-e\\s+"(.+?)"\\s*\'\';'
  m <- regexec(pat, snippet, perl = TRUE)
  captures <- regmatches(snippet, m)

  if (length(captures) > 0 && length(captures[[1]]) >= 2) {
    match <- captures[[1]][2]
  } else {
    match <- ""
  }

  # Filter out the current derivation from potential dependencies
  derivs_to_consider <- Filter(
    function(x) `!=`(x, name),
    all_derivs_names
  )

  # Check which derivations are referenced in the script
  deps <- sapply(
    derivs_to_consider,
    function(name) any(grepl(paste0("\\b", name, "\\b"), match))
  )

  # Only keep dependencies that are TRUE
  deps <- Filter(isTRUE, deps)
  names(deps)
}
