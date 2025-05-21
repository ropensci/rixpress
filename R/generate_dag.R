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
  dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)

  all_derivs_names <- sapply(rxp_list, function(x) (x$name))

  n <- length(rxp_list)
  dag <- vector("list", n)
  defined <- character(n)

  for (i in seq_along(rxp_list)) {
    d <- rxp_list[[i]]
    name <- d$name
    type <- d$type
    unserialize_function <- d$unserialize_function

    # Helper function to extract dependencies from script-based derivations.
    # Searches for derivation names within the script content extracted by script_regex.
    # - snippet_content: The Nix snippet string for the derivation.
    # - script_regex: PCRE regex to extract the executable script portion.
    #   Must contain one capturing group for the script content.
    # - all_deriv_names: Character vector of all derivation names in the pipeline.
    # - current_deriv_name: Name of the derivation currently being processed.
    # Returns a character vector of dependency names.
    extract_script_dependencies <- function(
      snippet_content,
      script_regex,
      all_deriv_names,
      current_deriv_name
    ) {
      m <- regexec(script_regex, snippet_content, perl = TRUE) # PCRE for features like (?s)
      match_list <- regmatches(snippet_content, m)

      script_text <- ""
      # Expecting the captured script text to be the second element of the first list item
      if (length(match_list) > 0 && length(match_list[[1]]) >= 2) {
        script_text <- match_list[[1]][2] # The first captured group
      }

      if (identical(script_text, "") || is.na(script_text)) {
        # is.na if regex doesn't match
        return(character(0)) # No script content found or regex failed
      }

      # Consider only other derivations as potential dependencies
      derivs_to_consider <- Filter(
        function(x) `!=`(x, current_deriv_name),
        all_deriv_names
      )

      if (length(derivs_to_consider) == 0) {
        return(character(0))
      }

      # Check for presence of each potential dependency name as a whole word
      dep_flags <- sapply(
        derivs_to_consider,
        function(dep_name)
          any(grepl(paste0("\\b", dep_name, "\\b"), script_text, perl = TRUE))
      )

      deps <- Filter(isTRUE, dep_flags)
      return(names(deps))
    }

    if (type == "rxp_r" || type == "rxp_py2r" || type == "rxp_r2py") {
      # For R scripts: extracts content from Rscript -e "..."
      deps <- extract_script_dependencies(
        snippet_content = d$snippet,
        script_regex = 'Rscript -e \\"([\\s\\S]*?)\\"', # Matches multi-line content in Rscript -e "..."
        all_deriv_names = all_derivs_names,
        current_deriv_name = name
      )
    } else if (type == "rxp_qmd" || type == "rxp_rmd") {
      # Determine file path and extension based on type
      if (type == "rxp_qmd") {
        # Try both .qmd and .Qmd extensions
        doc_file <- d$qmd_file
        if (!file.exists(doc_file)) {
          doc_file <- paste0(name, ".Qmd")
          if (!file.exists(doc_file)) {
            stop("Quarto file not found: ", name, ".qmd or ", name, ".Qmd")
          }
        }
      } else {
        # rxp_rmd
        # Try .rmd and .Rmd extensions
        doc_file <- d$rmd_file
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

        # Regex to match `rxp_read("name")` or `rxp_load("name")`,
        # optionally prefixed with `rixpress::`. Captures "name".
        # `(\\w+)` captures the derivation name (alphanumeric and underscore).
        dep_pattern_qmd <- "(rixpress::)?rxp_(read|load)\\s*\\(\\s*['\"](\\w+)['\"]\\s*\\)"
        matches <- regmatches(
          code,
          gregexpr(dep_pattern_qmd, code, perl = TRUE)
        )[[1]]

        # Extract the dependency names (captured group 3 from dep_pattern_qmd)
        if (length(matches) > 0) {
          dep_names <- vapply(
            matches,
            function(m) {
              regmatches(m, regexec(dep_pattern_qmd, m, perl = TRUE))[[1]][4]
            },
            character(1)
          )
          deps <- union(deps, dep_names)
        }
      }
      # Filter dependencies to only those previously defined in the rxp_list order
      # This is specific to QMD/RMD to ensure they only depend on objects already available.
      deps <- intersect(deps, defined[1:(i - 1)])
    } else if (type == "rxp_py") {
      # NB: rxp_r2py is R-based, handled by the Rscript block
      # For Python scripts: extracts content from python -c "..."
      deps <- extract_script_dependencies(
        snippet_content = d$snippet,
        script_regex = 'python -c \\"([\\s\\S]*?)\\"', # Matches multi-line content in python -c "..."
        all_deriv_names = all_derivs_names,
        current_deriv_name = name
      )
    } else if (type == "rxp_jl") {
      # For Julia scripts: extracts content from julia -e "..."
      # The regex `(?s)julia\\s+-e\\s+"(.+?)"` uses `(?s)` for dot-all to match multi-line
      # content within the quotes, and captures the content. It expects the script part to
      # be followed by a quote and then `''` (as per `rxp_jl` snippet construction),
      # but this regex focuses on just capturing the content inside `julia -e "..."`.
      deps <- extract_script_dependencies(
        snippet_content = d$snippet,
        script_regex = '(?s)julia\\s+-e\\s+"(.+?)"',
        all_deriv_names = all_derivs_names,
        current_deriv_name = name
      )
    } else if (type %in% c("rxp_copy", "rxp_r_file", "rxp_py_file")) {
      # Types with no script dependencies
      deps <- character(0)
    } else {
      # This will catch unhandled types including potentially rxp_r2py if it wasn't meant for Rscript block
      warning(
        "Unknown or unhandled derivation type for dependency extraction: ",
        type,
        " in derivation ",
        name
      )
      deps <- character(0) # Default to no dependencies for unknown types
    }
    dag[[i]] <- list(
      deriv_name = name,
      depends = deps,
      unserialize_function = unserialize_function,
      type = type
    )
    defined[i] <- name
  }

  jsonlite::write_json(list(derivations = dag), output_file, pretty = TRUE)

  if (identical(Sys.getenv("TESTTHAT"), "true")) {
    output_file
  }
}
