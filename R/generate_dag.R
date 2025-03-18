#' Generate a DAG from a list of derivations
#'
#' Creates a JSON representation of a directed acyclic graph (DAG) based on dependencies
#' between derivations.
#'
#' @param rxp_list A list of derivations, each with a `name` and `snippet`, output of rxp_r().
#' @param output_file Path to the output JSON file. Defaults to "_rixpress/dag.json".
#' @importFrom jsonlite write_json
#' @return Writes a JSON file representing the DAG.
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

    if (type == "rxp_r") {
      snippet <- d$snippet
      # Extract the content inside the Rscript -e quotes
      # (allowing for multiple lines)
      m <- regexec('Rscript -e \\"([\\s\\S]*?)\\"', snippet, perl = TRUE)
      match <- regmatches(snippet, m)

      derivs_to_consider <- Filter(
        function(x) `!=`(x, name),
        all_derivs_names
      )

      deps <- sapply(
        derivs_to_consider,
        function(name) any(grepl(paste0("\\b", name, "\\b"), unlist(match)))
      )

      # Only keep deps
      deps <- Filter(isTRUE, deps)
      deps <- names(deps)
    } else if (type == "rxp_quarto") {
      # Try both .qmd and .Qmd extensions
      qmd_file <- paste0(name, ".qmd")
      if (!file.exists(qmd_file)) {
        qmd_file <- paste0(name, ".Qmd")
        if (!file.exists(qmd_file)) {
          stop("Quarto file not found: ", name, ".qmd or ", name, ".Qmd")
        }
      }
      # Read the Quarto file
      qmd_content <- readLines(qmd_file, warn = FALSE)
      qmd_text <- paste(qmd_content, collapse = "\n")

      # Extract R code chunks (between ```{r} and ```)
      chunk_pattern <- "```\\{r\\}[\\s\\S]*?```"
      chunks <- regmatches(
        qmd_text,
        gregexpr(chunk_pattern, qmd_text, perl = TRUE)
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
      deps <- intersect(deps, defined[1:(i - 1)])
    } else if (type == "rxp_py") {
      snippet <- d$snippet
      # Extract the content inside the python -c quotes (allowing for multiple lines)
      m <- regexec('python -c \\"([\\s\\S]*?)\\"', snippet, perl = TRUE)
      match <- regmatches(snippet, m)
      block <- if (length(match[[1]]) > 1) match[[1]][2] else ""
      # Split the block into lines
      lines <- unlist(strsplit(block, "\n"))
      # Find lines that load pickled objects, e.g., with open('${dep}/dep.pkl', 'rb') as f:
      load_pattern <- "with open\\('\\$\\{(\\w+)\\}/\\1\\.pkl', 'rb'\\) as f:"
      load_lines <- grep(load_pattern, lines, value = TRUE)
      # Extract the dependency names
      deps <- unique(sub(load_pattern, "\\1", load_lines))
      # Filter to only those previously defined
      deps <- intersect(deps, defined[1:(i - 1)])
    } else {
      stop("Unknown derivation type: ", type)
    }

    dag[[i]] <- list(deriv_name = name, depends = deps, type = type)
    defined[i] <- name
  }

  jsonlite::write_json(list(derivations = dag), output_file, pretty = TRUE)

  if (identical(Sys.getenv("TESTTHAT"), "true")) {
    output_file
  }
}
