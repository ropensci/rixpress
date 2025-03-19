#' Generate Nix Pipeline Code with a Generic Default Target
#'
#' This function generates a string containing Nix code for a `pipeline.nix` file
#' based on a list of derivation objects. Each derivation is defined by a snippet
#' of Nix code and a name, typically created using a function like `rxp_r`. The
#' resulting Nix code defines all derivations and sets a generic default target
#' that builds all of them using `pkgs.symlinkJoin`. This is designed for
#' building reproducible analytical pipelines with Nix, inspired by the
#' `targets` R package.
#'
#' @param derivs A list of derivation objects, where each object must have two
#'   elements: `$name` (a character string naming the derivation) and `$snippet`
#'   (a character string containing the Nix code defining the derivation).
#'   Typically, these objects are created by a function like `rxp_r`.
#'
#' @param project_path Path to root of project, typically "."
#'
#' @return A character string containing the complete Nix code for a
#'   `pipeline.nix` file. This string can be written to a file or passed to
#'   another function for further processing.
#'
#' @details
#' The generated Nix code includes:
#' - A `default.nix` import for environment setup via the `rix` package.
#' - A `makeRDerivation` function to standardize derivation creation.
#' - All derivations from the `derivs` list.
#' - A default target (`allDerivations`) that builds all derivations when
#'   `nix-build pipeline.nix` is run without specifying an attribute.
#'
#' The function assumes the existence of a `libraries.R` file in the working
#' directory, which is copied into the build environment by the Nix code.
#'
#' @examples
#' \dontrun{
#' # Create derivation objects
#' d1 <- rxp_r(mtcars_am, filter(mtcars, am == 1))
#' d2 <- rxp_r(mtcars_head, head(mtcars_am))
#' list_derivs <- list(d1, d2)
#'
#' # Generate the pipeline code
#' rixpress(derivs = list_derivs, project_path = ".")
#'
#' }
#' @export
rixpress <- function(derivs, project_path) {
  flat_pipeline <- gen_flat_pipeline(derivs)

  generate_dag(derivs, output_file = "_rixpress/dag.json")

  pipeline <- gen_pipeline(
    dag_file = "_rixpress/dag.json",
    flat_pipeline = flat_pipeline
  )

  writeLines(pipeline, "pipeline.nix")
  extract_nix_file <- function(text) {
    pattern <- "import \\./([^;]+);"

    # Find the match and its position
    m <- regexec(pattern, text)

    if (m[[1]][1] == -1) {
      stop("No import statement found")
    }

    matches <- regmatches(text, m)

    matches[[1]][2]
  }

  nix_expressions <- unique(sapply(
    sapply(derivs, function(d) d$nix_env),
    extract_nix_file,
    USE.NAMES = FALSE
  ))

  suppressWarnings(
    invisible(
      lapply(
        nix_expressions,
        generate_libraries_from_nix,
        project_path = project_path
      )
    )
  )
}

#' gen_flat_pipeline Internal function used to generate most of the boilerplate in pipeline.nix
#' @param derivs A list of derivation objects, where each object must have two
#'   elements: `$name` (a character string naming the derivation) and `$snippet`
#'   (a character string containing the Nix code defining the derivation).
#'   Typically, these objects are created by a function like `rxp_r`.
#' @noRd
gen_flat_pipeline <- function(derivs) {
  # Extract derivation snippets and names
  derivation_texts <- sapply(derivs, function(d) d$snippet)
  deriv_names <- sapply(derivs, function(d) d$name)
  nix_envs <- paste0(
    unique(sapply(derivs, function(d) d$nix_env)),
    collapse = "\n"
  )
  derivations_code <- paste(derivation_texts, collapse = "\n\n")
  names_line <- paste(deriv_names, collapse = " ")

  # Determine required functions
  types <- sapply(derivs, function(d) d$type)
  need_r <- any(types %in% c("rxp_r", "rxp_file", "rxp_quarto"))
  need_py <- any(types %in% c("rxp_py"))

  # Build function definitions
  function_defs <- ""
  if (need_r) {
    function_defs <- paste0(
      function_defs,
      "
  # Function to create R derivations
  makeRDerivation = { name, buildInputs, configurePhase, buildPhase, src ? null }:
    let rdsFile = \"${name}.rds\";
    in defaultPkgs.stdenv.mkDerivation {
      inherit name src;
      dontUnpack = true;
      inherit buildInputs configurePhase buildPhase;
      installPhase = ''
        cp ${rdsFile} $out/
      '';
    };"
    )
  }
  if (need_py) {
    function_defs <- paste0(
      function_defs,
      "
  # Function to create Python derivations
  makePyDerivation = { name, buildInputs, configurePhase, buildPhase, src ? null }:
    let
      pickleFile = \"${name}.pickle\";
    in
      defaultPkgs.stdenv.mkDerivation {
        inherit name src;
        dontUnpack = true;
        buildInputs = buildInputs;
        inherit configurePhase buildPhase;
        installPhase = ''
          cp ${pickleFile} $out
        '';
      };"
    )
  }

  # Generate Nix code
  pipeline_nix <- sprintf(
    'let
  %s
  %s

  # Define all derivations
  %s

  # Generic default target that builds all derivations
  allDerivations = defaultPkgs.symlinkJoin {
    name = "all-derivations";
    paths = with builtins; attrValues { inherit %s; };
  };

in
{
  inherit %s;
  default = allDerivations;
}
',
    nix_envs,
    function_defs,
    derivations_code,
    names_line,
    names_line
  )

  strsplit(pipeline_nix, split = "\n")[[1]]
}

#' gen_pipeline Internal function used to finalize a flat pipeline
#' @param dag_file A json file giving the names and relationships between derivations.
#' @param flat_pipeline A flat pipeline, output of `gen_flat_elements()`.
#' @noRd
gen_pipeline <- function(dag_file, flat_pipeline) {
  dag <- jsonlite::read_json(dag_file)
  result_pipeline <- flat_pipeline # Copy to work on

  for (deriv in dag$derivations) {
    # Skip derivations with no dependencies or of type "rxp_quarto"
    if (length(deriv$depends) == 0 || deriv$type == "rxp_quarto") next

    deriv_name <- as.character(deriv$deriv_name[1])
    deps <- deriv$depends
    type <- deriv$type[1]

    # Determine the maker and script command based on type
    if (type == "rxp_r") {
      maker <- "makeRDerivation"
      script_cmd <- "Rscript -e \""
      load_func <- function(dep, indentation) {
        paste0(indentation, dep, " <- readRDS('${", dep, "}/", dep, ".rds')")
      }
    } else if (type == "rxp_py") {
      maker <- "makePyDerivation"
      script_cmd <- "python -c \""
      load_func <- function(deps, indentation) {
        # Generate unindented loading lines without import pickle
        unlist(lapply(deps, function(dep) {
          c(
            paste0(
              "with open('${",
              dep,
              "}/",
              dep,
              ".pickle', 'rb') as f: ",
              dep,
              " = pickle.load(f)"
            )
          )
        }))
      }
    } else {
      warning(paste("Unsupported type for derivation", deriv_name))
      next
    }

    # Locate the derivation block
    block_pattern <- paste0("^\\s*", deriv_name, " = ", maker, " \\{")
    start_idx <- grep(block_pattern, result_pipeline)
    if (length(start_idx) == 0) {
      warning(paste("Derivation", deriv_name, "not found"))
      next
    }
    start_idx <- start_idx[1]

    # Find the end of the block
    block_end_candidates <- grep("^\\s*};", result_pipeline)
    block_end_idx <- block_end_candidates[block_end_candidates > start_idx][1]
    if (is.na(block_end_idx)) {
      warning(paste("Block end for", deriv_name, "not found"))
      next
    }

    # Extract block lines
    block_lines <- result_pipeline[start_idx:block_end_idx]

    # Locate the buildPhase line
    build_phase_local_idx <- grep("buildPhase = ''", block_lines)
    if (length(build_phase_local_idx) == 0) {
      warning(paste("buildPhase not found for", deriv_name))
      next
    }
    build_phase_idx <- start_idx + build_phase_local_idx[1] - 1

    # Search for the script command line
    sub_block <- block_lines[build_phase_local_idx[1]:length(block_lines)]
    script_local_idx <- grep(script_cmd, sub_block, fixed = TRUE)
    if (length(script_local_idx) == 0) {
      warning(paste("Script command not found in buildPhase for", deriv_name))
      next
    }
    script_idx <- build_phase_idx + script_local_idx[1]

    # Generate loading lines (no indentation for Python)
    if (type == "rxp_r") {
      # Keep indentation for R
      if (script_idx + 1 <= length(result_pipeline)) {
        first_code_line <- result_pipeline[script_idx + 1]
        indentation <- sub("^([[:space:]]*).*", "\\1", first_code_line)
      } else {
        indentation <- "        " # Fallback indentation
      }
      load_lines <- sapply(deps, function(dep) load_func(dep, indentation))
    } else if (type == "rxp_py") {
      # No indentation for Python
      load_lines <- load_func(deps, "")
    }

    # Insert the loading lines after the script command line
    result_pipeline <- append(
      result_pipeline,
      load_lines,
      after = build_phase_idx + 2
    )
  }

  result_pipeline
}

#' Generate an R or Py script with library calls from a default.nix file
#'
#' @param nix_file Path to the default.nix file (default: "default.nix")
#' @param project_path Path to root of project, typically "."
#' @return An script to load the libraries inside of derivations.
#' @noRd
generate_libraries_from_nix <- function(nix_file, project_path) {
  generate_r_libraries_from_nix(nix_file, project_path)
  generate_py_libraries_from_nix(nix_file, project_path)
}
