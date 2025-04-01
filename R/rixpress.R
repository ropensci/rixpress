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
#' @param derivs A list of derivation objects, where each object is a list of
#'   five elements:
#'     - `$name`, character, name of the derivation
#'     - `$snippet`, character, the nix code snippet to build this derivation
#'     - `$type`, character, can be R, Python or Quarto
#'     - `$additional_files`, character vector of paths to files to make
#'        available to build sandbox
#'     - `$nix_env`, character, path to Nix environment to build this derivation
#'   A single deriv is the output of `rxp_r()`, `rxp_quarto()` or `rxp_py()`
#'   function.
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

  # Need to combine nix envs and additional files into a
  # list of two elements, "nix_env" and "additional_files"
  # which list all the unique combinations
  nix_expressions_and_additional_files <- lapply(
    derivs,
    function(d)
      list(
        "nix_env" = d$nix_env,
        "additional_files" = d$additional_files,
        "type" = d$type
      )
  )
  # Drop quarto objects, as these are handled separately
  nix_expressions_and_additional_files <- lapply(derivs, function(d) {
    if (d$type == "rxp_quarto") {
      d$additional_files <- ""
    }
    list(
      nix_env = d$nix_env,
      additional_files = d$additional_files,
      type = d$type
    )
  })

  flat_list <- list(
    nix_env = sapply(
      nix_expressions_and_additional_files,
      `[[`,
      "nix_env",
      USE.NAMES = FALSE
    ),
    additional_files = sapply(
      nix_expressions_and_additional_files,
      `[[`,
      "additional_files",
      USE.NAMES = FALSE
    )
  )

  nix_env_all <- flat_list$nix_env
  add_files_all <- flat_list$additional_files

  unique_env <- unique(nix_env_all)

  additional_files_combined <- sapply(
    unique_env,
    function(env) {
      idx <- which(nix_env_all == env)
      files <- unlist(add_files_all[idx])
      files <- files[!is.na(files) & files != ""]
      if (length(files) == 0) return("")
      unique(files)
    },
    USE.NAMES = FALSE
  )

  result <- list(
    nix_env = unique_env,
    additional_files = additional_files_combined
  )

  suppressWarnings(
    for (i in seq_along(result$nix_env)) {
      generate_libraries_from_nix(
        result$nix_env[i],
        result$additional_files[[i]],
        project_path = project_path
      )
    }
  )
}


#' parse_nix_envs Parses the nix_env element of a deriv object
#' @param derivs A list of derivation objects, where each object is a list of
#'   five elements:
#'     - `$name`, character, name of the derivation
#'     - `$snippet`, character, the nix code snippet to build this derivation
#'     - `$type`, character, can be R, Python or Quarto
#'     - `$additional_files`, character vector of paths to files to make
#'        available to build sandbox
#'     - `$nix_env`, character, path to Nix environment to build this derivation
#'   Typically, these objects are created by a function like `rxp_r`.
#' @noRd
parse_nix_envs <- function(derivs) {
  nix_envs <- unique(sapply(
    derivs,
    function(d) basename(d$nix_env),
    USE.NAMES = FALSE
  ))

  types <- sapply(derivs, function(d) d$type, USE.NAMES = FALSE)
  need_r <- any(types %in% c("rxp_r", "rxp_r_file", "rxp_quarto", "rxp_py2r"))
  need_py <- any(types %in% c("rxp_py", "rxp_py_file"))

  libraries_r <- character(0)
  libraries_py <- character(0)
  if (need_r) {
    libraries_r <- "libraries.R"
  }
  if (need_py) {
    libraries_py <- "libraries.py"
  }
  libraries <- c(libraries_r, libraries_py)
  libraries <- paste0(libraries, collapse = " ")

  base_name <- basename(nix_envs)
  base_name <- gsub("[^a-zA-Z0-9]", "_", base_name)
  base_name <- sub("_nix$", "", base_name)

  nix_lines <- c(
    paste0(base_name, " = import ./", nix_envs, ";"),
    paste0(base_name, "Pkgs = ", base_name, ".pkgs;"),
    paste0(base_name, "Shell = ", base_name, ".shell;"),
    paste0(base_name, "BuildInputs = ", base_name, "Shell.buildInputs;"),
    paste0(
      base_name,
      "ConfigurePhase = ''\n    cp ${./_rixpress/",
      base_name,
      "_",
      libraries,
      "} ",
      libraries,
      "\n    mkdir -p $out\n  ",
      "'';"
    )
  )
  paste(nix_lines, collapse = "\n  ")
}

#' gen_flat_pipeline Internal function used to generate most of the boilerplate in pipeline.nix
#' @param derivs A list of derivation objects, where each object is a list of
#'   five elements:
#'     - `$name`, character, name of the derivation
#'     - `$snippet`, character, the nix code snippet to build this derivation
#'     - `$type`, character, can be R, Python or Quarto
#'     - `$additional_files`, character vector of paths to files to make
#'        available to build sandbox
#'     - `$nix_env`, character, path to Nix environment to build this derivation
#'   A single deriv is the output of `rxp_r()`, `rxp_quarto()` or `rxp_py()`
#'   function.
#' @noRd
gen_flat_pipeline <- function(derivs) {
  derivation_texts <- sapply(derivs, function(d) d$snippet)
  derivations_code <- paste(derivation_texts, collapse = "\n\n")

  deriv_names <- sapply(derivs, function(d) d$name)
  names_line <- paste(deriv_names, collapse = " ")

  nix_envs <- parse_nix_envs(derivs)

  # Determine required functions
  types <- sapply(derivs, function(d) d$type)
  need_r <- any(types %in% c("rxp_r", "rxp_r_file", "rxp_quarto"))
  need_py <- any(types %in% c("rxp_py", "rxp_py_file"))

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
  pipeline <- flat_pipeline

  for (d in dag$derivations) {
    if (
      length(d$depends) == 0 || d$type == "rxp_quarto" || d$type == "rxp_py2r"
    )
      next

    deriv_name <- as.character(d$deriv_name[1])
    deps <- d$depends
    type <- d$type[1]

    # Set parameters based on derivation type
    if (type == "rxp_r") {
      maker <- "makeRDerivation"
      script_cmd <- "Rscript -e \""
      load_line <- function(dep, indent) {
        paste0(indent, dep, " <- readRDS('${", dep, "}/", dep, ".rds')")
      }
    } else if (type == "rxp_py") {
      maker <- "makePyDerivation"
      script_cmd <- "python -c \""
      load_line <- function(dep, indent) {
        paste0(
          "with open('${",
          dep,
          "}/",
          dep,
          ".pickle', 'rb') as f: ",
          dep,
          " = pickle.load(f)"
        )
      }
    } else {
      warning("Unsupported type for derivation ", deriv_name)
      next
    }

    # Locate the derivation block
    pattern <- paste0("^\\s*", deriv_name, " = ", maker, " \\{")
    start_idx <- grep(pattern, pipeline)
    if (!length(start_idx)) {
      warning("Derivation ", deriv_name, " not found")
      next
    }
    start_idx <- start_idx[1]

    # Find the end of the block
    end_candidates <- grep("^\\s*};", pipeline)
    block_end_idx <- end_candidates[end_candidates > start_idx][1]
    if (is.na(block_end_idx)) {
      warning("Block end for ", deriv_name, " not found")
      next
    }

    block <- pipeline[start_idx:block_end_idx]
    bp_idx <- grep("buildPhase = ''", block)
    if (!length(bp_idx)) {
      warning("buildPhase not found for ", deriv_name)
      next
    }
    build_phase_idx <- start_idx + bp_idx[1] - 1

    sub_block <- block[bp_idx[1]:length(block)]
    script_idx <- grep(script_cmd, sub_block, fixed = TRUE)
    if (!length(script_idx)) {
      warning("Script command not found in buildPhase for ", deriv_name)
      next
    }
    script_idx <- build_phase_idx + script_idx[1]

    # Determine indentation for R scripts, none for Python
    indent <- if (type == "rxp_r") {
      if (script_idx + 1 <= length(pipeline)) {
        sub("^([[:space:]]*).*", "\\1", pipeline[script_idx + 1])
      } else {
        "        "
      }
    } else {
      ""
    }

    load_lines <- sapply(deps, load_line, indent)
    pipeline <- append(pipeline, load_lines, after = build_phase_idx + 2)
  }

  pipeline
}


#' Generate an R or Py script with library calls from a default.nix file
#'
#' @param nix_env Nix environment where the derivation runs
#' @param additional_files Character vector, additional files to include. These
#'   are the files that contain custom functions required for this derivation.
#' @param project_path Path to root of project, typically "."
#' @return An script to load the libraries inside of derivations.
#' @noRd
generate_libraries_from_nix <- function(
  nix_env,
  additional_files = "",
  project_path
) {
  generate_r_libraries_from_nix(
    nix_env,
    additional_files,
    project_path
  )
  generate_py_libraries_from_nix(
    nix_env,
    additional_files,
    project_path
  )
}
