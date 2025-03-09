#' Generate Nix Pipeline Code with a Generic Default Target
#'
#' This function generates a string containing Nix code for a `pipeline.nix` file
#' based on a list of derivation objects. Each derivation is defined by a snippet
#' of Nix code and a name, typically created using a function like `drv_r`. The
#' resulting Nix code defines all derivations and sets a generic default target
#' that builds all of them using `pkgs.symlinkJoin`. This is designed for
#' building reproducible analytical pipelines with Nix, inspired by the
#' `targets` R package.
#'
#' @param derivs A list of derivation objects, where each object must have two
#'   elements: `$name` (a character string naming the derivation) and `$snippet`
#'   (a character string containing the Nix code defining the derivation).
#'   Typically, these objects are created by a function like `drv_r`.
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
#' d1 <- drv_r(mtcars_am, filter(mtcars, am == 1))
#' d2 <- drv_r(mtcars_head, head(mtcars_am))
#' derivs <- list(d1, d2)
#'
#' # Generate the pipeline code
#' rixpress(derivs)
#'
#' }
#' @export
rixpress <- function(derivs) {
  flat_pipeline <- gen_flat_pipeline(derivs)

  generate_dag(derivs, output_file = "_rixpress/dag.json")

  pipeline <- gen_pipeline(
    dag_file = "_rixpress/dag.json",
    flat_pipeline = flat_pipeline
  )

  writeLines(pipeline, "pipeline.nix")
  generate_libraries_from_nix("default.nix")
}

#' gen_flat_pipeline Internal function used to generate most of the boilerplate in pipeline.nix
#' @param derivs A list of derivation objects, where each object must have two
#'   elements: `$name` (a character string naming the derivation) and `$snippet`
#'   (a character string containing the Nix code defining the derivation).
#'   Typically, these objects are created by a function like `drv_r`.
#' @noRd
gen_flat_pipeline <- function(derivs) {
  # Extract derivation snippets and names from the derivs list
  derivation_texts <- sapply(derivs, function(d) d$snippet)
  deriv_names <- sapply(derivs, function(d) d$name)

  # Combine all derivation snippets into a single string
  derivations_code <- paste(derivation_texts, collapse = "\n\n")
  # Create a space-separated list of derivation names
  names_line <- paste(deriv_names, collapse = " ")

  # Generate the Nix code as a string
  pipeline_nix <- sprintf(
    'let
  default = import ./default.nix;
  pkgs = default.pkgs;
  shell = default.shell;

  commonBuildInputs = shell.buildInputs;
  commonConfigurePhase = \'\'\n    cp ${./libraries.R} libraries.R\n    mkdir -p $out\n  \'\';

  # Function to create R derivations
  makeRDerivation = { name, buildPhase }:
    let rdsFile = "${name}.rds";
    in pkgs.stdenv.mkDerivation {
      inherit name;
      buildInputs = commonBuildInputs;
      dontUnpack = true;
      configurePhase = commonConfigurePhase;
      inherit buildPhase;
      installPhase = \'\'\n        cp ${rdsFile} $out/\n      \'\';
    };

  # Define all derivations
%s

  # Generic default target that builds all derivations
  allDerivations = pkgs.symlinkJoin {
    name = "all-derivations";
    paths = with builtins; attrValues { inherit %s; };
  };

in
{
  inherit %s;  # Make individual derivations available as attributes
  default = allDerivations;  # Set the default target to build everything
}
',
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
  result_pipeline <- flat_pipeline # copy to work on

  for (deriv in dag$derivations) {
    # Skip derivations with no dependencies
    if (any(length(deriv$depends) == 0 | deriv$type == "drv_quarto")) next

    deriv_name <- as.character(deriv$deriv_name[1])
    deps <- deriv$depends

    # Locate the derivation block by matching a line that starts with the derivation name
    block_pattern <- paste0("^\\s*", deriv_name, " = makeRDerivation \\{")
    start_idx <- grep(block_pattern, result_pipeline)
    if (length(start_idx) == 0) {
      warning(paste("Derivation", deriv_name, "not found"))
      next
    }
    start_idx <- start_idx[1]

    # Find the end of the block: the first line after start_idx that starts with "};"
    block_end_candidates <- grep("^\\s*};", result_pipeline)
    block_end_idx <- block_end_candidates[block_end_candidates > start_idx][1]
    if (is.na(block_end_idx)) {
      warning(paste("Block end for", deriv_name, "not found"))
      next
    }

    # Restrict our search to the lines in this derivation block
    block_lines <- result_pipeline[start_idx:block_end_idx]

    # Locate the buildPhase line within the block
    build_phase_local_idx <- grep("buildPhase = ''", block_lines)
    if (length(build_phase_local_idx) == 0) {
      warning(paste("buildPhase not found for", deriv_name))
      next
    }
    build_phase_idx <- start_idx + build_phase_local_idx[1] - 1

    # Within the block, search for the Rscript line after the buildPhase line
    sub_block <- block_lines[build_phase_local_idx[1]:length(block_lines)]
    rscript_local_idx <- grep("Rscript -e \"", sub_block, fixed = TRUE)
    if (length(rscript_local_idx) == 0) {
      warning(paste("Rscript not found in buildPhase for", deriv_name))
      next
    }
    rscript_idx <- build_phase_idx + rscript_local_idx[1]

    # Determine indentation from the line immediately after the Rscript line
    if (rscript_idx + 1 <= length(result_pipeline)) {
      first_r_line <- result_pipeline[rscript_idx + 1]
      indentation <- sub("^([[:space:]]*).*", "\\1", first_r_line)
    } else {
      indentation <- "        " # fallback indentation
    }

    # Generate a readRDS call for each dependency
    readRDS_lines <- sapply(deps, function(dep) {
      paste0(indentation, dep, " <- readRDS('${", dep, "}/", dep, ".rds')")
    })

    # Insert the generated readRDS lines right after the Rscript line in the block
    result_pipeline <- append(
      result_pipeline,
      readRDS_lines,
      after = rscript_idx
    )
  }

  result_pipeline
}
