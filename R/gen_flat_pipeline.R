#' Generate Nix Pipeline Code with a Generic Default Target
#'
#' This function generates a string containing Nix code for a `pipeline.nix` file
#' based on a list of derivation objects. Each derivation is defined by a snippet
#' of Nix code and a name, typically created using a function like `mk_r`. The
#' resulting Nix code defines all derivations and sets a generic default target
#' that builds all of them using `pkgs.symlinkJoin`. This is designed for
#' building reproducible analytical pipelines with Nix, inspired by the
#' `targets` R package.
#'
#' @param derivs A list of derivation objects, where each object must have two
#'   elements: `$name` (a character string naming the derivation) and `$snippet`
#'   (a character string containing the Nix code defining the derivation).
#'   Typically, these objects are created by a function like `mk_r`.
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
#' # Define a simple mk_r function to create derivations
#' mk_r <- function(name, expr) {
#'   out_name <- deparse(substitute(name))
#'   expr_str <- deparse(substitute(expr))
#'   list(
#'     name = out_name,
#'     snippet = sprintf(
#'       '  %s = makeRDerivation {\n    name = "%s";\n    buildPhase = \'\'\n      Rscript -e "\\n        source(\'libraries.R\')\\n        %s <- %s\\n        saveRDS(%s, \'%s.rds\')"\n    \'\';\n  };',
#'       out_name, out_name, out_name, expr_str, out_name, out_name
#'     )
#'   )
#' }
#'
#' # Create derivation objects
#' d1 <- mk_r(mtcars_am, filter(mtcars, am == 1))
#' d2 <- mk_r(mtcars_head, head(mtcars_am))
#' deriv_list <- list(d1, d2)
#'
#' # Generate the pipeline code
#' pipeline_code <- gen_flat_pipeline(deriv_list)
#'
#' # Optionally write to a file
#' writeLines(pipeline_code, "pipeline.nix")
#'
#' @seealso \code{\link[rix]{rix}} for generating `default.nix` files,
#'   \code{\link[targets]{tar_make}} for inspiration on pipeline workflows.
#'
#' @export
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

  return(pipeline_nix)
}

# ----- Example Usage -----
# Create derivation snippets using mk_r.
d1 <- mk_r(mtcars_am, filter(mtcars, am == 1))
d2 <- mk_r(mtcars_head, head(mtcars_am))

# Collect the snippets in a list.
deriv_list <- list(d1, d2)

# Build the full pipeline.nix code.
flat_pipeline_code <- gen_flat_pipeline(deriv_list)

# Write the pipeline.nix file or print it.
writeLines(flat_pipeline_code, "flat_pipeline.nix")
cat(flat_pipeline_code)
