#' mk_r Creates a Nix expression running an R function
#' @param output Character, a variable name to save the
#'   output of the function expression
#' @param f A function expression, e.g., sqrt(2)
#' @param ide Character, the ide to use.
#' @examples
#' mk_r(mtcars_am, filter(mtcars, am == 1))
#' @noRd
mk_r <- function(name, expr) {
  out_name <- as_label(enexpr(name))
  expr_str <- as_label(enexpr(expr))

  build_phase <- sprintf(
    "Rscript -e \"\n        source('libraries.R')\n        %s <- %s\n        saveRDS(%s, '%s.rds')\"",
    out_name,
    expr_str,
    out_name,
    out_name
  )

  snippet <- sprintf(
    "  %s = makeRDerivation {\n    name = \"%s\";\n    buildPhase = ''\n      %s\n    '';\n  };",
    out_name,
    out_name,
    build_phase
  )

  list(name = out_name, snippet = snippet)
}


# build_pipeline now expects a list of mk_r objects (each with $name and $snippet)
build_pipeline <- function(derivs) {
  # Extract the snippets and names directly from the objects.
  derivation_texts <- sapply(derivs, function(d) d$snippet)
  deriv_names <- sapply(derivs, function(d) d$name)

  derivations_code <- paste(derivation_texts, collapse = "\n\n")
  names_line <- paste(deriv_names, collapse = " ")

  # Build the flat JSON mapping block.
  mapping_lines <- sapply(deriv_names, function(n) {
    sprintf("    %s = \"${%s}/%s.rds\";", n, n, n)
  })
  mapping_block <- paste(mapping_lines, collapse = "\n")

  # Construct the full pipeline.nix text.
  pipeline_nix <- sprintf(
    'let
  default = import ./default.nix;
  pkgs = default.pkgs;
  shell = default.shell;

  commonBuildInputs = shell.buildInputs;
  commonConfigurePhase = \'\'\n    cp ${./libraries.R} libraries.R\n    mkdir -p $out\n  \'\';

  # Function to create R derivations, without depends parameter
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

  # Define derivations
%s

  # Define a flat JSON mapping of derivation names to their output paths
  pathMappingJson = builtins.toJSON {
%s
  };

  # Write the flat JSON to a file
  pathMapping = pkgs.writeText "pathMapping.json" pathMappingJson;

in
{
  inherit %s;
  inherit pathMapping;
  default = pathMapping;  # Set pathMapping as the default target
}
',
    derivations_code,
    mapping_block,
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
pipeline_code <- build_pipeline(deriv_list)

# Write the pipeline.nix file or print it.
writeLines(pipeline_code, "pipeline.nix")
cat(pipeline_code)
