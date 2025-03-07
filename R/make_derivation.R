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


