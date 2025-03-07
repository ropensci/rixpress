#' mk_r Creates a Nix expression running an R function
#' @param output Character, a variable name to save the
#'   output of the function expression
#' @param name Symbol, name of the derivation.
#' @param expr R code to generate the expression.
#' @details At a basic level, `mk_r(mtcars_am, filter(mtcars, am == 1))`
#'   is equivalent to `mtcars <- filter(mtcars, am == 1)`. `mk_r()` generates
#'   the required Nix boilerplate to output a so-called "derivation" in Nix
#'   jargon. A Nix derivation is a recipe that defines how to create an output
#'   (in this case `mtcars_am`) including its dependencies, build steps,
#'   and output paths.
#' @return A list of two elements, `name`, the `name` of the derivation,
#'   and `snippet` the Nix boilerplate code.
#' @examples mk_r(mtcars_am, filter(mtcars, am == 1))
#' @importFrom rlang as_label enexpr
#' @export
mk_r <- function(name, expr) {
  out_name <- rlang::as_label(rlang::enexpr(name))
  expr_str <- rlang::as_label(rlang::enexpr(expr))

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
