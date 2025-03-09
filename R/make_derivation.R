#' drv_r Creates a Nix expression running an R function
#' @param output Character, a variable name to save the
#'   output of the function expression
#' @param name Symbol, name of the derivation.
#' @param expr R code to generate the expression.
#' @details At a basic level, `drv_r(mtcars_am, filter(mtcars, am == 1))`
#'   is equivalent to `mtcars <- filter(mtcars, am == 1)`. `drv_r()` generates
#'   the required Nix boilerplate to output a so-called "derivation" in Nix
#'   jargon. A Nix derivation is a recipe that defines how to create an output
#'   (in this case `mtcars_am`) including its dependencies, build steps,
#'   and output paths.
#' @return A list of two elements, `name`, the `name` of the derivation,
#'   and `snippet` the Nix boilerplate code.
#' @examples drv_r(mtcars_am, filter(mtcars, am == 1))
#' @importFrom rlang as_label enexpr
#' @export
drv_r <- function(name, expr) {
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

  list(name = out_name, snippet = snippet, type = "drv_r")
}

#' Render a Quarto document as a Nix derivation
#'
#' @param name Symbol, derivation name.
#' @param qmd_file Character, path to .qmd file.
#'
#' @details Detects `drv_read("ref")` in the .qmd file and replaces with derivation output paths.
#'
#' @return List with `name` (string) and `snippet` (Nix code).
#'
#' @examples
#' drv_quarto(report, "doc.qmd")
#'
#' @importFrom rlang as_label enexpr
#' @export
drv_quarto <- function(name, qmd_file) {
  name_str <- rlang::as_label(rlang::enexpr(name))

  # Parse qmd file for refs
  content <- readLines(qmd_file, warn = FALSE)
  content_str <- paste(content, collapse = "\n")
  matches <- gregexpr('drv_read\\("([^"]+)"\\)', content_str)
  refs <- regmatches(content_str, matches)[[1]]
  refs <- sub('drv_read\\("([^"]+)"\\)', '\\1', refs)
  refs <- unique(refs)

  # Substitution commands
  sub_cmds <- sapply(refs, function(ref) {
    sprintf(
      "substituteInPlace %s --replace-fail 'drv_read(\"%s\")' 'drv_read(\"${%s}/%s.rds\")'",
      qmd_file,
      ref,
      ref,
      ref
    )
  })

  # Build phase
  build_phase <- paste(
    "  mkdir home",
    "  export HOME=$PWD/home",
    if (length(sub_cmds) > 0)
      paste("  ", sub_cmds, sep = "", collapse = "\n") else "",
    sprintf("  quarto render %s --output-dir $out", qmd_file),
    sep = "\n"
  )

  # Nix snippet
  snippet <- sprintf(
    "  %s = pkgs.stdenv.mkDerivation {\n    name = \"%s\";\n    src = ./.;\n    buildInputs = [ commonBuildInputs pkgs.which pkgs.quarto ];\n    buildPhase = ''\n%s\n    '';\n  };",
    name_str,
    name_str,
    build_phase
  )

  list(name = name_str, snippet = snippet, type = "drv_quarto")
}
