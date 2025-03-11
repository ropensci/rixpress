#' rxp_r Creates a Nix expression running an R function
#' @param output Character, a variable name to save the
#'   output of the function expression
#' @param name Symbol, name of the derivation.
#' @param expr R code to generate the expression.
#' @details At a basic level, `rxp_r(mtcars_am, filter(mtcars, am == 1))`
#'   is equivalent to `mtcars <- filter(mtcars, am == 1)`. `rxp_r()` generates
#'   the required Nix boilerplate to output a so-called "derivation" in Nix
#'   jargon. A Nix derivation is a recipe that defines how to create an output
#'   (in this case `mtcars_am`) including its dependencies, build steps,
#'   and output paths.
#' @return A list of two elements, `name`, the `name` of the derivation,
#'   and `snippet` the Nix boilerplate code.
#' @examples rxp_r(mtcars_am, filter(mtcars, am == 1))
#' @export
rxp_r <- function(name, expr) {
  out_name <- deparse(substitute(name))
  expr_str <- deparse(substitute(expr))
  # Replace " by ' or else Nix complains
  expr_str <- gsub("\"", "'", expr_str)

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

  list(name = out_name, snippet = snippet, type = "rxp_r")
}

#' Render a Quarto document as a Nix derivation
#'
#' @param name Symbol, derivation name.
#' @param qmd_file Character, path to .qmd file.
#'
#' @details Detects `rxp_read("ref")` in the .qmd file and replaces with derivation output paths.
#'
#' @return List with `name` (string) and `snippet` (Nix code).
#'
#' @examples
#' rxp_quarto(report, "doc.qmd")
#'
#' @export
rxp_quarto <- function(name, qmd_file) {
  out_name <- deparse(substitute(name))

  # Parse qmd file for refs
  content <- readLines(qmd_file, warn = FALSE)
  content_str <- paste(content, collapse = "\n")
  matches <- gregexpr('rxp_read\\("([^"]+)"\\)', content_str)
  refs <- regmatches(content_str, matches)[[1]]
  refs <- sub('rxp_read\\("([^"]+)"\\)', '\\1', refs)
  refs <- unique(refs)

  # Substitution commands
  sub_cmds <- sapply(refs, function(ref) {
    sprintf(
      "substituteInPlace %s --replace-fail 'rxp_read(\"%s\")' 'rxp_read(\"${%s}/%s.rds\")'",
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
    "  %s = pkgs.stdenv.mkDerivation {\n    name = \"%s\";\n    src = pkgs.lib.cleanSource ./.;\n    buildInputs = [ commonBuildInputs pkgs.which pkgs.quarto ];\n    buildPhase = ''\n%s\n    '';\n  };",
    out_name,
    out_name,
    build_phase
  )

  list(name = out_name, snippet = snippet, type = "rxp_quarto")
}

#' rxp_file Creates a Nix expression that reads in a file.
#'
#' @param name Symbol, the name of the derivation.
#' @param path Character, the file path to include.
#' @param read_function Function, must be a function of only one
#'   argument, the path. If you wish to pass several arguments
#'   to this function, make it an anonymous function. See @details.
#'
#' @details The function must only take one single argument, the
#'   path to the file to read. Because of this limitation, if
#'   you need to pass more than one argument to the function,
#'   make it an anonymous function, for example:
#'   `d0 <- rxp_file(mtcars, 'mtcars.csv', \(x) (read.csv(file = x, sep = "|")))`
#'
#' @return A list with three elements: `name`, the derivation name,
#'   and `snippet`, the generated Nix code and the derivation type.
#'
#' @examples
#' \dontrun{
#' rxp_file(data, "./data.csv", read.csv)
#' }
#'
#' @export
rxp_file <- function(name, path, read_function) {
  out_name <- deparse(substitute(name))
  read_func_str <- deparse(substitute(read_function))
  # Replace " by ' or else Nix complains
  read_func_str <- gsub("\"", "'", read_func_str)

  build_phase <- sprintf(
    "cp $src input_file\nRscript -e \"\n        source('libraries.R')\n        data <- do.call(%s, list('input_file'))\n        saveRDS(data, '%s.rds')\"",
    read_func_str,
    out_name
  )

  snippet <- sprintf(
    "  %s = makeRDerivation {\n    name = \"%s\";\n    src = ./%s;\n    buildPhase = ''\n      %s\n    '';\n  };",
    out_name,
    out_name,
    path,
    build_phase
  )

  list(name = out_name, snippet = snippet, type = "rxp_r")
}
