#' rxp_r Creates a Nix expression running an R function
#' @param output Character, a variable name to save the
#'   output of the function expression
#' @param name Symbol, name of the derivation.
#' @param expr R code to generate the expression.
#' @param nix_env Character, path to the Nix environment file, default is "default.nix".
#' @details At a basic level, `rxp_r(mtcars_am, filter(mtcars, am == 1))`
#'   is equivalent to `mtcars <- filter(mtcars, am == 1)`. `rxp_r()` generates
#'   the required Nix boilerplate to output a so-called "derivation" in Nix
#'   jargon. A Nix derivation is a recipe that defines how to create an output
#'   (in this case `mtcars_am`) including its dependencies, build steps,
#'   and output paths.
#' @return A list with elements: `name`, the `name` of the derivation,
#'   `snippet`, the Nix boilerplate code, `type`, and `nix_env`.
#' @examples rxp_r(mtcars_am, filter(mtcars, am == 1))
#' @export
rxp_r <- function(name, expr, nix_env = "default.nix") {
  out_name <- deparse(substitute(name))
  expr_str <- deparse(substitute(expr))
  expr_str <- gsub("\"", "'", expr_str) # Replace " with ' for Nix

  build_phase <- sprintf(
    "Rscript -e \"\n        source('libraries.R')\n        %s <- %s\n        saveRDS(%s, '%s.rds')\"",
    out_name,
    expr_str,
    out_name,
    out_name
  )

  # Derive base from nix_env
  base <- gsub("[^a-zA-Z0-9]", "_", nix_env)
  base <- sub("_nix$", "", base)

  # Updated snippet with buildInputs and configurePhase
  snippet <- sprintf(
    "  %s = makeRDerivation {\n    name = \"%s\";\n    buildInputs = %sBuildInputs;\n    configurePhase = %sConfigurePhase;\n    buildPhase = ''\n      %s\n    '';\n  };",
    out_name,
    out_name,
    base,
    base,
    build_phase
  )

  # nix_env code
  nix_lines <- c(
    paste0(base, " = import ./", nix_env, ";"),
    paste0(base, "Pkgs = ", base, ".pkgs;"),
    paste0(base, "Shell = ", base, ".shell;"),
    paste0(base, "BuildInputs = ", base, "Shell.buildInputs;"),
    paste0(
      base,
      "ConfigurePhase = ''\n    cp ${./_rixpress/",
      base,
      "_libraries.R} libraries.R\n    mkdir -p $out\n  '';"
    )
  )
  nix_code <- paste(nix_lines, collapse = "\n  ")

  list(name = out_name, snippet = snippet, type = "rxp_r", nix_env = nix_code)
}

#' Render a Quarto document as a Nix derivation
#'
#' @param name Symbol, derivation name.
#' @param qmd_file Character, path to .qmd file.
#' @param additional_files Character vector, additional files to include.
#' @param nix_env Character, path to the Nix environment file, default is "default.nix".
#' @details Detects `rxp_read("ref")` in the .qmd file and replaces with derivation output paths.
#' @return List with `name` (string), `snippet` (Nix code), `type`, and `nix_env`.
#' @examples
#' rxp_quarto(report, "doc.qmd")
#' @export
rxp_quarto <- function(
  name,
  qmd_file,
  additional_files = character(0),
  nix_env = "default.nix"
) {
  out_name <- deparse(substitute(name))

  content <- readLines(qmd_file, warn = FALSE)
  content_str <- paste(content, collapse = "\n")

  # Extract unique rxp_read references
  matches <- gregexpr('rxp_read\\("([^"]+)"\\)', content_str)
  refs <- regmatches(content_str, matches)[[1]]
  refs <- sub('rxp_read\\("([^"]+)"\\)', '\\1', refs)
  refs <- unique(refs)

  # Generate substitution commands for each reference
  sub_cmds <- sapply(refs, function(ref) {
    sprintf(
      "substituteInPlace %s --replace-fail 'rxp_read(\"%s\")' 'rxp_read(\"${%s}/%s.rds\")'",
      qmd_file,
      ref,
      ref,
      ref
    )
  })

  build_phase <- paste(
    "  mkdir home",
    "  export HOME=$PWD/home",
    if (length(sub_cmds) > 0)
      paste("  ", sub_cmds, sep = "", collapse = "\n") else "",
    sprintf("  quarto render %s --output-dir $out", qmd_file),
    sep = "\n"
  )

  # Prepare the fileset for src
  fileset_parts <- c(qmd_file, additional_files)
  fileset_nix <- paste(
    sapply(fileset_parts, function(part) {
      if (dir.exists(part)) {
        sprintf("./%s", part)
      } else {
        sprintf("./%s", part)
      }
    }),
    collapse = " "
  )

  # Compute base from nix_env
  base <- sub("\\.nix$", "", basename(nix_env))

  # Generate the Nix derivation snippet with updated buildInputs and configurePhase
  snippet <- sprintf(
    "  %s = defaultPkgs.stdenv.mkDerivation {\n    name = \"%s\";\n    src = defaultPkgs.lib.fileset.toSource {\n      root = ./.;\n      fileset = defaultPkgs.lib.fileset.unions [ %s ];\n    };\n    buildInputs = %sBuildInputs;\n    configurePhase = %sConfigurePhase;\n    buildPhase = ''\n%s\n    '';\n  };",
    out_name,
    out_name,
    fileset_nix,
    base,
    base,
    build_phase
  )

  # Generate nix_env code
  nix_lines <- c(
    paste0(base, " = import ./", nix_env, ";"),
    paste0(base, "Pkgs = ", base, ".pkgs;"),
    paste0(base, "Shell = ", base, ".shell;"),
    paste0(base, "BuildInputs = ", base, "Shell.buildInputs;"),
    paste0(
      base,
      "ConfigurePhase = ''\n    cp ${./_rixpress/",
      base,
      "_libraries.R} libraries.R\n    mkdir -p $out\n  '';"
    )
  )
  nix_code <- paste(nix_lines, collapse = "\n  ")

  # Return the result as a list
  list(
    name = out_name,
    snippet = snippet,
    type = "rxp_quarto",
    nix_env = nix_code
  )
}
#' rxp_file Creates a Nix expression that reads in a file.
#'
#' @param name Symbol, the name of the derivation.
#' @param path Character, the file path to include, can be an url.
#' @param read_function Function to read in the data, must be a
#'   function of only one argument, the path. If you wish to pass
#'   several arguments to this function, make it an anonymous function.
#' @param nix_env Character, path to the Nix environment file, default is "default.nix".
#' @details The function must only take one single argument, the
#'   path to the file to read. Because of this limitation, if
#'   you need to pass more than one argument to the function,
#'   make it an anonymous function, for example:
#'   `d0 <- rxp_file(mtcars, 'mtcars.csv', \(x) (read.csv(file = x, sep = "|")))`
#' @return A list with elements: `name`, the derivation name,
#'   `snippet`, the generated Nix code, `type`, and `nix_env`.
#' @examples
#' \dontrun{
#' rxp_file(data, "./data.csv", read.csv)
#' }
#' @export
rxp_file <- function(name, path, read_function, nix_env = "default.nix") {
  out_name <- deparse(substitute(name))
  read_func_str <- deparse(substitute(read_function))
  # Replace double quotes with single quotes for Nix compatibility
  read_func_str <- gsub("\"", "'", read_func_str)

  # Define the build phase for the Nix derivation
  build_phase <- sprintf(
    "cp $src input_file\nRscript -e \"\n        source('libraries.R')\n        data <- do.call(%s, list('input_file'))\n        saveRDS(data, '%s.rds')\"",
    read_func_str,
    out_name
  )

  # Handle the source (src) part based on whether path is a URL or local file
  if (grepl("^https?://", path)) {
    # If path is a URL, use nix-prefetch-url to get the hash
    hash <- tryCatch(
      {
        system(paste("nix-prefetch-url", shQuote(path)), intern = TRUE)
      },
      error = function(e) {
        stop("Failed to run nix-prefetch-url: ", e$message)
      }
    )
    if (length(hash) == 0 || hash == "") {
      stop("nix-prefetch-url did not return a hash for URL: ", path)
    }
    hash <- trimws(hash[1]) # Take the first line and trim whitespace
    # Generate the fetchurl expression with the URL and computed hash
    src_part <- sprintf(
      "defaultPkgs.fetchurl {\n      url = \"%s\";\n      sha256 = \"%s\";\n    }",
      path,
      hash
    )
  } else {
    # If path is a local file, use it directly as the source
    src_part <- sprintf("./%s", path)
  }

  # Compute base from nix_env for environment-specific attributes
  base <- sub("\\.nix$", "", basename(nix_env))

  # Generate the Nix derivation snippet with buildInputs and configurePhase
  snippet <- sprintf(
    "  %s = makeRDerivation {\n    name = \"%s\";\n    src = %s;\n    buildInputs = %sBuildInputs;\n    configurePhase = %sConfigurePhase;\n    buildPhase = ''\n      %s\n    '';\n  };",
    out_name,
    out_name,
    src_part,
    base,
    base,
    build_phase
  )

  # Generate nix_env code
  nix_lines <- c(
    paste0(base, " = import ./", nix_env, ";"),
    paste0(base, "Pkgs = ", base, ".pkgs;"),
    paste0(base, "Shell = ", base, ".shell;"),
    paste0(base, "BuildInputs = ", base, "Shell.buildInputs;"),
    paste0(
      base,
      "ConfigurePhase = ''\n    cp ${./_rixpress/",
      base,
      "_libraries.R} libraries.R\n    mkdir -p $out\n  '';"
    )
  )
  nix_code <- paste(nix_lines, collapse = "\n  ")

  list(
    name = out_name,
    snippet = snippet,
    type = "rxp_r",
    nix_env = nix_code
  )
}
