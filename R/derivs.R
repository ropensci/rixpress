#' rxp_r Creates a Nix expression running an R function
#' @param name Symbol, name of the derivation.
#' @param expr R code to generate the expression.
#' @param additional_files Character vector, additional files to include. These
#'   are the files that contain custom functions required for this derivation.
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
rxp_r <- function(
  name,
  expr,
  additional_files = "",
  nix_env = "default.nix"
) {
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

  list(
    name = out_name,
    snippet = snippet,
    type = "rxp_r",
    additional_files = additional_files,
    nix_env = nix_code
  )
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
#' \dontrun{
#'   rxp_quarto(report, "doc.qmd")
#' }
#' @export
rxp_quarto <- function(
  name,
  qmd_file,
  additional_files = "",
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
    additional_files = additional_files,
    nix_env = nix_code
  )
}

#' Create a Nix derivation for Python code execution
#'
#' This function generates a Nix derivation that executes a given Python
#' expression and serializes the result using Python's `pickle` module. The
#' derivation is designed to work with Python environments defined in a
#' `default.nix` file.
#'
#' @param name A symbol representing the name of the variable to be serialized
#'   and the derivation name.
#'   It must be a valid Python variable name (e.g., no dots).
#' @param py_expr A string containing the Python expression to be assigned to
#'   the variable specified by `name`.
#'   The function will execute `<name> = <py_expr>` in Python.
#' @param additional_files Character vector, additional files to include. These
#'   are the files that contain custom functions required for this derivation.
#' @param nix_env The path to the Nix environment file (default: "default.nix").
#'
#' @return A list containing the derivation name, the Nix derivation snippet,
#'   the type of derivation ("rxp_py"), potential additional files,
#'   and the Nix environment setup code.
#'
#' @examples
#' # Generate a derivation that assigns 42 to 'my_result' and pickles it
#' \dontrun{
#'   rxp_py(my_result, "42")
#' }
#' # The generated Python code will be:
#' # exec(open('libraries.py').read()); exec('my_result = 42');
#' # import pickle; with open('my_result.pickle', 'wb') as f:
#' #     pickle.dump(globals()['my_result'], f)
#' @export
rxp_py <- function(
  name,
  py_expr,
  additional_files = "",
  nix_env = "default.nix"
) {
  out_name <- deparse(substitute(name))

  py_expr <- gsub("'", "\\'", py_expr, fixed = TRUE)

  build_phase <- sprintf(
    "python -c \"
exec(open('libraries.py').read())
exec('%s = %s')
with open('%s.pickle', 'wb') as f: pickle.dump(globals()['%s'], f)\"",
    out_name,
    py_expr,
    out_name,
    out_name
  )

  base <- gsub("[^a-zA-Z0-9]", "_", nix_env)
  base <- sub("_nix$", "", base)

  snippet <- sprintf(
    "  %s = makePyDerivation {\n    name = \"%s\";\n    buildInputs = %sBuildInputs;\n    configurePhase = %sConfigurePhase;\n    buildPhase = ''\n      %s\n    '';\n  };",
    out_name,
    out_name,
    base,
    base,
    build_phase
  )

  nix_lines <- c(
    paste0(base, " = import ./", nix_env, ";"),
    paste0(base, "Pkgs = ", base, ".pkgs;"),
    paste0(base, "Shell = ", base, ".shell;"),
    paste0(base, "BuildInputs = ", base, "Shell.buildInputs;"),
    paste0(
      base,
      "ConfigurePhase = ''\n    cp ${./_rixpress/",
      base,
      "_libraries.py} libraries.py\n    mkdir -p $out\n  '';"
    )
  )
  nix_code <- paste(nix_lines, collapse = "\n  ")

  list(
    name = out_name,
    snippet = snippet,
    type = "rxp_py",
    additional_files = additional_files,
    nix_env = nix_code
  )
}

#' rxp_file_common Creates a Nix expression with shared logic for R and Python file reading.
#'
#' @param out_name Character, the name of the derivation as a string.
#' @param path Character, the file path (URL or local).
#' @param nix_env Character, path to the Nix environment file.
#' @param build_phase Character, the language-specific build phase script.
#' @param type Character, the type of derivation ("rxp_r" or "rxp_py").
#' @param derivation_func Character, the Nix derivation function ("makeRDerivation" or "makePyDerivation").
#' @param library_ext Character, the library file extension ("R" or "py").
#' @return A list with `name`, `snippet`, `type`, and `nix_env`.
rxp_file_common <- function(
  out_name,
  path,
  nix_env,
  build_phase,
  type,
  derivation_func,
  library_ext
) {
  # Handle source: URL or local file
  if (grepl("^https?://", path)) {
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
    hash <- trimws(hash[1])
    src_part <- sprintf(
      "defaultPkgs.fetchurl {\n      url = \"%s\";\n      sha256 = \"%s\";\n    }",
      path,
      hash
    )
  } else {
    src_part <- sprintf("./%s", path)
  }

  # Compute base from nix_env
  base <- sub("\\.nix$", "", basename(nix_env))

  # Generate Nix derivation snippet
  snippet <- sprintf(
    "  %s = %s {\n    name = \"%s\";\n    src = %s;\n    buildInputs = %sBuildInputs;\n    configurePhase = %sConfigurePhase;\n    buildPhase = ''\n      %s\n    '';\n  };",
    out_name,
    derivation_func,
    out_name,
    src_part,
    base,
    base,
    build_phase
  )

  # Generate nix_env setup code
  nix_lines <- c(
    paste0(base, " = import ./", nix_env, ";"),
    paste0(base, "Pkgs = ", base, ".pkgs;"),
    paste0(base, "Shell = ", base, ".shell;"),
    paste0(base, "BuildInputs = ", base, "Shell.buildInputs;"),
    paste0(
      base,
      "ConfigurePhase = ''\n    cp ${./_rixpress/",
      base,
      "_libraries.",
      library_ext,
      "} libraries.",
      library_ext,
      "\n    mkdir -p $out\n  '';"
    )
  )

  nix_code <- paste(nix_lines, collapse = "\n  ")

  list(
    name = out_name,
    snippet = snippet,
    type = type,
    additional_files = "",
    nix_env = nix_code
  )
}

#' rxp_r_file Creates a Nix expression that reads in a file using R.
#'
#' @param name Symbol, the name of the derivation.
#' @param path Character, the file path to include, can be a URL.
#' @param read_function Function, an R function to read the data, taking one argument (the path).
#' @param nix_env Character, path to the Nix environment file, default is "default.nix".
#' @details See original documentation for usage notes.
#' @return A list with `name`, `snippet`, `type`, and `nix_env`.
#' @export
rxp_r_file <- function(name, path, read_function, nix_env = "default.nix") {
  out_name <- deparse(substitute(name))
  read_func_str <- deparse(substitute(read_function))
  read_func_str <- gsub("\"", "'", read_func_str)

  build_phase <- sprintf(
    "cp $src input_file
Rscript -e \"
source('libraries.R')
data <- do.call(%s, list('input_file'))
saveRDS(data, '%s.rds')\"",
    read_func_str,
    out_name
  )

  rxp_file_common(
    out_name = out_name,
    path = path,
    nix_env = nix_env,
    build_phase = build_phase,
    type = "rxp_r",
    derivation_func = "makeRDerivation",
    library_ext = "R"
  )
}

#' rxp_py_file Creates a Nix expression that reads in a file using Python.
#'
#' @param name Symbol, the name of the derivation.
#' @param path Character, the file path to include, can be a URL.
#' @param read_function Character, a Python expression string evaluating to a function taking one argument.
#' @param nix_env Character, path to the Nix environment file, default is "default.nix".
#' @details See original documentation for usage notes.
#' @return A list with `name`, `snippet`, `type`, and `nix_env`.
#' @export
rxp_py_file <- function(name, path, read_function, nix_env = "default.nix") {
  out_name <- deparse(substitute(name))

  read_function <- gsub("'", "\\'", read_function, fixed = TRUE)

  if (!is.character(read_function) || length(read_function) != 1) {
    stop("read_function must be a single character string")
  }

  build_phase <- sprintf(
    "cp $src input_file
python -c \"
exec(open('libraries.py').read())
file_path = 'input_file'
data = eval('%s')(file_path)
with open('%s.pickle', 'wb') as f:\n    pickle.dump(data, f)\n\"\n",
    read_function,
    out_name
  )

  rxp_file_common(
    out_name = out_name,
    path = path,
    nix_env = nix_env,
    build_phase = build_phase,
    type = "rxp_py",
    derivation_func = "makePyDerivation",
    library_ext = "py"
  )
}

#result <- unserialize_pkl(
#  name = mtcars_csv,
#  input_derivation = mtcars_pkl,
 # save_code = "to_csv",
#  output_file = "mtcars.csv"
#)
unserialize_pkl <- function(name, input_derivation, save_method, output_file, nix_env = "default.nix") {
  # Convert symbol arguments to strings for derivation names
  out_name <- deparse(substitute(name))
  input_deriv <- deparse(substitute(input_derivation))
  
  # Define the build phase: copy the input .pickle file and run Python code
  build_phase <- sprintf(
    "cp ${%s}/%s.pickle input.pickle\npython -c \"
exec(open('libraries.py').read())
with open('input.pickle', 'rb') as f:\n    obj = pickle.load(f)\n%s.%s('%s')\n\"",
    input_deriv, input_deriv, input_deriv, save_method, output_file
  )
  
  # Derive the base name from nix_env for environment variable names
  base <- gsub("[^a-zA-Z0-9]", "_", nix_env)
  base <- sub("_nix$", "", base)
  
  # Generate the Nix derivation snippet with an overridden installPhase
  snippet <- sprintf(
    "  %s = (makePyDerivation {\n    name = \"%s\";\n    buildInputs = %sBuildInputs;\n    configurePhase = %sConfigurePhase;\n    buildPhase = ''\n%s\n    '';\n  }).overrideAttrs (old: {\n    installPhase = ''\n      cp %s $out\n    '';\n  });",
    out_name, out_name, base, base, build_phase, output_file
  )
  
  # Generate Nix environment setup code
  nix_lines <- c(
    paste0(base, " = import ./", nix_env, ";"),
    paste0(base, "Pkgs = ", base, ".pkgs;"),
    paste0(base, "Shell = ", base, ".shell;"),
    paste0(base, "BuildInputs = ", base, "Shell.buildInputs;"),
    paste0(
      base,
      "ConfigurePhase = ''\n    cp ${./_rixpress/",
      base,
      "_libraries.py} libraries.py\n    mkdir -p $out\n  '';"
    )
  )
  nix_code <- paste(nix_lines, collapse = "\n  ")
  
  # Return the result as a structured list
  list(
    name = out_name,
    snippet = snippet,
    type = "unserialize_pkl",
    additional_files = "",
    nix_env = nix_code
  )
}
