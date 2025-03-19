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

#' Create a Nix derivation for Python code execution
#'
#' This function generates a Nix derivation that executes a given Python
#' expression and serializes the result using Python's `pickle` module. The
#' derivation is designed to work with Python environments defined in a
#' `default.nix` file.
#'
#' @param name A symbol representing the name of the variable to be serialized
#'   and the derivation name. It must be a valid Python variable name (e.g., no dots).
#' @param py_expr A string containing the Python expression to be assigned to
#'   the variable specified by `name`. The function will execute `<name> = <py_expr>`
#'   in Python.
#' @param nix_env The path to the Nix environment file (default: "default.nix").
#'   This file should define the Python packages required for the derivation.
#'
#' @return A list containing the derivation name, the Nix derivation snippet,
#'   the type of derivation ("rxp_py"), and the Nix environment setup code.
#'
#' @examples
#' # Generate a derivation that assigns 42 to 'my_result' and pickles it
#' rxp_py(my_result, "42")
#' # The generated Python code will be:
#' # exec(open('libraries.py').read()); exec('my_result = 42');
#' # import pickle; with open('my_result.pickle', 'wb') as f:
#' #     pickle.dump(globals()['my_result'], f)
rxp_py <- function(name, py_expr, nix_env = "default.nix") {
  out_name <- deparse(substitute(name))

  build_phase <- sprintf(
    "python -c \"
exec(open('libraries.py').read())
exec('''%s''')
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

  list(name = out_name, snippet = snippet, type = "rxp_py", nix_env = nix_code)
}

#' rxp_py_file Creates a Nix expression that reads in a file using Python.
#'
#' This function generates a Nix derivation that reads a file using a specified
#' Python function and serializes the result using Python's `pickle` module.
#' It is designed to work within an R environment but targets Python execution
#'
#' @param name Symbol, the name of the derivation and the output variable.
#' @param path Character, the file path to include, which can be a local path or a URL.
#' @param read_function Character, a string that evaluates to a Python function
#'   taking one argument, the path to the file. Examples include `"pandas.read_csv"`
#'   for simple function calls or `"lambda x: pandas.read_csv(x, sep=\"|\")"` for
#'   functions requiring additional arguments.
#' @param nix_env Character, path to the Nix environment file, defaults to `"default.nix"`.
#'   This file should define the Python environment and dependencies.
#'
#' @details 
#' The `read_function` parameter must be a string that, when evaluated in Python,
#' returns a callable function accepting a single argument (the file path).
#' For example:
#' - `"pandas.read_csv"` evaluates to the `pandas.read_csv` function.
#' - `"lambda x: pandas.read_csv(x, sep=\"|\")"` evaluates to a lambda function that
#'   calls `pandas.read_csv` with a custom separator.
#' 
#' Use double quotes (`"`) for strings within `read_function` to avoid conflicts
#' with the single quotes used in the Python execution string.
#'
#' @return A list containing:
#'   - `name`: The derivation name as a string.
#'   - `snippet`: The generated Nix derivation code.
#'   - `type`: Set to `"rxp_py"` to indicate a Python derivation.
#'   - `nix_env`: The Nix environment setup code.
#'
#' @examples
#' \dontrun{
#' # Read a CSV file with pandas
#' rxp_py_file(data, "./data.csv", "pandas.read_csv")
#' 
#' # Read a CSV file with a custom separator
#' rxp_py_file(data, "https://example.com/data.csv", "lambda x: pd.read_csv(x, sep=\"|\")")
#' }
#'
#' @export
rxp_py_file <- function(name, path, read_function, nix_env = "default.nix") {
  out_name <- deparse(substitute(name))

  # Validate that read_function is a single character string
  if (!is.character(read_function) || length(read_function) != 1) {
    stop("read_function must be a single character string")
  }

  # No need to escape quotes with triple quotes in use

  # Determine source: local file or URL
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
  
  # Define the build phase with triple quotes
  build_phase <- sprintf(
    "cp $src input_file
python -c \"
exec(open('libraries.py').read())
file_path = 'input_file'
data = eval('''%s''')(file_path)
import pickle
import os
out_dir = os.environ['out']
with open(os.path.join(out_dir, '%s.pickle'), 'wb') as f:\n    pickle.dump(data, f)\n\"\n",
    read_function,
    out_name
  )
  
  # Extract base name from nix_env
  base <- sub("\\.nix$", "", basename(nix_env))
  
  # Generate the Nix derivation snippet
  snippet <- sprintf(
    "%s = makePyDerivation {\n    name = \"%s\";\n    src = %s;\n    buildInputs = %sBuildInputs;\n    configurePhase = %sConfigurePhase;\n    buildPhase = ''\n      %s\n    '';\n  };",
    out_name,
    out_name,
    src_part,
    base,
    base,
    build_phase
  )
  
  # Generate the nix_env setup code
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
  
  # Return the result as a list
  list(
    name = out_name,
    snippet = snippet,
    type = "rxp_py",
    nix_env = nix_code
  )
}
