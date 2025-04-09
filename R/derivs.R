#' rxp_r Creates a Nix expression running an R function
#' @param name Symbol, name of the derivation.
#' @param expr R code to generate the expression.
#' @param additional_files Character vector, additional files to include. Custom
#'   functions must go into a script called "functions.R", and additional files
#'   that need to be accessible during the build process can be named anything.
#' @param nix_env Character, path to the Nix environment file, default is "default.nix".
#' @details At a basic level, `rxp_r(mtcars_am, filter(mtcars, am == 1))`
#'   is equivalent to `mtcars <- filter(mtcars, am == 1)`. `rxp_r()` generates
#'   the required Nix boilerplate to output a so-called "derivation" in Nix
#'   jargon. A Nix derivation is a recipe that defines how to create an output
#'   (in this case `mtcars_am`) including its dependencies, build steps,
#'   and output paths.
#' @return A list with elements: `name`, the `name` of the derivation,
#'   `snippet`, the Nix boilerplate code, `type`, `additional_files` and `nix_env`.
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

  # Prepare the fileset for src
  # Remove functions.R as this is handled separately
  fileset_parts <- setdiff(additional_files, "functions.R")
  if (identical(fileset_parts, character(0)) || fileset_parts == "") {
    src_snippet <- ""
  } else {
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
    src_snippet <- sprintf(
      "   src = defaultPkgs.lib.fileset.toSource {\n      root = ./.;\n      fileset = defaultPkgs.lib.fileset.unions [ %s ];\n    };\n ",
      fileset_nix
    )
  }

  # Updated snippet with buildInputs and configurePhase
  snippet <- sprintf(
    "  %s = makeRDerivation {\n    name = \"%s\";\n  %s  buildInputs = %sBuildInputs;\n    configurePhase = %sConfigurePhase;\n    buildPhase = ''\n      %s\n    '';\n  };",
    out_name,
    out_name,
    src_snippet,
    base,
    base,
    build_phase
  )

  list(
    name = out_name,
    snippet = snippet,
    type = "rxp_r",
    additional_files = additional_files,
    nix_env = nix_env
  )
}

#' Render a Quarto document as a Nix derivation
#'
#' @param name Symbol, derivation name.
#' @param qmd_file Character, path to .qmd file.
#' @param additional_files Character vector, additional files to include.
#' @param nix_env Character, path to the Nix environment file, default is "default.nix".
#' @param args A character of additional arguments to be passed directly to
#'   the `quarto` command.
#' @details Detects `rxp_read("ref")` in the .qmd file and replaces with
#'   derivation output paths.
#' @return A list with elements: `name`, the `name` of the derivation,
#'   `snippet`, the Nix boilerplate code, `type`, `additional_files` and `nix_env`.
#' @examples
#' \dontrun{
#'   # `images` is a folder containing images to include in the Quarto doc
#'   rxp_quarto(name = report, qmd_file = "doc.qmd", additional_files = "images", args = "-- to typst")
#' }
#' @export
rxp_quarto <- function(
  name,
  qmd_file,
  additional_files = "",
  nix_env = "default.nix",
  args = ""
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
      "substituteInPlace %s --replace-fail 'rxp_read(\"%s\")' 'rxp_read(\"${%s}\")'",
      qmd_file,
      ref,
      ref
    )
  })

  build_phase <- paste(
    "  mkdir home",
    "  export HOME=$PWD/home",
    "  export RETICULATE_PYTHON='${defaultPkgs.python3}/bin/python'\n",
    if (length(sub_cmds) > 0)
      paste("  ", sub_cmds, sep = "", collapse = "\n") else "",
    sprintf("  quarto render %s %s --output-dir $out", qmd_file, args),
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

  # Derive base from nix_env
  base <- gsub("[^a-zA-Z0-9]", "_", nix_env)
  base <- sub("_nix$", "", base)

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

  # Return the result as a list
  list(
    name = out_name,
    snippet = snippet,
    type = "rxp_quarto",
    qmd_file = qmd_file,
    additional_files = additional_files,
    nix_env = nix_env
  )
}

#' rxp_py Creates a Nix expression running a Python function
#' @param name Symbol, name of the derivation.
#' @param expr R code to generate the expression.
#' @param additional_files Character vector, additional files to include. Custom
#'   functions must go into a script called "functions.R", and additional files
#'   that need to be accessible during the build process can be named anything.
#' @param nix_env Character, path to the Nix environment file, default is "default.nix".
#' @details At a basic level,
#'   `rxp_py(mtcars_am, "mtcars.filter(polars.col('am') == 1)")`
#'   is equivalent to `mtcars_am = mtcars.filter(polars.col('am') == 1).
#'   `rxp_py()` generates the required Nix boilerplate to output a
#'   so-called "derivation" in Nix jargon. A Nix derivation is a recipe
#'   that defines how to create an output (in this case `mtcars_am`)
#'   including its dependencies, build steps, and output paths.
#' @return A list with elements: `name`, the `name` of the derivation,
#'   `snippet`, the Nix boilerplate code, `type`, `additional_files` and `nix_env`.
#' @examples rxp_py(mtcars_pl_am, py_expr = "mtcars_pl.filter(polars.col('am') == 1).to_pandas()")
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

  # Prepare the fileset for src
  # Remove functions.py as this is handled separately
  fileset_parts <- setdiff(additional_files, "functions.py")
  if (identical(fileset_parts, character(0)) || fileset_parts == "") {
    src_snippet <- ""
  } else {
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
    src_snippet <- sprintf(
      "   src = defaultPkgs.lib.fileset.toSource {\n      root = ./.;\n      fileset = defaultPkgs.lib.fileset.unions [ %s ];\n    };\n ",
      fileset_nix
    )
  }

  snippet <- sprintf(
    "  %s = makePyDerivation {\n    name = \"%s\";\n  %s  buildInputs = %sBuildInputs;\n    configurePhase = %sConfigurePhase;\n    buildPhase = ''\n      %s\n    '';\n  };",
    out_name,
    out_name,
    src_snippet,
    base,
    base,
    build_phase
  )

  list(
    name = out_name,
    snippet = snippet,
    type = "rxp_py",
    additional_files = additional_files,
    nix_env = nix_env
  )
}

#' rxp_file_common Creates a Nix expression with shared logic for R and Python file reading.
#'
#' @param out_name Character, the name of the derivation as a string.
#' @param path Character, the file path (URL or local) or folder path if copying a whole folder.
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
  # Handle source: URL or local file/folder
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

  base <- gsub("[^a-zA-Z0-9]", "_", nix_env)
  base <- sub("_nix$", "", base)

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

  list(
    name = out_name,
    snippet = snippet,
    type = type,
    additional_files = "",
    nix_env = nix_env
  )
}

#' rxp_r_file Creates a Nix expression that reads in a file using R.
#'
#' @param name Symbol, the name of the derivation.
#' @param path Character, the file path to include, can be a URL.
#' @param read_function Function, an R function to read the data, taking one argument (the path).
#' @param nix_env Character, path to the Nix environment file, default is "default.nix".
#' @param copy_data_folder Logical, if TRUE then the entire folder is copied recursively.
#' @details If copy_data_folder is TRUE, the build phase copies the folder recursively using `cp -r`
#'          and uses the file's basename (e.g. "oceans.shp") from the folder rather than a direct file copy.
#'          The source passed to the derivation becomes the folder containing the data.
#' @return A list with `name`, `snippet`, `type`, and `nix_env`.
#' @export
rxp_r_file <- function(name, path, read_function, nix_env = "default.nix", copy_data_folder = FALSE) {
  out_name <- deparse(substitute(name))
  read_func_str <- deparse(substitute(read_function))
  read_func_str <- gsub("\"", "'", read_func_str)

  if (!copy_data_folder) {
    # Original behavior: copy a single file.
    build_phase <- sprintf(
      "cp $src input_file\nRscript -e \"\nsource('libraries.R')\ndata <- do.call(%s, list('input_file'))\nsaveRDS(data, '%s.rds')\"",
      read_func_str,
      out_name
    )
    actual_path <- path
  } else {
    # New behavior: copy the whole folder.
    file_name <- basename(path)
    build_phase <- sprintf(
      "cp -r $src input_folder\nRscript -e \"\nsource('libraries.R')\ndata <- do.call(%s, list('input_folder/%s'))\nsaveRDS(data, '%s.rds')\"",
      read_func_str,
      file_name,
      out_name
    )
    actual_path <- dirname(path)
  }

  rxp_file_common(
    out_name = out_name,
    path = actual_path,
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
#' @param copy_data_folder Logical, if TRUE then the entire folder is copied recursively.
#' @details If copy_data_folder is TRUE, the build phase copies the folder recursively using `cp -r`
#'          and uses the file's basename (e.g. "oceans.shp") within the input folder.
#'          The source passed to the derivation becomes the folder containing the data.
#' @return A list with `name`, `snippet`, `type`, and `nix_env`.
#' @export
rxp_py_file <- function(name, path, read_function, nix_env = "default.nix", copy_data_folder = FALSE) {
  out_name <- deparse(substitute(name))

  # Sanitize read_function string.
  read_function <- gsub("'", "\\'", read_function, fixed = TRUE)
  if (!is.character(read_function) || length(read_function) != 1) {
    stop("read_function must be a single character string")
  }

  if (!copy_data_folder) {
    # Original behavior: copy a single file.
    build_phase <- sprintf(
      "cp $src input_file\npython -c \"\nexec(open('libraries.py').read())\nfile_path = 'input_file'\ndata = eval('%s')(file_path)\nwith open('%s.pickle', 'wb') as f:\n    pickle.dump(data, f)\n\"\n",
      read_function,
      out_name
    )
    actual_path <- path
  } else {
    # New behavior: copy the whole folder.
    file_name <- basename(path)
    build_phase <- sprintf(
      "cp -r $src input_folder\npython -c \"\nexec(open('libraries.py').read())\nfile_path = 'input_folder/%s'\ndata = eval('%s')(file_path)\nwith open('%s.pickle', 'wb') as f:\n    pickle.dump(data, f)\n\"\n",
      file_name,
      read_function,
      out_name
    )
    actual_path <- dirname(path)
  }

  rxp_file_common(
    out_name = out_name,
    path = actual_path,
    nix_env = nix_env,
    build_phase = build_phase,
    type = "rxp_py",
    derivation_func = "makePyDerivation",
    library_ext = "py"
  )
}


#' Generate the Nix derivation snippet for Python-R object transfer.
#'
#' This function constructs the `build_phase` and Nix derivation snippet
#' based on the given parameters.
#'
#' @param out_name Character, name of the derivation.
#' @param expr_str Character, name of the object being transferred.
#' @param nix_env Character, path to the Nix environment file.
#' @param direction Character, either "py2r" (Python to R) or "r2py" (R to Python).
#' @return A list with elements: `name`, `snippet`, `type`, `additional_files`, `nix_env`.
rxp_common_setup <- function(out_name, expr_str, nix_env, direction) {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop(
      "The 'reticulate' package is required to convert between Python and R objects.\nPlease install it to use these functions."
    )
  }

  expr_str <- gsub("\"", "'", expr_str) # Replace " with ' for Nix
  base <- gsub("[^a-zA-Z0-9]", "_", nix_env)
  base <- sub("_nix$", "", base)

  if (direction == "py2r") {
    r_command <- sprintf(
      "         %s <- reticulate::py_load_object('${%s}/%s.pickle', pickle = 'pickle', convert = TRUE)\n         saveRDS(%s, '%s.rds')",
      out_name,
      expr_str,
      expr_str,
      out_name,
      out_name
    )
  } else if (direction == "r2py") {
    r_command <- sprintf(
      "         %s <- readRDS('${%s}/%s.rds')\n         reticulate::py_save_object(%s, '%s.pickle', pickle = 'pickle')",
      expr_str,
      expr_str,
      expr_str,
      expr_str,
      out_name
    )
  } else {
    stop("Invalid direction. Use 'py2r' or 'r2py'.")
  }

  build_phase <- sprintf(
    "export RETICULATE_PYTHON='${defaultPkgs.python3}/bin/python'\n       Rscript -e \"\n         source('libraries.R')\n%s\"",
    r_command
  )

  snippet <- sprintf(
    "  %s = makeRDerivation {\n    name = \"%s\";\n    buildInputs = %sBuildInputs;\n    configurePhase = %sConfigurePhase;\n    buildPhase = ''\n      %s\n    '';\n  };",
    out_name,
    out_name,
    base,
    base,
    build_phase
  )

  list(
    name = out_name,
    snippet = snippet,
    type = paste0("rxp_", direction),
    additional_files = "",
    nix_env = nix_env
  )
}


#' Transfer Python object into an R session.
#'
#' @param name Symbol, name of the derivation.
#' @param expr Symbol, Python object to be loaded into R.
#' @param nix_env Character, path to the Nix environment file, default is "default.nix".
#' @details `rxp_py2r(my_obj, my_python_object)` loads a serialized
#'   Python object and saves it as an RDS file using `reticulate::py_load_object()`.
#' @return A list with elements: `name`, the name of the derivation,
#'   `snippet`, the Nix boilerplate code, `type`, `additional_files`
#'   (for compatibility reasons only) and `nix_env`.
#' @examples
#' \dontrun{
#'   rxp_py2r(my_obj, my_python_object)
#' }
#' @export
rxp_py2r <- function(name, expr, nix_env = "default.nix") {
  out_name <- deparse(substitute(name))
  expr_str <- deparse(substitute(expr))
  rxp_common_setup(out_name, expr_str, nix_env, "py2r")
}

#' Transfer R object into a Python session.
#'
#' @param name Symbol, name of the derivation.
#' @param expr Symbol, R object to be saved into a Python pickle.
#' @param nix_env Character, path to the Nix environment file, default is "default.nix".
#' @details `rxp_r2py(my_obj, my_r_object)` saves an R object to a Python pickle
#'   using `reticulate::py_save_object()`.
#' @return A list with elements: `name`, the name of the derivation,
#'   `snippet`, the Nix boilerplate code, `type`, `additional_files`
#'   (for compatibility reasons only) and `nix_env`.
#' @examples
#' \dontrun{
#'   rxp_r2py(my_obj, my_r_object)
#' }
#' @export
rxp_r2py <- function(name, expr, nix_env = "default.nix") {
  out_name <- deparse(substitute(name))
  expr_str <- deparse(substitute(expr))
  rxp_common_setup(out_name, expr_str, nix_env, "r2py")
}
