#' Create a Nix expression running an R function
#' @family derivations
#' @param name Symbol, name of the derivation.
#' @param expr R code to generate the expression.
#' @param additional_files Character vector, additional files to include. Custom
#'   functions must go into a script called "functions.R", and additional files
#'   that need to be accessible during the build process can be named anything.
#' @param nix_env Character, path to the Nix environment file, default is
#'   "default.nix".
#' @param serialize_function Function, defaults to NULL. A function used to
#'   serialize objects for transfer between derivations. It must accept two
#'   arguments: the object to serialize (first), and the target file path
#'   (second). If your function has a different signature, wrap it to match this
#'   interface. By default, `saveRDS()` is used, but this may yield unexpected
#'   results, especially for complex objects like machine learning models. For
#'   instance, for `{keras}` models, use `keras::save_model_hdf5()` to capture
#'   the full model (architecture, weights, training config, optimizer state,
#'   etc.).
#' @param unserialize_function Function, defaults to NULL. A function used to
#'   unserialize objects transferred between derivations. By default,
#'   `readRDS()` is used, but this may produce unexpected results with complex
#'   objects like machine learning models. For example, if the parent derivation
#'   used `keras::save_model_hdf5()` to serialize a model, this derivation
#'   should use `keras::load_model_hdf5()` to load it correctly.
#' @param env_var Character vector, defaults to NULL. A named vector of
#'   environment variables to set before running the R script, e.g.,
#'   `c("CMDSTAN" = "${defaultPkgs.cmdstan}/opt/cmdstan)"`.
#'   Each entry will be added as an export statement in the build phase.
#' @details At a basic level, `rxp_r(mtcars_am, filter(mtcars, am == 1))` is
#'   equivalent to `mtcars_am <- filter(mtcars, am == 1)`. `rxp_r()` generates the
#'   required Nix boilerplate to output a so-called "derivation" in Nix jargon.
#'   A Nix derivation is a recipe that defines how to create an output (in this
#'   case `mtcars_am`) including its dependencies, build steps, and output
#'   paths.
#' @return An object of class derivation which inherits from lists.
#' @examples \dontrun{
#'   # Basic usage
#'   rxp_r(name = filtered_mtcars, expr = filter(mtcars, am == 1))
#'
#'   # Serialize object using qs
#'   rxp_r(
#'    name = filtered_mtcars,
#'    expr = filter(mtcars, am == 1),
#'    serialize_function = qs::qsave
#'   )
#'   # Unerialize using qs::qread in the next derivation
#'   rxp_r(
#'    name = mtcars_mpg,
#'    expr = select(filtered_mtcars, mpg),
#'    unserialize_function = qs::qread
#'   )
#' }
#' @export
rxp_r <- function(
  name,
  expr,
  additional_files = "",
  nix_env = "default.nix",
  serialize_function = NULL,
  unserialize_function = NULL,
  env_var = NULL
) {
  out_name <- deparse1(substitute(name))
  expr_str <- deparse1(substitute(expr))
  expr_str <- gsub("\"", "'", expr_str) # Replace " with ' for Nix
  expr_str <- gsub("$", "\\$", expr_str, fixed = TRUE) # Replace $ with \$ for Nix

  if (is.null(serialize_function)) {
    serialize_str <- "saveRDS"
  } else {
    serialize_str <- deparse1(substitute(serialize_function))
  }

  if (is.null(unserialize_function)) {
    unserialize_str <- "readRDS"
  } else {
    unserialize_str <- deparse1(substitute(unserialize_function))
  }

  # Generate environment variable export statements if env_var is provided
  env_exports <- ""
  if (!is.null(env_var)) {
    env_exports <- paste(
      vapply(
        names(env_var),
        function(var_name)
          sprintf("export %s=%s", var_name, env_var[[var_name]]),
        character(1)
      ),
      collapse = "\n      "
    )
    if (env_exports != "") {
      env_exports <- paste0(env_exports, "\n      ")
    }
  }

  # Prepare the fileset for src
  # Remove functions.R as this is handled separately
  fileset_parts <- setdiff(additional_files, "functions.R")
  fileset_parts <- fileset_parts[nzchar(fileset_parts)]

  # build copy command for additional files
  copy_cmd <- ""
  if (length(fileset_parts) > 0) {
    copy_lines <- vapply(
      fileset_parts,
      function(f) sprintf("cp -r ${./%s} %s", f, f),
      character(1)
    )
    copy_cmd <- paste0(paste(copy_lines, collapse = "\n      "), "\n      ")
  }

  build_phase <- sprintf(
    "%s%sRscript -e \"\n        source('libraries.R')\n        %s <- %s\n        %s(%s, '%s')\"",
    env_exports,
    copy_cmd,
    out_name,
    expr_str,
    serialize_str,
    out_name,
    out_name
  )

  # Derive base from nix_env
  base <- gsub("[^a-zA-Z0-9]", "_", nix_env)
  base <- sub("_nix$", "", base)

  if (length(fileset_parts) > 0) {
    fileset_nix <- paste0("./", fileset_parts, collapse = " ")
    src_snippet <- sprintf(
      "   src = defaultPkgs.lib.fileset.toSource {\n      root = ./.;\n      fileset = defaultPkgs.lib.fileset.unions [ %s ];\n    };\n ",
      fileset_nix
    )
  } else {
    src_snippet <- ""
  }

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
    nix_env = nix_env,
    serialize_function = serialize_str,
    unserialize_function = unserialize_str,
    env_var = env_var
  ) |>
    structure(class = "derivation")
}


#' Create a Nix expression running a Python function
#'
#' @param name Symbol, name of the derivation.
#' @param py_expr Character, Python code to generate the expression.
#' @param additional_files Character vector, additional files to include. Custom
#'   functions must go into a script called "functions.py", and additional files
#'   that need to be accessible during the build process can be named anything.
#' @param nix_env Character, path to the Nix environment file, default is
#'   "default.nix".
#' @param serialize_function Character, defaults to NULL. The name of the Python
#'   function used to serialize the object. It must accept two arguments: the
#'   object to serialize (first), and the target file path (second). If NULL,
#'   the default behavior uses `pickle.dump`. Define this function in
#'   `functions.py`.
#' @param unserialize_function Character, defaults to NULL. The name of the
#'   Python function used to unserialize the object. It must accept one
#'   argument: the file path.
#' @param env_var Character vector, defaults to NULL. A named vector of environment variables
#'   before running the Python script, e.g., c(PYTHONPATH = "/path/to/modules").
#'   Each entry will be added as an export statement in the build phase.
#' @details At a basic level,
#'   `rxp_py(mtcars_am, "mtcars.filter(polars.col('am') == 1).to_pandas()")`
#'    is equivalent to
#'   `mtcars_am = mtcars.filter(polars.col('am') == 1).to_pandas()`. `rxp_py()`
#'   generates the required Nix boilerplate to output a so-called "derivation"
#'   in Nix jargon. A Nix derivation is a recipe that defines how to create an
#'   output (in this case `mtcars_am`) including its dependencies, build steps,
#'   and output paths.
#' @return An object of class derivation which inherits from lists.
#' @examples
#' \dontrun{
#'   rxp_py(
#'     mtcars_pl_am,
#'     py_expr = "mtcars_pl.filter(polars.col('am') == 1).to_pandas()"
#'   )
#'
#'   # Custom serialization
#'   rxp_py(
#'     mtcars_pl_am,
#'     py_expr = "mtcars_pl.filter(polars.col('am') == 1).to_pandas()",
#'     serialize_function = "serialize_model",
#'     additional_files = "functions.py")
#' }
#' @export
rxp_py <- function(
  name,
  py_expr,
  additional_files = "",
  nix_env = "default.nix",
  serialize_function = NULL,
  unserialize_function = NULL,
  env_var = NULL
) {
  out_name <- deparse1(substitute(name))
  py_expr <- gsub("'", "\\'", py_expr, fixed = TRUE)

  # Handle serialize_function for the build_phase
  if (is.null(serialize_function)) {
    serialize_str <- sprintf(
      "with open('%s', 'wb') as f: pickle.dump(globals()['%s'], f)",
      out_name,
      out_name
    )
  } else {
    if (!is.character(serialize_function))
      stop("serialize_function must be a character string or NULL")
    serialize_str <- sprintf(
      "%s(globals()['%s'], '%s')",
      serialize_function,
      out_name,
      out_name
    )
  }

  # Handle unserialize_function to pass down
  if (is.null(unserialize_function)) {
    unserialize_str <- "pickle.load"
  } else {
    if (!is.character(unserialize_function))
      stop("unserialize_function must be a character string or NULL")
    unserialize_str <- unserialize_function
  }

  # Generate environment variable export statements if env_var is provided
  env_exports <- ""
  if (!is.null(env_var)) {
    env_exports <- paste(
      vapply(
        names(env_var),
        function(var) sprintf("export %s=%s", var, env_var[[var]]),
        character(1)
      ),
      collapse = "\n      "
    )
    if (env_exports != "") env_exports <- paste0(env_exports, "\n      ")
  }

  fileset_parts <- setdiff(additional_files, "functions.py")
  fileset_parts <- fileset_parts[nzchar(fileset_parts)]

  # build copy command for additional files
  copy_cmd <- ""
  if (length(fileset_parts) > 0) {
    copy_lines <- vapply(
      fileset_parts,
      function(f) sprintf("cp -r ${./%s} %s", f, f),
      character(1)
    )
    copy_cmd <- paste0(paste(copy_lines, collapse = "\n      "), "\n      ")
  }

  # Construct build_phase including cp commands then python execution
  build_phase <- paste0(
    env_exports,
    copy_cmd,
    "python -c \"\n",
    "exec(open('libraries.py').read())\n",
    "exec('",
    out_name,
    " = ",
    py_expr,
    "')\n",
    serialize_str,
    "\n",
    "\""
  )

  # Derive base from nix_env
  base <- gsub("[^a-zA-Z0-9]", "_", nix_env)
  base <- sub("_nix$", "", base)

  # Prepare the src snippet only once using the same fileset_parts
  if (length(fileset_parts) > 0) {
    fileset_nix <- paste0("./", fileset_parts, collapse = " ")
    src_snippet <- sprintf(
      "   src = defaultPkgs.lib.fileset.toSource {\n      root = ./.;\n      fileset = defaultPkgs.lib.fileset.unions [ %s ];\n    };\n ",
      fileset_nix
    )
  } else {
    src_snippet <- ""
  }

  # Generate the Nix snippet
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
    nix_env = nix_env,
    serialize_function = serialize_str,
    unserialize_function = unserialize_str,
    env_var = env_var
  ) |>
    structure(class = "derivation")
}

#' Render a Quarto document as a Nix derivation
#'
#' @family derivations
#' @param name Symbol, derivation name.
#' @param qmd_file Character, path to .qmd file.
#' @param additional_files Character vector, additional files to include, for
#'   example a folder containing images to include in the Quarto document.
#' @param nix_env Character, path to the Nix environment file, default is
#'   "default.nix".
#' @param args A character of additional arguments to be passed directly to the
#'   `quarto` command.
#' @param env_var List, defaults to NULL. A named list of environment variables
#'   to set before running the Quarto render command, e.g., c(QUARTO_PROFILE =
#'   "production"). Each entry will be added as an export statement in the build
#'   phase.
#' @details To include built derivations in the document,
#'   `rxp_read("derivation_name")` should be put in the .qmd file.
#' @return An object of class derivation which inherits from lists.
#' @examples
#' \dontrun{
#'   # Compile a .qmd file to a pdf using typst
#'   # `images` is a folder containing images to include in the Quarto doc
#'   rxp_qmd(
#'     name = report,
#'     qmd_file = "report.qmd",
#'     additional_files = "images",
#'     args = "--to typst"
#'   )
#' }
#' @export
rxp_qmd <- function(
  name,
  qmd_file,
  additional_files = "",
  nix_env = "default.nix",
  args = "",
  env_var = NULL
) {
  out_name <- deparse1(substitute(name))

  content <- readLines(qmd_file, warn = FALSE)
  content_str <- paste(content, collapse = "\n")

  # Extract unique rxp_read references
  matches <- gregexpr('rxp_read\\("([^"]+)"\\)', content_str)
  refs <- regmatches(content_str, matches)[[1]]
  refs <- sub('rxp_read\\("([^"]+)"\\)', '\\1', refs)
  refs <- unique(refs)

  # Generate substitution commands for each reference
  sub_cmds <- vapply(
    refs,
    function(ref) {
      sprintf(
        "substituteInPlace %s --replace-fail 'rxp_read(\"%s\")' 'rxp_read(\"${%s}\")'",
        qmd_file,
        ref,
        ref
      )
    },
    character(1)
  )

  # Generate environment variable export statements if env_var is provided
  env_exports <- ""
  if (!is.null(env_var)) {
    env_exports <- paste(
      vapply(
        names(env_var),
        function(var_name)
          sprintf("      export %s=%s", var_name, env_var[[var_name]]),
        character(1)
      ),
      collapse = "\n"
    )
    if (env_exports != "") {
      env_exports <- paste0(env_exports, "\n")
    }
  }

  build_phase <- paste(
    "      mkdir home",
    "      export HOME=$PWD/home",
    "      export RETICULATE_PYTHON=${defaultPkgs.python3}/bin/python",
    env_exports,
    if (length(sub_cmds) > 0)
      paste("      ", sub_cmds, sep = "", collapse = "\n") else "",
    sprintf("      quarto render %s %s --output-dir $out", qmd_file, args),
    sep = "\n"
  )

  # Prepare the fileset for src
  if (identical(additional_files, "")) {
    additional_files <- NULL
  }
  fileset_parts <- c(qmd_file, additional_files)
  fileset_nix <- paste0("./", fileset_parts, collapse = " ")

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

  list(
    name = out_name,
    snippet = snippet,
    type = "rxp_qmd",
    qmd_file = qmd_file,
    additional_files = additional_files,
    nix_env = nix_env,
    args = args,
    env_var = env_var
  ) |>
    structure(class = "derivation")
}


#' rxp_file_common
#'
#' Creates a Nix expression with shared logic for R and Python file reading.
#'
#' @param out_name Character, the name of the derivation as a string.
#' @param path Character, the file path (URL or local) or folder path.
#' @param nix_env Character, path to the Nix environment file.
#' @param build_phase Character, the language-specific build phase script.
#' @param type Character, the type of derivation ("rxp_r" or "rxp_py").
#' @param derivation_func Character, the Nix derivation function
#'   ("makeRDerivation" or "makePyDerivation").
#' @param library_ext Character, the library file extension ("R" or "py").
#' @param env_var List, defaults to NULL. A named list of environment variables
#'   to set before running the script, e.g., c(DATA_PATH = "/path/to/data").
#'   Each entry will be added as an export statement in the build phase.
#' @examples
#' \dontrun{
#'   rxp_file_common(
#'     out_name = out_name,
#'     path = actual_path,
#'     nix_env = nix_env,
#'     build_phase = build_phase,
#'     type = "rxp_r",
#'     derivation_func = "makeRDerivation",
#'     library_ext = "R",
#'     env_var = env_var
#'   )
#' }
#' @return A list with `name`, `snippet`, `type`, and `nix_env`.
rxp_file_common <- function(
  out_name,
  path,
  nix_env,
  build_phase,
  type,
  derivation_func,
  library_ext,
  env_var = NULL
) {
  # Handle source: URL or local path
  src_part <- if (grepl("^https?://", path)) {
    hash <- tryCatch(
      system(paste("nix-prefetch-url", shQuote(path)), intern = TRUE),
      error = function(e) stop("Failed to run nix-prefetch-url: ", e$message)
    )
    if (length(hash) == 0 || hash == "") {
      stop("nix-prefetch-url did not return a hash for URL: ", path)
    }
    sprintf(
      "defaultPkgs.fetchurl {\n      url = \"%s\";\n      sha256 = \"%s\";\n    }",
      path,
      trimws(hash[1])
    )
  } else {
    sprintf("./%s", path)
  }

  base <- gsub("[^a-zA-Z0-9]", "_", nix_env)
  base <- sub("_nix$", "", base)

  # Generate environment variable export statements if env_var is provided
  env_exports <- ""
  if (!is.null(env_var)) {
    env_exports <- paste(
      vapply(
        names(env_var),
        function(var_name)
          sprintf("export %s=%s", var_name, env_var[[var_name]]),
        character(1)
      ),
      collapse = "\n      "
    )
    if (env_exports != "") {
      env_exports <- paste0(env_exports, "\n      ")
    }
  }

  # Add env_exports to the build_phase
  if (env_exports != "") {
    build_phase <- paste0(env_exports, build_phase)
  }

  # Build the Nix derivation snippet
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
    nix_env = nix_env,
    env_var = env_var
  ) |>
    structure(class = "derivation")
}

#' rxp_r_file
#'
#' Creates a Nix expression that reads in a file (or folder of data) using R.
#'
#' @param name Symbol, the name of the derivation.
#' @param path Character, the file path to include (e.g., "data/mtcars.shp") or
#'   a folder path (e.g., "data"). See details.
#' @param read_function Function, an R function to read the data, taking one
#'   argument (the path).
#' @param nix_env Character, path to the Nix environment file, default is
#'   "default.nix".
#' @param copy_data_folder Logical, if TRUE then the entire folder is copied
#'   recursively into the build sandbox.
#' @param env_var List, defaults to NULL. A named list of environment variables
#'   to set before running the R script, e.g., c(VAR = "hello"). Each entry will
#'   be added as an export statement in the build phase.
#' @details There are three ways to read in data in a rixpress pipeline: the
#'   first is to point directly to a file, for example, `rxp_r_file(mtcars, path
#'   = "data/mtcars.csv", read_function = read.csv)`. The second way is to point
#'   to a file but to also include the files in the "data/" folder (the folder
#'   can be named something else). This is needed when data is split between
#'   several files, such as a shapefile which typically also needs other files
#'   such as `.shx` and `.dbf` files. For this, `copy_data_folder` must be set
#'   to `TRUE`. The last way to read in data, is to only point to a folder, and
#'   use a function that recursively reads in all data. For example
#'   `rxp_r_file(many_csvs, path = "data", read_function = \(x)(readr::read_csv(
#'   list.files(x, full.names = TRUE, pattern = ".csv$"))))` the provided
#'   anonymous function will read all the `.csv` files in the `data/` folder.
#' @return An object of class derivation which inherits from lists.
#' @examples
#' \dontrun{
#'   # Read a CSV file
#'   rxp_r_file(
#'     name = mtcars,
#'     path = "data/mtcars.csv",
#'     read_function = \(x) (read.csv(file = x, sep = "|"))
#'   )
#'
#'   # Read all CSV files in a directory using a lambda function
#'  rxp_r_file(
#'    name = mtcars_r,
#'    path = "data",
#'    read_function = \(x)
#'      (readr::read_delim(list.files(x, full.names = TRUE), delim = "|")),
#'    copy_data_folder = TRUE
#'  )
#' }
#' @export
rxp_r_file <- function(
  name,
  path,
  read_function,
  nix_env = "default.nix",
  copy_data_folder = FALSE,
  env_var = NULL
) {
  out_name <- deparse1(substitute(name))
  read_func_str <- deparse1(substitute(read_function))
  read_func_str <- gsub("\"", "'", read_func_str)

  if (!copy_data_folder) {
    # Use single file copy.
    build_phase <- sprintf(
      "cp $src input_file\n      Rscript -e \"\n        source('libraries.R')\n        data <- do.call(%s, list('input_file'))\n        saveRDS(data, '%s')\"",
      read_func_str,
      out_name
    )
    actual_path <- path
  } else {
    if (file.exists(path) && file.info(path)$isdir) {
      # If the provided path is a folder, use that folder as the source.
      actual_path <- path
      build_phase <- sprintf(
        "cp -r $src input_folder\n      Rscript -e \"\n        source('libraries.R')\n        data <- do.call(%s, list('input_folder/'))\n        saveRDS(data, '%s')\"",
        read_func_str,
        out_name
      )
    } else {
      # Otherwise assume path is a file; use its directory as the source.
      actual_path <- dirname(path)
      file_name <- basename(path)
      build_phase <- sprintf(
        "cp -r $src input_folder\n      Rscript -e \"\n        source('libraries.R')\n        data <- do.call(%s, list('input_folder/%s'))\n        saveRDS(data, '%s')\"",
        read_func_str,
        file_name,
        out_name
      )
    }
  }

  rxp_file_common(
    out_name = out_name,
    path = actual_path,
    nix_env = nix_env,
    build_phase = build_phase,
    type = "rxp_r",
    derivation_func = "makeRDerivation",
    library_ext = "R",
    env_var = env_var
  )
}

#' rxp_py_file
#'
#' Creates a Nix expression that reads in a file (or folder of data) using
#' Python.
#'
#' @family derivations
#' @param name Symbol, the name of the derivation.
#' @param path Character, the file path to include (e.g., "data/mtcars.shp") or
#'   a folder path (e.g., "data"). See details.
#' @param read_function Character, a Python function to read the data, taking
#'   one argument (the path).
#' @param nix_env Character, path to the Nix environment file, default is
#'   "default.nix".
#' @param copy_data_folder Logical, if TRUE then the entire folder is copied
#'   recursively into the build sandbox.
#' @param env_var List, defaults to NULL. A named list of environment variables
#'   to set before running the Python script, e.g., c(PYTHONPATH =
#'   "/path/to/modules"). Each entry will be added as an export statement in the
#'   build phase.
#' @details There are three ways to read in data in a rixpress pipeline: the
#'   first is to point directly to a file, for example, `rxp_py_file(mtcars, path =
#'   "data/mtcars.csv", read_function = pandas.read_csv)`. The second way is to
#'   point to a file but to also include of the files in the "data/" folder (the
#'   folder can named something else). This is needed when data is split between
#'   several files, such as a shapefile which typically also needs other files
#'   such as `.shx` and `.dbf` files. For this, `copy_data_folder` must be set
#'   to `TRUE`. The last way to read in data, is to only point to a folder, and
#'   use a function that recursively reads in all data. For example
#'   `rxp_py_file(many_csvs, path = "data", read_function = 'lambda x:
#'   pandas.read_csv(os.path.join(x, os.listdir(x)[0]), delimiter="|")')` the
#'   provided anonymous function will read all the `.csv` file in the `data/`
#'   folder.
#' @return An object of class derivation which inherits from lists.
#' @examples
#' \dontrun{
#'   # Read a CSV file with pandas
#'   rxp_py_file(
#'     name = pandas_data,
#'     path = "data/dataset.csv",
#'     read_function = "pandas.read_csv"
#'   )
#'
#' # Read all CSV files in a directory using a
#' # user defined function
#'  rxp_py_file(
#'   name = mtcars_py,
#'   path = 'data',
#'   read_function = "read_many_csvs",
#'   copy_data_folder = TRUE
#' )
#' }
#' @export
rxp_py_file <- function(
  name,
  path,
  read_function,
  nix_env = "default.nix",
  copy_data_folder = FALSE,
  env_var = NULL
) {
  out_name <- deparse1(substitute(name))
  # Sanitize the read_function string.
  read_function <- gsub("'", "\\'", read_function, fixed = TRUE)
  if (!is.character(read_function) || length(read_function) != 1) {
    stop("read_function must be a single character string")
  }

  if (!copy_data_folder) {
    build_phase <- sprintf(
      "cp $src input_file\npython -c \"\nexec(open('libraries.py').read())\nfile_path = 'input_file'\ndata = eval('%s')(file_path)\nwith open('%s', 'wb') as f:\n    pickle.dump(data, f)\n\"\n",
      read_function,
      out_name
    )
    actual_path <- path
  } else {
    if (file.exists(path) && file.info(path)$isdir) {
      # If path is a folder.
      actual_path <- path
      build_phase <- sprintf(
        "cp -r $src input_folder\npython -c \"\nexec(open('libraries.py').read())\nfile_path = 'input_folder/'\ndata = eval('%s')(file_path)\nwith open('%s', 'wb') as f:\n    pickle.dump(data, f)\n\"\n",
        read_function,
        out_name
      )
    } else {
      # Assume path is a file.
      actual_path <- dirname(path)
      file_name <- basename(path)
      build_phase <- sprintf(
        "cp -r $src input_folder\npython -c \"\nexec(open('libraries.py').read())\nfile_path = 'input_folder/%s'\ndata = eval('%s')(file_path)\nwith open('%s', 'wb') as f:\n    pickle.dump(data, f)\n\"\n",
        file_name,
        read_function,
        out_name
      )
    }
  }

  rxp_file_common(
    out_name = out_name,
    path = actual_path,
    nix_env = nix_env,
    build_phase = build_phase,
    type = "rxp_py",
    derivation_func = "makePyDerivation",
    library_ext = "py",
    env_var = env_var
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
#' @param direction Character, either "py2r" (Python to R) or "r2py" (R to
#'   Python).
#' @return A list with elements: `name`, `snippet`, `type`, `additional_files`,
#'   `nix_env`.
#' @examples
#' \dontrun{
#'   # This is an internal function used by rxp_py2r and rxp_r2py
#'   # Not typically called directly by users
#'   deriv <- rxp_common_setup(
#'     out_name = "r_data",
#'     expr_str = "py_data",
#'     nix_env = "default.nix",
#'     direction = "py2r"
#'   )
#' }
rxp_common_setup <- function(out_name, expr_str, nix_env, direction) {
  expr_str <- gsub("\"", "'", expr_str) # Replace " with ' for Nix
  base <- gsub("[^a-zA-Z0-9]", "_", nix_env)
  base <- sub("_nix$", "", base)

  if (direction == "py2r") {
    r_command <- sprintf(
      "         %s <- reticulate::py_load_object('${%s}/%s', pickle = 'pickle', convert = TRUE)\n         saveRDS(%s, '%s')",
      out_name,
      expr_str,
      expr_str,
      out_name,
      out_name
    )
  } else if (direction == "r2py") {
    r_command <- sprintf(
      "         %s <- readRDS('${%s}/%s')\n         reticulate::py_save_object(%s, '%s', pickle = 'pickle')",
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
    "export RETICULATE_PYTHON=${defaultPkgs.python3}/bin/python\n       Rscript -e \"\n         source('libraries.R')\n%s\"",
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
  ) |>
    structure(class = "derivation")
}


#' Transfer Python object into an R session.
#'
#' @family interop function
#' @param name Symbol, name of the derivation.
#' @param expr Symbol, Python object to be loaded into R.
#' @param nix_env Character, path to the Nix environment file, default is
#'   "default.nix".
#' @details `rxp_py2r(my_obj, my_python_object)` loads a serialized Python
#'   object and saves it as an RDS file using `reticulate::py_load_object()`.
#' @return An object of class derivation which inherits from lists.
#' @examples
#' \dontrun{
#' rxp_py2r(my_obj, my_python_object)
#' }
#' @export
rxp_py2r <- function(name, expr, nix_env = "default.nix") {
  out_name <- deparse1(substitute(name))
  expr_str <- deparse1(substitute(expr))
  rxp_common_setup(out_name, expr_str, nix_env, "py2r")
}

#' Transfer R object into a Python session.
#'
#' @family interop functions
#' @param name Symbol, name of the derivation.
#' @param expr Symbol, R object to be saved into a Python pickle.
#' @param nix_env Character, path to the Nix environment file, default is
#'   "default.nix".
#' @details `rxp_r2py(my_obj, my_r_object)` saves an R object to a Python pickle
#'   using `reticulate::py_save_object()`.
#' @return An object of class derivation which inherits from lists.
#' @examples
#' \dontrun{
#'   rxp_r2py(my_obj, my_r_object)
#' }
#' @export
rxp_r2py <- function(name, expr, nix_env = "default.nix") {
  out_name <- deparse1(substitute(name))
  expr_str <- deparse1(substitute(expr))
  rxp_common_setup(out_name, expr_str, nix_env, "r2py")
}

#' Render an R Markdown document as a Nix derivation
#'
#' @family derivations
#' @param name Symbol, derivation name.
#' @param rmd_file Character, path to .Rmd file.
#' @param additional_files Character vector, additional files to include, for
#'   example a folder containing the pictures to include in the R Markdown
#'   document.
#' @param nix_env Character, path to the Nix environment file, default is
#'   "default.nix".
#' @param params List, parameters to pass to the R Markdown document. Default is
#'   NULL.
#' @param env_var List, defaults to NULL. A named list of environment variables
#'   to set before running the R Markdown render command, e.g., c(RSTUDIO_PANDOC
#'   = "/path/to/pandoc"). Each entry will be added as an export statement in
#'   the build phase.
#' @details To include objects built in the pipeline,
#'   `rxp_read("derivation_name")` should be put in the .Rmd file.
#' @return An object of class derivation which inherits from lists.
#' @examples
#' \dontrun{
#'   # Compile a .Rmd file to a pdf
#'   # `images` is a folder containing images to include in the R Markdown doc
#'   rxp_rmd(
#'     name = report,
#'     rmd_file = "report.Rmd",
#'     additional_files = "images"
#'   )
#' }
#' @export
rxp_rmd <- function(
  name,
  rmd_file,
  additional_files = "",
  nix_env = "default.nix",
  params = NULL,
  env_var = NULL
) {
  out_name <- deparse1(substitute(name))

  content <- readLines(rmd_file, warn = FALSE)
  content_str <- paste(content, collapse = "\n")

  # Extract unique rxp_read references
  matches <- gregexpr('rxp_read\\("([^"]+)"\\)', content_str)
  refs <- regmatches(content_str, matches)[[1]]
  refs <- sub('rxp_read\\("([^"]+)"\\)', '\\1', refs)
  refs <- unique(refs)

  # Generate substitution commands for each reference
  sub_cmds <- vapply(
    refs,
    function(ref) {
      sprintf(
        "substituteInPlace %s --replace-fail 'rxp_read(\"%s\")' 'rxp_read(\"${%s}\")'",
        rmd_file,
        ref,
        ref
      )
    },
    character(1)
  )

  # Prepare render arguments
  render_args <- "rmarkdown::render(input = file.path('$PWD', rmd_file), output_dir = '$out'"

  if (!is.null(params)) {
    params_str <- paste0(
      "list(",
      paste(
        mapply(
          function(name, value) sprintf("%s = %s", name, deparse(value)),
          names(params),
          params
        ),
        collapse = ", "
      ),
      ")"
    )
    render_args <- paste0(render_args, sprintf(", params = %s", params_str))
  }

  render_args <- paste0(render_args, ")")

  # Generate environment variable export statements if env_var is provided
  env_exports <- ""
  if (!is.null(env_var)) {
    env_exports <- paste(
      vapply(
        names(env_var),
        function(var_name)
          sprintf("      export %s=%s", var_name, env_var[[var_name]]),
        character(1)
      ),
      collapse = "\n"
    )
    if (env_exports != "") {
      env_exports <- paste0(env_exports, "\n")
    }
  }

  build_phase <- paste(
    "      mkdir home",
    "      export HOME=$PWD/home",
    "      export RETICULATE_PYTHON=${defaultPkgs.python3}/bin/python",
    env_exports,
    if (length(sub_cmds) > 0)
      paste("      ", sub_cmds, sep = "", collapse = "\n") else "",
    sprintf("      Rscript -e \"rmd_file <- '%s'; %s\"", rmd_file, render_args),
    sep = "\n"
  )

  # Prepare the fileset for src
  fileset_parts <- c(rmd_file, additional_files)
  fileset_nix <- paste0("./", fileset_parts, collapse = " ")

  # Derive base from nix_env
  base <- gsub("[^a-zA-Z0-9]", "_", nix_env)
  base <- sub("_nix$", "", base)

  snippet <- sprintf(
    "  %s = defaultPkgs.stdenv.mkDerivation {\n    name = \"%s\";\n    src = defaultPkgs.lib.fileset.toSource {\n      root = ./.;\n      fileset = defaultPkgs.lib.fileset.unions [ %s ];\n    };\n    buildInputs = %sBuildInputs;\n    configurePhase = %sConfigurePhase;\n    buildPhase = ''\n%s\n    '';\n  };",
    out_name,
    out_name,
    fileset_nix,
    base,
    base,
    build_phase
  )

  list(
    name = out_name,
    snippet = snippet,
    type = "rxp_rmd",
    rmd_file = rmd_file,
    additional_files = additional_files,
    nix_env = nix_env,
    params = params,
    env_var = env_var
  ) |>
    structure(class = "derivation")
}

#' Print method for derivation objects
#' @param x An object of class "derivation"
#' @param ... Additional arguments passed to print methods
#' @return Nothing, prints a summary of the derivation object to the console.
#' @examples
#' \dontrun{
#' # d0 is a previously defined derivation
#'   print(d0)
#' }
#' @family utilities
#' @export
print.derivation <- function(x, ...) {
  cat("Name:", x$name, "\n")
  cat("Type:", x$type, "\n")
  if ("serialize_function" %in% names(x)) {
    cat("Serialize function:", x$serialize_function, "\n")
  }
  if ("unserialize_function" %in% names(x)) {
    cat("Unserialize function:", x$unserialize_function, "\n")
  }
  if (x$type == "rxp_qmd") {
    cat("QMD file:", x$qmd_file, "\n")
  }
  if (x$type == "rxp_rmd") {
    cat("RMD file:", x$rmd_file, "\n")
  }
  cat(
    "Additional files:",
    if (length(x$additional_files) == 0 || x$additional_files == "") "None" else
      paste(x$additional_files, collapse = ", "),
    "\n"
  )
  cat("Nix env:", x$nix_env, "\n")
  if ("env_var" %in% names(x)) {
    cat(
      "Env variables:",
      if (is.null(x$env_var)) "None" else
        paste(names(x$env_var), x$env_var, sep = "=", collapse = ", "),
      "\n"
    )
  }
}
