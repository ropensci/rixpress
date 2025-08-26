#' Creates a Nix expression that reads in a file (or folder of data) using R.
#'
#' @family derivations
#' @param name Symbol, the name of the derivation.
#' @param path Character, the file path to include (e.g., "data/mtcars.shp") or
#'   a folder path (e.g., "data"). See details.
#' @param read_function Function, an R function to read the data, taking one
#'   argument (the path).
#' @param user_functions Character vector, user-defined functions to include.
#'   This should be a script (or scripts) containing user-defined functions
#'   to include during the build process for this derivation. It is recommended
#'   to use one script per function, and only include the required script(s) in
#'   the derivation.
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
  user_functions = "",
  nix_env = "default.nix",
  copy_data_folder = FALSE,
  env_var = NULL
) {
  out_name <- deparse1(substitute(name))
  read_func_str <- deparse1(substitute(read_function))
  read_func_str <- gsub("\"", "'", read_func_str)

  # build copy command for user_functions
  user_functions_copy_cmd <- ""
  if (!is.null(user_functions) && length(user_functions) > 0) {
    user_functions_clean <- user_functions[nzchar(user_functions)]
    if (length(user_functions_clean) > 0) {
      user_copy_lines <- vapply(
        user_functions_clean,
        function(f) sprintf("cp ${./%s} %s", f, f),
        character(1)
      )
      user_functions_copy_cmd <- paste0(
        paste(user_copy_lines, collapse = "\n      "),
        "\n      "
      )
    }
  }

  # Generate source commands for user_functions
  source_cmd <- ""
  if (!is.null(user_functions) && length(user_functions) > 0) {
    user_functions_clean <- user_functions[nzchar(user_functions)]
    if (length(user_functions_clean) > 0) {
      source_lines <- vapply(
        user_functions_clean,
        function(f) sprintf("source('%s')", f),
        character(1)
      )
      source_cmd <- paste0(
        paste(source_lines, collapse = "\n        "),
        "\n        "
      )
    }
  }

  if (!copy_data_folder) {
    # Use single file copy.
    build_phase <- sprintf(
      "%scp $src input_file\n      Rscript -e \"\n        source('libraries.R')\n        %sdata <- do.call(%s, list('input_file'))\n        saveRDS(data, '%s')\"",
      user_functions_copy_cmd,
      source_cmd,
      read_func_str,
      out_name
    )
    actual_path <- path
  } else {
    if (file.exists(path) && file.info(path)$isdir) {
      # If the provided path is a folder, use that folder as the source.
      actual_path <- path
      build_phase <- sprintf(
        "%scp -r $src input_folder\n      Rscript -e \"\n        source('libraries.R')\n        %sdata <- do.call(%s, list('input_folder/'))\n        saveRDS(data, '%s')\"",
        user_functions_copy_cmd,
        source_cmd,
        read_func_str,
        out_name
      )
    } else {
      # Otherwise assume path is a file; use its directory as the source.
      actual_path <- dirname(path)
      file_name <- basename(path)
      build_phase <- sprintf(
        "%scp -r $src input_folder\n      Rscript -e \"\n        source('libraries.R')\n        %sdata <- do.call(%s, list('input_folder/%s'))\n        saveRDS(data, '%s')\"",
        user_functions_copy_cmd,
        source_cmd,
        read_func_str,
        file_name,
        out_name
      )
    }
  }

  # Handle source: URL or local path - include user_functions in fileset for local paths
  src_part <- if (grepl("^https?://", actual_path)) {
    hash <- tryCatch(
      system(paste("nix-prefetch-url", shQuote(actual_path)), intern = TRUE),
      error = function(e) stop("Failed to run nix-prefetch-url: ", e$message)
    )
    if (length(hash) == 0 || hash == "") {
      stop("nix-prefetch-url did not return a hash for URL: ", actual_path)
    }
    sprintf(
      "defaultPkgs.fetchurl {\n      url = \"%s\";\n      sha256 = \"%s\";\n    }",
      actual_path,
      trimws(hash[1])
    )
  } else {
    # For local paths, include user_functions in fileset if provided
    if (!is.null(user_functions) && any(nzchar(user_functions))) {
      user_functions_clean <- user_functions[nzchar(user_functions)]
      fileset_parts <- c(actual_path, user_functions_clean)
      fileset_nix <- paste0("./", fileset_parts, collapse = " ")
      sprintf(
        "defaultPkgs.lib.fileset.toSource {\n      root = ./.;\n      fileset = defaultPkgs.lib.fileset.unions [ %s ];\n    }",
        fileset_nix
      )
    } else {
      sprintf("./%s", actual_path)
    }
  }

  base <- gsub("[^a-zA-Z0-9]", "_", nix_env)
  base <- sub("_nix$", "", base)

  # Generate environment variable export statements if env_var is provided
  env_exports <- ""
  if (!is.null(env_var)) {
    env_exports <- paste(
      vapply(
        names(env_var),
        function(var_name) {
          sprintf("export %s=%s", var_name, env_var[[var_name]])
        },
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
  snippet <- make_derivation_snippet(
    out_name = out_name,
    src_snippet = sprintf("    src = %s;\n", src_part),
    base = base,
    build_phase = build_phase,
    derivation_type = "R"
  )

  list(
    name = out_name,
    snippet = snippet,
    type = "rxp_r",
    additional_files = "",
    user_functions = user_functions,
    nix_env = nix_env,
    env_var = env_var
  ) |>
    structure(class = "rxp_derivation")
}

#' Creates a Nix expression that reads in a file (or folder of data) using
#' Python.
#'
#' @family derivations
#' @param name Symbol, the name of the derivation.
#' @param path Character, the file path to include (e.g., "data/mtcars.shp") or
#'   a folder path (e.g., "data"). See details.
#' @param read_function Character, a Python function to read the data, taking
#'   one argument (the path).
#' @param user_functions Character vector, user-defined functions to include.
#'   This should be a script (or scripts) containing user-defined functions
#'   to include during the build process for this derivation. It is recommended
#'   to use one script per function, and only include the required script(s) in
#'   the derivation.
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
  user_functions = "",
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

  # build copy command for user_functions (explicit copy, not -r)
  user_functions_copy_cmd <- ""
  if (!is.null(user_functions) && any(nzchar(user_functions))) {
    user_functions_clean <- user_functions[nzchar(user_functions)]
    user_copy_lines <- vapply(
      user_functions_clean,
      function(f) sprintf("cp ${./%s} %s", f, f),
      character(1)
    )
    user_functions_copy_cmd <- paste0(
      paste(user_copy_lines, collapse = "\n"),
      "\n"
    )
  }

  # Generate import commands for user_functions
  user_import_cmd <- ""
  if (!is.null(user_functions) && any(nzchar(user_functions))) {
    user_functions_clean <- user_functions[nzchar(user_functions)]
    import_lines <- vapply(
      user_functions_clean,
      function(f) sprintf("exec(open('%s').read())", f),
      character(1)
    )
    user_import_cmd <- paste0(paste(import_lines, collapse = "\n"), "\n")
  }

  if (!copy_data_folder) {
    build_phase <- sprintf(
      "%scp $src input_file\npython -c \"\nexec(open('libraries.py').read())\n%sfile_path = 'input_file'\ndata = eval('%s')(file_path)\nwith open('%s', 'wb') as f:\n    pickle.dump(data, f)\n\"\n",
      user_functions_copy_cmd,
      user_import_cmd,
      read_function,
      out_name
    )
    actual_path <- path
  } else {
    if (file.exists(path) && file.info(path)$isdir) {
      # If path is a folder.
      actual_path <- path
      build_phase <- sprintf(
        "%scp -r $src input_folder\npython -c \"\nexec(open('libraries.py').read())\n%sfile_path = 'input_folder/'\ndata = eval('%s')(file_path)\nwith open('%s', 'wb') as f:\n    pickle.dump(data, f)\n\"\n",
        user_functions_copy_cmd,
        user_import_cmd,
        read_function,
        out_name
      )
    } else {
      # Assume path is a file.
      actual_path <- dirname(path)
      file_name <- basename(path)
      build_phase <- sprintf(
        "%scp -r $src input_folder\npython -c \"\nexec(open('libraries.py').read())\n%sfile_path = 'input_folder/%s'\ndata = eval('%s')(file_path)\nwith open('%s', 'wb') as f:\n    pickle.dump(data, f)\n\"\n",
        user_functions_copy_cmd,
        user_import_cmd,
        file_name,
        read_function,
        out_name
      )
    }
  }

  # Handle source: URL or local path - include user_functions in fileset for local paths
  src_part <- if (grepl("^https?://", actual_path)) {
    hash <- tryCatch(
      system(paste("nix-prefetch-url", shQuote(actual_path)), intern = TRUE),
      error = function(e) stop("Failed to run nix-prefetch-url: ", e$message)
    )
    if (length(hash) == 0 || hash == "") {
      stop("nix-prefetch-url did not return a hash for URL: ", actual_path)
    }
    sprintf(
      "defaultPkgs.fetchurl {\n      url = \"%s\";\n      sha256 = \"%s\";\n    }",
      actual_path,
      trimws(hash[1])
    )
  } else {
    # For local paths, include user_functions in fileset if provided
    if (!is.null(user_functions) && any(nzchar(user_functions))) {
      user_functions_clean <- user_functions[nzchar(user_functions)]
      fileset_parts <- c(actual_path, user_functions_clean)
      fileset_nix <- paste0("./", fileset_parts, collapse = " ")
      sprintf(
        "defaultPkgs.lib.fileset.toSource {\n      root = ./.;\n      fileset = defaultPkgs.lib.fileset.unions [ %s ];\n    }",
        fileset_nix
      )
    } else {
      sprintf("./%s", actual_path)
    }
  }

  base <- gsub("[^a-zA-Z0-9]", "_", nix_env)
  base <- sub("_nix$", "", base)

  # Generate environment variable export statements if env_var is provided
  env_exports <- ""
  if (!is.null(env_var)) {
    env_exports <- paste(
      vapply(
        names(env_var),
        function(var_name) {
          sprintf("export %s=%s", var_name, env_var[[var_name]])
        },
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
  snippet <- make_derivation_snippet(
    out_name = out_name,
    src_snippet = sprintf("    src = %s;\n", src_part),
    base = base,
    build_phase = build_phase,
    derivation_type = "Py"
  )

  list(
    name = out_name,
    snippet = snippet,
    type = "rxp_py",
    additional_files = "",
    user_functions = user_functions,
    nix_env = nix_env,
    env_var = env_var
  ) |>
    structure(class = "rxp_derivation")
}

#' Transfer Python object into an R session.
#'
#' @family interop functions
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
