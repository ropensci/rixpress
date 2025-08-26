#' Clean user functions vector
#'
#' Removes empty character entries from the provided `user_functions` vector.
#'
#' @param user_functions Character vector of user-defined function script paths.
#' @return Character vector without empty strings.
clean_user_functions <- function(user_functions) {
  if (is.null(user_functions)) return(character(0))
  user_functions[nzchar(user_functions)]
}

#' Build copy commands for user functions
#'
#' Generates shell commands to copy user-defined function scripts into the
#' build sandbox.
#'
#' @param user_functions Character vector of script file names.
#' @return A string of `cp` commands, suitable for embedding in a build phase.
build_user_copy_cmd <- function(user_functions) {
  if (length(user_functions) == 0) return("")
  paste0(
    paste(
      sprintf("cp ${./%s} %s", user_functions, user_functions),
      collapse = "\n      "
    ),
    "\n      "
  )
}

#' Build code import/source commands for user functions
#'
#' Creates language-specific commands to load user-defined function scripts.
#'
#' @param user_functions Character vector of script file names.
#' @param lang Language string, `"R"` or `"Python"`.
#' @return A string of import/source statements.
build_user_code_cmd <- function(user_functions, lang) {
  if (length(user_functions) == 0) return("")
  fmt <- switch(
    lang,
    R      = "source('%s')",
    Py = "exec(open('%s').read())",
    stop("Unsupported lang: ", lang)
  )
  paste(sprintf(fmt, user_functions), collapse = "\n")
}

#' Build environment variable export commands
#'
#' Generates shell export commands for setting environment variables before
#' running the build script.
#'
#' @param env_var Named list of environment variables.
#' @return A string of `export` commands with line breaks, or `""` if none.
build_env_exports <- function(env_var) {
  if (is.null(env_var) || length(env_var) == 0) return("")
  lines <- vapply(names(env_var), function(var) {
    sprintf("export %s=%s", var, env_var[[var]])
  }, character(1))
  paste0(paste(lines, collapse = "\n      "), "\n      ")
}

#' Build Nix src part
#'
#' Creates the `src` attribute for the Nix derivation. Handles both remote URLs
#' (using `nix-prefetch-url`) and local paths (with optional user function
#' scripts).
#'
#' @param actual_path Character path to file or directory.
#' @param user_functions Character vector of additional files to include.
#' @return A string with the `src` Nix expression.
build_src_part <- function(actual_path, user_functions) {
  if (grepl("^https?://", actual_path)) {
    hash <- tryCatch(
      system(paste("nix-prefetch-url", shQuote(actual_path)), intern = TRUE),
      error = function(e) stop("Failed to run nix-prefetch-url: ", e$message)
    )
    if (length(hash) == 0 || hash == "")
      stop("nix-prefetch-url did not return a hash for URL: ", actual_path)

    sprintf(
      "defaultPkgs.fetchurl {\n      url = \"%s\";\n      sha256 = \"%s\";\n    }",
      actual_path,
      trimws(hash[1])
    )
  } else if (length(user_functions) > 0) {
    fileset_parts <- c(actual_path, user_functions)
    fileset_nix <- paste0("./", fileset_parts, collapse = " ")
    sprintf(
      "defaultPkgs.lib.fileset.toSource {\n      root = ./.;\n      fileset = defaultPkgs.lib.fileset.unions [ %s ];\n    }",
      fileset_nix
    )
  } else {
    sprintf("./%s", actual_path)
  }
}

#' Sanitize nix environment string
#'
#' Produces a base identifier string by replacing invalid characters and
#' stripping suffixes.
#'
#' @param nix_env Character path to a Nix environment file (e.g., `"default.nix"`).
#' @return Sanitized base string.
sanitize_base <- function(nix_env) {
  base <- gsub("[^a-zA-Z0-9]", "_", nix_env)
  sub("_nix$", "", base)
}

#' Build build phase command (fixed single-file copy with subfolders)
#'
#' Constructs the build-phase shell command for R or Python, choosing the right
#' copy mode (file vs folder) and setting the file path argument correctly.
#' For the single-file case it preserves subfolders by copying the file from its
#' path *inside* the $src directory (not $src itself).
#'
#' @param lang "R" or "Python".
#' @param read_func String representing the function to call for reading data.
#' @param copy_cmd Shell copy command for user functions (may be empty).
#' @param user_code Source/import statements for user functions.
#' @param out_name Name of the output object (RDS/pickle file).
#' @param copy_data_folder Logical, whether to copy the entire folder.
#' @param path Input path (file or folder).
#' @return A list with `actual_path` and `build_phase` string.
build_phase <- function(lang, read_func, copy_cmd, user_code, out_name,
                        copy_data_folder, path) {

  rel_path <- function(p) sub("^\\./+", "", p)

  if (!copy_data_folder) {
    # single file: copy the file *inside* $src preserving subdirs
    frel <- rel_path(path)
    actual_path <- path
    copy_line <- sprintf("cp \"$src/%s\" input_file", frel)
    arg_R  <- "input_file"
    arg_Py <- "file_path = 'input_file'"

  } else if (file.exists(path) && isTRUE(file.info(path)$isdir)) {
    actual_path <- path
    copy_line <- "cp -r $src input_folder"
    arg_R  <- "input_folder/"
    arg_Py <- "file_path = 'input_folder/'"

  } else {
    fname <- basename(path)
    actual_path <- dirname(path)
    copy_line <- "cp -r $src input_folder"
    arg_R  <- sprintf("input_folder/%s", fname)
    arg_Py <- sprintf("file_path = 'input_folder/%s'", fname)
  }

  # helper to add optional copy of user_functions (copy_cmd may be "")
  copy_cmd_block <- if (nzchar(copy_cmd)) c(copy_cmd) else character(0)

  if (lang == "R") {
    # Build Rscript lines (no extra left-margin padding)
    r_lines <- c(
      "Rscript -e \"",
      "source('libraries.R')",
      if (nzchar(user_code)) unlist(strsplit(user_code, "\n")),
      sprintf("data <- do.call(%s, list('%s'))", read_func, arg_R),
      sprintf("saveRDS(data, '%s')\"", out_name)
    )
    body_block <- c(copy_cmd_block, r_lines)

  } else if (lang == "Py") {
    # Build python lines (no leading spaces). Ensure file_path is always defined.
    py_lines <- c(
      "python -c \"",
      "exec(open('libraries.py').read())"
    )
    if (nzchar(user_code)) {
      py_lines <- c(py_lines, unlist(strsplit(user_code, "\n")))
    }
    py_lines <- c(py_lines,
                  arg_Py,
                  sprintf("data = eval('%s')(file_path)", read_func),
                  sprintf("with open('%s', 'wb') as f:", out_name),
                  "    pickle.dump(data, f)",
                  "\"")
    body_block <- c(copy_cmd_block, py_lines)

  } else {
    stop("Unsupported lang: ", lang)
  }

  # join everything without inserting extra indentation
  body <- paste(body_block, collapse = "\n")
  build_phase <- paste(copy_line, body, sep = "\n")

  list(actual_path = actual_path, build_phase = build_phase)
}

#' Generic Nix expression builder for R and Python data readers
#'
#' Creates a Nix derivation that reads a file or folder of data using R or
#' Python. Handles user-defined functions, environment variables, and Nix
#' environment specification.
#'
#' @param lang `"R"` or `"Python"`.
#' @inheritParams rxp_r_file
#' @inheritParams rxp_py_file
#' @return An object of class `rxp_derivation`.
rxp_file <- function(lang,
                     name,
                     path,
                     read_function,
                     user_functions = "",
                     nix_env = "default.nix",
                     copy_data_folder = FALSE,
                     env_var = NULL) {

  out_name <- deparse1(substitute(name))
  user_functions <- clean_user_functions(user_functions)

  # Sanitize read_function
  read_func_str <- switch(
    lang,
    R      = gsub("\"", "'", deparse1(substitute(read_function))),
    Py = {
      if (!is.character(read_function) || length(read_function) != 1)
        stop("For Python, read_function must be a single string")
      gsub("'", "\\'", read_function, fixed = TRUE)
    }
  )

  copy_cmd  <- build_user_copy_cmd(user_functions)
  user_code <- build_user_code_cmd(user_functions, lang)
  env_exports <- build_env_exports(env_var)

  # Build build_phase and actual_path
  bp <- build_phase(lang, read_func_str, copy_cmd, user_code,
                    out_name, copy_data_folder, path)

  if (env_exports != "") {
    bp$build_phase <- paste0(env_exports, bp$build_phase)
  }

  src_part <- build_src_part(bp$actual_path, user_functions)
  base <- sanitize_base(nix_env)

  snippet <- make_derivation_snippet(
    out_name = out_name,
    src_snippet = sprintf("    src = %s;\n", src_part),
    base = base,
    build_phase = bp$build_phase,
    derivation_type = lang
  )

  structure(list(
    name = out_name,
    snippet = snippet,
    type = paste0("rxp_", tolower(lang)),
    additional_files = "",
    user_functions = user_functions,
    nix_env = nix_env,
    env_var = env_var
  ), class = "rxp_derivation")
}


#' Creates a Nix expression that reads in a file (or folder of data) using R.
#'
#' @family derivations
#' @param name Symbol, the name of the derivation.
#' @param path Character, file or folder path to include.
#' @param read_function Function, R function to read the data.
#' @param user_functions Character vector of script paths to include.
#' @param nix_env Character, path to the Nix environment file.
#' @param copy_data_folder Logical, copy folder recursively if TRUE.
#' @param env_var Named list of environment variables.
#' @return An object of class `rxp_derivation`.
#' @export
rxp_r_file <- function(...) rxp_file("R", ...)

#' Creates a Nix expression that reads in a file (or folder of data) using Python.
#'
#' @family derivations
#' @param name Symbol, the name of the derivation.
#' @param path Character, file or folder path to include.
#' @param read_function Character, Python function to read the data.
#' @param user_functions Character vector of script paths to include.
#' @param nix_env Character, path to the Nix environment file.
#' @param copy_data_folder Logical, copy folder recursively if TRUE.
#' @param env_var Named list of environment variables.
#' @return An object of class `rxp_derivation`.
#' @export
rxp_py_file <- function(...) rxp_file("Py", ...)



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
#' @keywords internal
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

  snippet <- make_derivation_snippet(
    out_name = out_name,
    src_snippet = "",
    base = base,
    build_phase = build_phase,
    derivation_type = "R"
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
