#' Clean user functions vector
#'
#' Removes empty character entries from the provided `user_functions` vector.
#'
#' @param user_functions Character vector of user-defined function script paths.
#' @return Character vector without empty strings.
clean_user_functions <- function(user_functions) {
  if (is.null(user_functions)) {
    return(character(0))
  }
  user_functions[nzchar(user_functions)]
}

#' Build code import/source commands for user functions
#'
#' Creates language-specific commands to load user-defined function scripts.
#'
#' @param user_functions Character vector of script file names.
#' @param lang Language string, `"R"`, `"Python"` and `"Julia"`.
#' @return A string of import/source statements.
build_user_code_cmd <- function(user_functions, lang) {
  if (length(user_functions) == 0) {
    return("")
  }
  # Always prepend input_folder/
  files <- paste0("input_folder/", user_functions)
  fmt <- switch(
    lang,
    R = "source('%s')",
    Py = "exec(open('%s').read())",
    Jl = "include('%s')",
    stop("Unsupported lang: ", lang)
  )
  paste(sprintf(fmt, files), collapse = "\n")
}

#' Build environment variable export commands
#'
#' Generates shell export commands for setting environment variables before
#' running the build script.
#'
#' @param env_var Named list of environment variables.
#' @return A string of `export` commands with line breaks, or `""` if none.
build_env_exports <- function(env_var) {
  if (is.null(env_var) || length(env_var) == 0) {
    return("")
  }
  lines <- vapply(
    names(env_var),
    function(var) {
      sprintf("export %s=%s", var, env_var[[var]])
    },
    character(1)
  )
  paste0(paste(lines, collapse = "\n"), "\n")
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
    if (length(hash) == 0 || hash == "") {
      stop("nix-prefetch-url did not return a hash for URL: ", actual_path)
    }
    sprintf(
      "defaultPkgs.fetchurl {\n      url = \"%s\";\n      sha256 = \"%s\";\n    }",
      actual_path,
      trimws(hash[1])
    )
  } else {
    all_files <- c(actual_path, user_functions)
    all_files <- all_files[nzchar(all_files)]
    fileset_nix <- paste0("./", all_files, collapse = " ")
    sprintf(
      "defaultPkgs.lib.fileset.toSource {\n      root = ./.;\n      fileset = defaultPkgs.lib.fileset.unions [ %s ];\n    }",
      fileset_nix
    )
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

#' Build build phase command
#'
#' Constructs the build-phase shell command for R, Python, or Julia.
#'
#' @param lang `"R"`, `"Py"`, or `"Jl"`.
#' @param read_func String representing the function to call for reading data.
#' @param user_code Source/import statements for user functions.
#' @param out_name Name of the output object (RDS/pickle file).
#' @param path Input path (file or folder).
#' @return A string with the build phase commands.
build_phase <- function(lang, read_func, user_code, out_name, path) {
  rel_path <- paste0("input_folder/", path) # everything under input_folder
  copy_line <- "cp -r $src input_folder"

  # User scripts
  copy_cmd_block <- character(0) # fileset handles everything now

  if (lang == "R") {
    r_lines <- c(
      "Rscript -e \"",
      "source('libraries.R')",
      if (nzchar(user_code)) unlist(strsplit(user_code, "\n")),
      sprintf("data <- do.call(%s, list('%s'))", read_func, rel_path),
      sprintf("saveRDS(data, '%s')\"", out_name)
    )
    body_block <- c(copy_cmd_block, copy_line, r_lines)
  } else if (lang == "Py") {
    py_lines <- c(
      "python -c \"",
      "exec(open('libraries.py').read())",
      if (nzchar(user_code)) unlist(strsplit(user_code, "\n")),
      sprintf("file_path = '%s'", rel_path),
      sprintf("data = eval('%s')(file_path)", read_func),
      sprintf("with open('%s', 'wb') as f:", out_name),
      "    pickle.dump(data, f)",
      "\""
    )
    body_block <- c(copy_cmd_block, copy_line, py_lines)
  } else if (lang == "Jl") {
    jl_lines <- c(
      "# Julia derivation build",
      copy_line,
      if (nzchar(user_code)) unlist(strsplit(user_code, "\n")),
      sprintf("data = %s(\"%s\")", read_func, rel_path)
    )
    body_block <- c(copy_cmd_block, jl_lines)
  } else {
    stop("Unsupported lang: ", lang)
  }

  paste(body_block, collapse = "\n")
}

#' Generic Nix expression builder for R, Python, and Julia data readers
#'
#' Creates a Nix derivation that reads a file or folder of data using R,
#' Python, or Julia. Handles user-defined functions, environment variables, and Nix
#' environment specification.
#'
#' @param lang `"R"`, `"Py"` or `"Jl"`.
#' @inheritParams rxp_r_file
#' @inheritParams rxp_py_file
#' @inheritParams rxp_jl_file
#' @return An object of class `rxp_derivation`.
rxp_file <- function(
  lang,
  name,
  path,
  read_function,
  user_functions = "",
  nix_env = "default.nix",
  env_var = NULL
) {
  out_name <- deparse1(substitute(name))
  user_functions <- clean_user_functions(user_functions)

  read_func_str <- switch(
    lang,
    R = gsub("\"", "'", deparse1(substitute(read_function))),
    Py = {
      if (!is.character(read_function) || length(read_function) != 1) {
        stop("For Python, read_function must be a single string")
      }
      gsub("'", "\\'", read_function, fixed = TRUE)
    },
    Jl = gsub("\"", "'", deparse1(substitute(read_function)))
  )

  user_code <- build_user_code_cmd(user_functions, lang)
  env_exports <- build_env_exports(env_var)
  bp <- build_phase(lang, read_func_str, user_code, out_name, path)
  if (nzchar(env_exports)) {
    bp <- paste(env_exports, bp, sep = "\n")
  }

  src_part <- build_src_part(path, user_functions)
  base <- sub("_nix$", "", gsub("[^a-zA-Z0-9]", "_", nix_env))

  snippet <- make_derivation_snippet(
    out_name = out_name,
    src_snippet = sprintf("    src = %s;\n", src_part),
    base = base,
    build_phase = bp,
    derivation_type = lang
  )

  structure(
    list(
      name = out_name,
      snippet = snippet,
      type = paste0("rxp_", tolower(lang)),
      additional_files = "",
      user_functions = user_functions,
      nix_env = nix_env,
      env_var = env_var
    ),
    class = "rxp_derivation"
  )
}


#' Creates a Nix expression that reads in a file (or folder of data) using R.
#'
#' @family derivations
#' @param name Symbol, the name of the derivation.
#' @param path Character, file or folder path to include.
#' @param read_function Function, R function to read the data.
#' @param user_functions Character vector of script paths to include.
#' @param nix_env Character, path to the Nix environment file.
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
#' @param env_var Named list of environment variables.
#' @return An object of class `rxp_derivation`.
#' @export
rxp_py_file <- function(...) rxp_file("Py", ...)

#' Creates a Nix expression that reads in a file (or folder of data) using Julia.
#'
#' @family derivations
#' @param name Symbol, the name of the derivation.
#' @param path Character, file or folder path to include.
#' @param read_function Function, Julia function to read the data.
#' @param user_functions Character vector of script paths to include.
#' @param nix_env Character, path to the Nix environment file.
#' @param env_var Named list of environment variables.
#' @return An object of class `rxp_derivation`.
#' @export
rxp_jl_file <- function(...) rxp_file("Jl", ...)


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
#' @return An object of class `rxp_derivation`.
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
#' @return An object of class `rxp_derivation`.
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
