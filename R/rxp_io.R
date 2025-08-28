# Constants for language configuration
LANG_CONFIG <- list(
  R = list(
    source_cmd = "source('%s')",
    derivation_type = "R",
    output_ext = "RDS"
  ),
  Py = list(
    source_cmd = "exec(open('%s').read())",
    derivation_type = "Py",
    output_ext = "pickle"
  ),
  Jl = list(
    source_cmd = "include('%s')",
    derivation_type = "Jl",
    output_ext = "jld2"
  )
)

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
#' @param lang Language string, `"R"`, `"Py"`, or `"Jl"`.
#' @return A string of import/source statements.
build_user_code_cmd <- function(user_functions, lang) {
  if (length(user_functions) == 0) {
    return("")
  }

  if (!lang %in% names(LANG_CONFIG)) {
    stop("Unsupported lang: ", lang)
  }

  files <- file.path("input_folder", user_functions)
  commands <- sprintf(LANG_CONFIG[[lang]]$source_cmd, files)
  paste(commands, collapse = "\n")
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

  exports <- sprintf("export %s=%s", names(env_var), env_var)
  paste0(paste(exports, collapse = "\n"), "\n")
}

#' Build Nix src part
#'
#' Creates the `src` attribute for the Nix derivation. Handles both remote URLs
#' (using `nix-prefetch-url`) and local paths (with optional user function
#' scripts).
#'
#' @param path Character path to file or directory.
#' @param user_functions Character vector of additional files to include.
#' @return A string with the `src` Nix expression.
build_src_part <- function(path, user_functions = character(0)) {
  if (is_remote_url(path)) {
    return(build_remote_src(path))
  } else {
    return(build_local_src(path, user_functions))
  }
}

#' Check if path is a remote URL
#' @param path Character path
#' @return Logical
is_remote_url <- function(path) {
  grepl("^https?://", path)
}

#' Build remote source configuration
#' @param url Character URL
#' @return Character Nix expression
build_remote_src <- function(url) {
  hash <- tryCatch(
    system(paste("nix-prefetch-url", shQuote(url)), intern = TRUE),
    error = function(e) stop("Failed to run nix-prefetch-url: ", e$message)
  )

  if (length(hash) == 0 || hash == "") {
    stop("nix-prefetch-url did not return a hash for URL: ", url)
  }

  sprintf(
    "defaultPkgs.fetchurl {\n      url = \"%s\";\n      sha256 = \"%s\";\n    }",
    url,
    trimws(hash[1])
  )
}

#' Build local source configuration
#' @param path Character path
#' @param user_functions Character vector of additional files
#' @return Character Nix expression
build_local_src <- function(path, user_functions) {
  all_files <- c(path, user_functions)
  all_files <- all_files[nzchar(all_files)]
  fileset_nix <- paste0("./", all_files, collapse = " ")

  sprintf(
    "defaultPkgs.lib.fileset.toSource {\n      root = ./.;\n      fileset = defaultPkgs.lib.fileset.unions [ %s ];\n    }",
    fileset_nix
  )
}

#' Sanitize nix environment string
#'
#' Produces a base identifier string by replacing invalid characters and
#' stripping suffixes.
#'
#' @param nix_env Character path to a Nix environment file (e.g., `"default.nix"`).
#' @return Sanitized base string.
sanitize_nix_env <- function(nix_env) {
  base <- gsub("[^a-zA-Z0-9]", "_", nix_env)
  sub("_nix$", "", base)
}

#' Build language-specific execution commands
#' @param lang Language string
#' @param read_func String representing the function to call
#' @param user_code Source/import statements for user functions
#' @param out_name Name of the output object
#' @param rel_path Relative path to input file
#' @return Character vector of commands
build_language_commands <- function(
  lang,
  read_func,
  user_code,
  out_name,
  rel_path
) {
  user_lines <- if (nzchar(user_code)) {
    strsplit(user_code, "\n")[[1]]
  } else {
    character(0)
  }

  switch(
    lang,
    R = c(
      "Rscript -e \"",
      "source('libraries.R')",
      user_lines,
      sprintf("data <- do.call(%s, list('%s'))", read_func, rel_path),
      sprintf("saveRDS(data, '%s')\"", out_name)
    ),
    Py = c(
      "python -c \"",
      "exec(open('libraries.py').read())",
      user_lines,
      sprintf("file_path = '%s'", rel_path),
      sprintf("data = eval('%s')(file_path)", read_func),
      sprintf("with open('%s', 'wb') as f:", out_name),
      "    pickle.dump(data, f)",
      "\""
    ),
    Jl = c(
      "# Julia derivation build",
      user_lines,
      sprintf("data = %s(\"%s\")", read_func, rel_path)
    ),
    stop("Unsupported lang: ", lang)
  )
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
  rel_path <- file.path("input_folder", path)
  copy_line <- "cp -r $src input_folder"

  lang_commands <- build_language_commands(
    lang,
    read_func,
    user_code,
    out_name,
    rel_path
  )
  all_commands <- c(copy_line, lang_commands)

  paste(all_commands, collapse = "\n")
}

#' Process read function for different languages
#' @param read_function Function or character
#' @param lang Language string
#' @param parent_env Environment from calling function for proper substitution
#' @return Character string
process_read_function <- function(
  read_function,
  lang,
  parent_env = parent.frame()
) {
  switch(
    lang,
    R = {
      # Get the actual expression from the parent environment
      read_expr <- substitute(read_function, parent_env)

      # Convert to string, handling both symbols and function definitions
      if (is.symbol(read_expr)) {
        # Simple function name like readRDS
        as.character(read_expr)
      } else {
        # Function definition or call - need to deparse properly
        func_str <- deparse1(read_expr)
        # Join multi-line deparsed functions and clean up
        func_str <- paste(func_str, collapse = " ")
        # Replace double quotes with single quotes for Nix compatibility
        gsub("\"", "'", func_str, fixed = TRUE)
      }
    },
    Py = {
      if (!is.character(read_function) || length(read_function) != 1) {
        stop("For Python, read_function must be a single string")
      }
      gsub("'", "\\'", read_function, fixed = TRUE)
    },
    Jl = {
      read_expr <- substitute(read_function, parent_env)
      if (is.symbol(read_expr)) {
        as.character(read_expr)
      } else {
        func_str <- deparse1(read_expr)
        func_str <- paste(func_str, collapse = " ")
        gsub("\"", "'", func_str, fixed = TRUE)
      }
    },
    stop("Unsupported lang: ", lang)
  )
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
  read_func_str <- process_read_function(read_function, lang, environment())

  # Build components
  user_code <- build_user_code_cmd(user_functions, lang)
  env_exports <- build_env_exports(env_var)
  bp <- build_phase(lang, read_func_str, user_code, out_name, path)

  # Add environment exports if present
  if (nzchar(env_exports)) {
    bp <- paste(env_exports, bp, sep = "\n")
  }

  # Build source and base identifiers
  src_part <- build_src_part(path, user_functions)
  base <- sanitize_nix_env(nix_env)

  snippet <- make_derivation_snippet(
    out_name = out_name,
    src_snippet = sprintf("    src = %s;\n", src_part),
    base = base,
    build_phase = bp,
    derivation_type = lang
  )

  create_rxp_derivation(
    out_name,
    snippet,
    lang,
    user_functions,
    nix_env,
    env_var
  )
}

#' Create rxp_derivation object
#' @param out_name Character output name
#' @param snippet Character Nix snippet
#' @param lang Character language
#' @param user_functions Character vector
#' @param nix_env Character nix environment
#' @param env_var Named list of environment variables
#' @return rxp_derivation object
create_rxp_derivation <- function(
  out_name,
  snippet,
  lang,
  user_functions,
  nix_env,
  env_var
) {
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
rxp_common_setup <- function(out_name, expr_str, nix_env, direction) {
  expr_str <- gsub("\"", "'", expr_str)
  base <- sanitize_nix_env(nix_env)

  r_command <- build_transfer_command(out_name, expr_str, direction)
  build_phase <- build_reticulate_phase(r_command)

  snippet <- make_derivation_snippet(
    out_name = out_name,
    src_snippet = "",
    base = base,
    build_phase = build_phase,
    derivation_type = "R"
  )

  structure(
    list(
      name = out_name,
      snippet = snippet,
      type = paste0("rxp_", direction),
      additional_files = "",
      nix_env = nix_env
    ),
    class = "rxp_derivation"
  )
}

#' Build transfer command for py2r or r2py
#' @param out_name Character output name
#' @param expr_str Character expression string
#' @param direction Character direction
#' @return Character command
build_transfer_command <- function(out_name, expr_str, direction) {
  switch(
    direction,
    py2r = sprintf(
      "         %s <- reticulate::py_load_object('${%s}/%s', pickle = 'pickle', convert = TRUE)\n         saveRDS(%s, '%s')",
      out_name,
      expr_str,
      expr_str,
      out_name,
      out_name
    ),
    r2py = sprintf(
      "         %s <- readRDS('${%s}/%s')\n         reticulate::py_save_object(%s, '%s', pickle = 'pickle')",
      expr_str,
      expr_str,
      expr_str,
      expr_str,
      out_name
    ),
    stop("Invalid direction. Use 'py2r' or 'r2py'.")
  )
}

#' Build reticulate build phase
#' @param r_command Character R command
#' @return Character build phase
build_reticulate_phase <- function(r_command) {
  sprintf(
    "export RETICULATE_PYTHON=${defaultPkgs.python3}/bin/python\n       Rscript -e \"\n         source('libraries.R')\n%s\"",
    r_command
  )
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
#' @export
rxp_r2py <- function(name, expr, nix_env = "default.nix") {
  out_name <- deparse1(substitute(name))
  expr_str <- deparse1(substitute(expr))
  rxp_common_setup(out_name, expr_str, nix_env, "r2py")
}
