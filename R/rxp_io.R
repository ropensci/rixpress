# Constants for language configuration
LANG_CONFIG <- list(
  R = list(
    source_cmd = "source('%s')",
    derivation_type = "rxp_r",
    output_ext = "RDS"
  ),
  Py = list(
    source_cmd = "exec(open('%s').read())",
    derivation_type = "rxp_py",
    output_ext = "pickle"
  ),
  Jl = list(
    source_cmd = "include('%s')",
    derivation_type = "rxp_jl",
    output_ext = "jld2"
  )
)

#' Clean user functions vector
#'
#' Removes empty character entries from the provided `user_functions` vector.
#'
#' @param user_functions Character vector of user-defined function script paths.
#' @return Character vector without empty strings.
#' @keywords internal
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
#' @keywords internal
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
#' @keywords internal
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
#' @keywords internal
build_src_part <- function(path, user_functions = character(0)) {
  if (is_remote_url(path)) {
    if (length(user_functions) > 0) {
      return(build_hybrid_src(path, user_functions))
    } else {
      return(build_remote_src(path))
    }
  } else {
    return(build_local_src(path, user_functions))
  }
}

#' Build hybrid source configuration for remote URL + local user functions
#' @param url Character URL
#' @param user_functions Character vector of additional files
#' @return Character Nix expression
#' @keywords internal
build_hybrid_src <- function(url, user_functions) {
  hash <- tryCatch(
    system(paste("nix-prefetch-url", shQuote(url)), intern = TRUE),
    error = function(e) stop("Failed to run nix-prefetch-url: ", e$message)
  )

  if (length(hash) == 0 || hash == "") {
    stop("nix-prefetch-url did not return a hash for URL: ", url)
  }

  remote_filename <- basename(url)

  # Create the local fileset for user functions
  user_files_list <- paste0("./", user_functions, collapse = " ")
  local_src_var <- sprintf(
    "localSrc = defaultPkgs.lib.fileset.toSource {\n        root = ./.;\n        fileset = defaultPkgs.lib.fileset.unions [ %s ];\n      }",
    user_files_list
  )

  # Create a single robust copy command for all user functions
  user_files_cp <- "cp -r ${localSrc}/* $out/"

  sprintf(
    "(let\n      %s;\n    in defaultPkgs.runCommand \"combined-src\" {} ''\n      mkdir -p $out\n      cp ${defaultPkgs.fetchurl {\n        url = \"%s\";\n        sha256 = \"%s\";\n      }} $out/%s\n      %s\n    '')",
    local_src_var,
    url,
    trimws(hash[1]),
    remote_filename,
    user_files_cp
  )
}

#' Check if path is a remote URL
#' @param path Character path
#' @return Logical
#' @keywords internal
is_remote_url <- function(path) {
  grepl("^https?://", path)
}

#' Build remote source configuration
#' @param url Character URL
#' @return Character Nix expression
#' @keywords internal
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
#' @keywords internal
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
#' @keywords internal
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
#' @param serialize_str Language-specific serializer identifier or NULL for default
#' @return Character vector of commands
#' @keywords internal
build_language_commands <- function(
  lang,
  read_func,
  user_code,
  out_name,
  rel_path,
  serialize_str = NULL
) {
  user_lines <- if (nzchar(user_code)) {
    strsplit(user_code, "\n")[[1]]
  } else {
    character(0)
  }

  switch(
    lang,
    R = {
      ser_fun <- if (is.null(serialize_str)) "saveRDS" else serialize_str
      ser_call <- sprintf("%s(data, '%s')", ser_fun, out_name)
      c(
        "Rscript -e \"",
        "source('libraries.R')",
        user_lines,
        sprintf("data <- do.call(%s, list('%s'))", read_func, rel_path),
        paste0(ser_call, "\"")
      )
    },
    Py = {
      if (is.null(serialize_str)) {
        ser_lines <- c(
          sprintf("with open('%s', 'wb') as f:", out_name),
          "    pickle.dump(data, f)"
        )
      } else {
        ser_lines <- c(sprintf("%s(data, '%s')", serialize_str, out_name))
      }
      c(
        "python -c \"",
        "exec(open('libraries.py').read())",
        user_lines,
        sprintf("file_path = '%s'", rel_path),
        sprintf("data = eval('%s')(file_path)", read_func),
        ser_lines,
        "\""
      )
    },
    Jl = {
      # Escape double quotes within -e string
      user_lines_jl <- gsub("'", "\\\"", user_lines, fixed = TRUE)
      read_line <- sprintf("data = %s(\\\"%s\\\")", read_func, rel_path)
      ser_line <- if (is.null(serialize_str)) {
        paste0(
          "using Serialization; ",
          "io = open(\\\"",
          out_name,
          "\\\", \\\"w\\\"); ",
          "serialize(io, data); ",
          "close(io)"
        )
      } else {
        sprintf("%s(data, \\\"%s\\\")", serialize_str, out_name)
      }
      c(
        "julia -e \"",
        "if isfile(\\\"libraries.jl\\\"); include(\\\"libraries.jl\\\"); end;",
        user_lines_jl,
        read_line,
        ser_line,
        "\""
      )
    },
    stop("Unsupported lang: ", lang)
  )
}

#' Build build_phase command
#'
#' Constructs the build-phase shell command for R, Python, or Julia.
#'
#' @param lang `"R"`, `"Py"`, or `"Jl"`.
#' @param read_func String representing the function to call for reading data.
#' @param user_code Source/import statements for user functions.
#' @param out_name Name of the output object (RDS/pickle file).
#' @param path Input path (file or folder).
#' @param user_functions Character vector of user function files.
#' @param serialize_str Language-specific serializer identifier or NULL for default.
#' @return A string with the build phase commands.
#' @keywords internal
build_phase <- function(
  lang,
  read_func,
  user_code,
  out_name,
  path,
  user_functions = character(0),
  serialize_str = NULL
) {
  if (is_remote_url(path)) {
    rel_name <- basename(path)
    rel_path <- rel_name

    if (length(user_functions) > 0) {
      # Hybrid case: everything is in $src, just organize it
      copy_line <- paste(
        "cp $src/* .",
        "mkdir -p input_folder",
        paste(
          sprintf("cp %s input_folder/", basename(user_functions)),
          collapse = "\n"
        ),
        sep = "\n"
      )
    } else {
      # Pure remote case
      copy_line <- paste0("cp -r $src ", rel_path)
    }
  } else {
    # Local case
    rel_name <- path
    rel_path <- file.path("input_folder", rel_name)
    copy_line <- "cp -r $src input_folder"
  }

  lang_commands <- build_language_commands(
    lang,
    read_func,
    user_code,
    out_name,
    rel_path,
    serialize_str
  )
  all_commands <- c(copy_line, lang_commands)

  paste(all_commands, collapse = "\n")
}

#' Process read function for different languages
#' @param read_function Function or character
#' @param lang Language string
#' @param parent_env Environment from calling function for proper substitution
#' @return Character string
#' @keywords internal
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
        gsub("\"", "", func_str, fixed = TRUE)
      }
    },
    stop("Unsupported lang: ", lang)
  )
}

#' Process serialize function for different languages
#'
#' Mirrors the behaviour of rxp_r/rxp_py/rxp_jl:
#' - R: supports bare symbol, character, or function; defaults to saveRDS
#' - Python: character function name, defaults to pickle.dump via with-open
#' - Julia: character function name, defaults to Serialization.serialize
#'
#' @param encoder Language-specific serializer (see above)
#' @param lang Language string
#' @param parent_env Environment for proper substitution (R only)
#' @return For R: function name string; For Py/Jl: NULL (use default) or function name string
#' @keywords internal
process_encoder <- function(
  encoder,
  lang,
  parent_env = parent.frame()
) {
  switch(
    lang,
    R = {
      serialize_expr <- substitute(encoder, parent_env)
      if (identical(serialize_expr, quote(NULL))) {
        "saveRDS"
      } else if (is.character(serialize_expr)) {
        serialize_expr
      } else {
        deparse1(serialize_expr)
      }
    },
    Py = {
      if (is.null(encoder)) {
        NULL
      } else {
        if (!is.character(encoder) || length(encoder) != 1) {
          stop(
            "For Python, encoder must be a single character string or NULL"
          )
        }
        encoder
      }
    },
    Jl = {
      if (is.null(encoder)) {
        NULL
      } else {
        if (!is.character(encoder) || length(encoder) != 1) {
          stop(
            "For Julia, encoder must be a single character string or NULL"
          )
        }
        encoder
      }
    },
    stop("Unsupported lang: ", lang)
  )
}

#' Generic Nix expression builder for R, Python, and Julia data readers
#'
#' You should not call it directly, but instead use one of `rxp_r_file()`,
#' `rxp_py_file()` or `rxp_jl_file()`.
#'
#' Creates a Nix derivation that reads a file or folder of data using R,
#' Python, or Julia. Handles user-defined functions, environment variables, and Nix
#' environment specification.
#'
#' @param lang `"R"`, `"Py"` or `"Jl"`.
#' @param name Symbol, the name of the derivation.
#' @param path Character, the file path to include (e.g., "data/mtcars.shp") or
#'   a folder path (e.g., "data"). See details.
#' @param read_function Function, an R function to read the data, taking one
#'   argument (the path). This can be a user-defined function that is made available
#'   using `user_functions`. See details.
#' @param user_functions Character vector, user-defined functions to include.
#'   This should be a script (or scripts) containing user-defined functions
#'   to include during the build process for this derivation. It is recommended
#'   to use one script per function, and only include the required script(s) in
#'   the derivation.
#' @param nix_env Character, path to the Nix environment file, default is
#'   "default.nix".
#' @param env_var List, defaults to NULL. A named list of environment variables
#'   to set before running the R script, e.g., c(VAR = "hello"). Each entry will
#'   be added as an export statement in the build phase.
#' @param encoder Function/character, defaults to NULL.
#'   A language-specific serializer to write the loaded object to disk.
#'   - R: function/symbol/character (e.g., `qs::qsave`) taking `(object, path)`. Defaults to `saveRDS`.
#'   - Python: character name of a function taking `(object, path)`. Defaults to using `pickle.dump`.
#'   - Julia: character name of a function taking `(object, path)`. Defaults to using `Serialization.serialize`.
#' @return An object of class `rxp_derivation`.
#' @keywords internal
rxp_file <- function(
  lang,
  name,
  path,
  read_function,
  user_functions = "",
  nix_env = "default.nix",
  env_var = NULL,
  encoder = NULL
) {
  out_name <- deparse1(substitute(name))
  path <- gsub("/+$", "", path)
  user_functions <- clean_user_functions(user_functions)
  read_func_str <- process_read_function(read_function, lang, environment())
  serialize_str <- process_encoder(
    encoder,
    lang,
    environment()
  )

  # Build components
  user_code <- build_user_code_cmd(user_functions, lang)
  env_exports <- build_env_exports(env_var)
  bp <- build_phase(
    lang,
    read_func_str,
    user_code,
    out_name,
    path,
    user_functions,
    serialize_str
  )

  # Add environment exports if present
  if (nzchar(env_exports)) {
    bp <- paste(env_exports, bp, sep = "\n")
  }

  # Build source and base identifiers
  src_part <- build_src_part(path, user_functions)
  base <- sanitize_nix_env(nix_env)

  derivation_type <- switch(
    lang,
    "R" = "rxp_r",
    "Py" = "rxp_py",
    "Jl" = "rxp_jl",
    stop("Unknown derivation type for lang: ", lang)
  )

  snippet <- make_derivation_snippet(
    out_name = out_name,
    src_snippet = sprintf("    src = %s;\n", src_part),
    base = base,
    build_phase = bp,
    derivation_type = derivation_type
  )

  # For display/debugging purposes, expose the effective serializer
  effective_serializer <- if (is.null(serialize_str)) {
    switch(
      lang,
      R = "saveRDS",
      Py = "pickle.dump",
      Jl = "Serialization.serialize"
    )
  } else {
    serialize_str
  }

  create_rxp_derivation(
    out_name,
    snippet,
    lang,
    user_functions,
    nix_env,
    env_var,
    encoder = effective_serializer
  )
}

#' Create rxp_derivation object
#' @param out_name Character output name
#' @param snippet Character Nix snippet
#' @param lang Character language
#' @param user_functions Character vector
#' @param nix_env Character nix environment
#' @param env_var Named list of environment variables
#' @param encoder Character serializer identifier (for display)
#' @return rxp_derivation object
#' @keywords internal
create_rxp_derivation <- function(
  out_name,
  snippet,
  lang,
  user_functions,
  nix_env,
  env_var,
  encoder = NULL
) {
  structure(
    list(
      name = out_name,
      snippet = snippet,
      type = paste0("rxp_", tolower(lang)),
      additional_files = "",
      user_functions = user_functions,
      nix_env = nix_env,
      env_var = env_var,
      encoder = encoder
    ),
    class = "rxp_derivation"
  )
}

#' Creates a Nix expression that reads in a file (or folder of data) using R.
#'
#' @family derivations
#' @return An object of class `rxp_derivation`.
#' @inheritDotParams rxp_file name:encoder
#' @details The basic usage is to provide a path to a file, and the function
#'   to read it. For example: `rxp_r_file(mtcars, path = "data/mtcars.csv", read_function = read.csv)`.
#'   It is also possible instead to point to a folder that contains many
#'   files that should all be read at once, for example:
#'   `rxp_r_file(many_csvs, path = "data", read_function = \(x)(readr::read_csv(list.files(x, full.names = TRUE, pattern = ".csv$"))))`.
#'   See the `vignette("importing-data")` vignette for more detailed examples.
#' @export
rxp_r_file <- function(...) rxp_file("R", ...)

#' Creates a Nix expression that reads in a file (or folder of data) using Python.
#'
#' @family derivations
#' @return An object of class `rxp_derivation`.
#' @inheritDotParams rxp_file name:encoder
#' @details The basic usage is to provide a path to a file, and the function
#'   to read it. For example: `rxp_r_file(mtcars, path = "data/mtcars.csv", read_function = read.csv)`.
#'   It is also possible instead to point to a folder that contains many
#'   files that should all be read at once, for example:
#'   `rxp_r_file(many_csvs, path = "data", read_function = \(x)(readr::read_csv(list.files(x, full.names = TRUE, pattern = ".csv$"))))`
#'   See the `vignette("importing-data")` vignette for more detailed examples.
#' @export
rxp_py_file <- function(...) rxp_file("Py", ...)

#' Creates a Nix expression that reads in a file (or folder of data) using Julia.
#'
#' @family derivations
#' @return An object of class `rxp_derivation`.
#' @inheritDotParams rxp_file name:encoder
#' @details The basic usage is to provide a path to a file, and the function
#'   to read it. For example: `rxp_r_file(mtcars, path = "data/mtcars.csv", read_function = read.csv)`.
#'   It is also possible instead to point to a folder that contains many
#'   files that should all be read at once, for example:
#'   `rxp_r_file(many_csvs, path = "data", read_function = \(x)(readr::read_csv(list.files(x, full.names = TRUE, pattern = ".csv$"))))`.
#'   See the `vignette("importing-data")` vignette for more detailed examples.
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
    derivation_type = "rxp_r"
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
#' @keywords internal
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
#' @keywords internal
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
