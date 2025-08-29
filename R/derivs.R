#' Helper function to generate a Nix derivation snippet
#'
#' @param out_name Character, name of the derivation
#' @param src_snippet Character, the src part of the derivation
#' @param base Character, base name for buildInputs and configurePhase
#' @param build_phase Character, the build phase commands
#' @param derivation_type Character, one of "rxp_r", "rxp_py", "rxp_jl", "rxp_qmd", "rxp_rmd"
#' @param noop_build Logical, defaults to FALSE. If TRUE, the derivation
#'   produces a no-op build (a stub output with no actual build steps). Any
#'   downstream derivations depending on a no-op build will themselves also
#'   become no-op builds.
#' @return Character string with the formatted Nix derivation
#' @keywords internal
make_derivation_snippet <- function(
  out_name,
  src_snippet,
  base,
  build_phase,
  derivation_type,
  noop_build = FALSE
) {
  # If noop_build is TRUE, create a dummy derivation that just creates an empty output
  if (noop_build) {
    return(sprintf(
      "  %s = defaultPkgs.runCommand \"%s\" {} \"\n    mkdir -p $out\n    echo 'Build skipped for %s' > $out/NOOPBUILD\n  \";",
      out_name,
      out_name,
      out_name
    ))
  }

  # Normal build logic for when noop_build = FALSE
  # Determine the derivation function based on type
  derivation_func <- switch(
    derivation_type,
    "rxp_r" = "makeRDerivation",
    "rxp_py" = "makePyDerivation",
    "rxp_jl" = "makeJlDerivation",
    "rxp_qmd" = "defaultPkgs.stdenv.mkDerivation",
    "rxp_rmd" = "defaultPkgs.stdenv.mkDerivation",
    stop("Unknown derivation type: ", derivation_type)
  )

  # Format the build phase with appropriate indentation
  formatted_build_phase <- if (derivation_type %in% c("rxp_qmd", "rxp_rmd")) {
    paste0("\n", build_phase, "\n    ")
  } else {
    paste0("\n      ", build_phase, "\n    ")
  }

  # Generate the snippet
  sprintf(
    "  %s = %s {\n    name = \"%s\";\n%s    buildInputs = %sBuildInputs;\n    configurePhase = %sConfigurePhase;\n    buildPhase = ''%s'';\n  };",
    out_name,
    derivation_func,
    out_name,
    src_snippet,
    base,
    base,
    formatted_build_phase
  )
}


#' Helper function to parse unserialize_function parameter
#'
#' @param unserialize_expr Expression from substitute(unserialize_function)
#' @param default_func Default function to use if NULL
#' @param parent_env Parent environment for evaluation
#' @return Processed unserialize string or list
#' @keywords internal
parse_unserialize_function <- function(
  unserialize_expr,
  default_func,
  parent_env = parent.frame()
) {
  if (identical(unserialize_expr, quote(NULL))) {
    return(default_func)
  }

  # Check if it's a call to c() with named arguments
  if (is.call(unserialize_expr) && identical(unserialize_expr[[1]], quote(c))) {
    call_names <- names(unserialize_expr)
    if (!is.null(call_names) && any(nzchar(call_names[-1]))) {
      # Has named arguments - extract as named list
      unserialize_str <- list()
      for (i in 2:length(unserialize_expr)) {
        if (nzchar(call_names[i])) {
          val <- unserialize_expr[[i]]
          unserialize_str[[call_names[i]]] <- if (is.character(val)) {
            val
          } else {
            as.character(val)
          }
        }
      }
      return(as.list(unserialize_str))
    } else {
      # No names, treat as single value
      if (length(unserialize_expr) > 1) {
        val <- unserialize_expr[[2]]
        return(if (is.character(val)) val else as.character(val))
      }
      return(deparse1(unserialize_expr))
    }
  }

  if (is.character(unserialize_expr)) {
    return(unserialize_expr)
  }

  # Try to evaluate it
  tryCatch(
    {
      unserialize_val <- eval(unserialize_expr, envir = parent_env)
      if (
        !is.null(names(unserialize_val)) && length(names(unserialize_val)) > 0
      ) {
        as.list(setNames(unserialize_val, names(unserialize_val)))
      } else {
        if (is.character(unserialize_val)) {
          unserialize_val
        } else {
          deparse1(unserialize_expr)
        }
      }
    },
    error = function(e) {
      deparse1(unserialize_expr)
    }
  )
}

#' Build environment variable export statements
#'
#' @param env_var Named vector of environment variables
#' @param indent Number of spaces for indentation
#' @return String with export statements
#' @keywords internal
build_env_exports <- function(env_var, indent = 6) {
  if (is.null(env_var) || length(env_var) == 0) {
    return("")
  }

  indent_str <- paste(rep(" ", indent), collapse = "")
  exports <- vapply(
    names(env_var),
    function(var) sprintf("export %s=%s", var, env_var[[var]]),
    character(1)
  )

  paste0(
    paste(paste0(indent_str, exports), collapse = "\n"),
    "\n",
    indent_str
  )
}

#' Build copy commands for files
#'
#' @param files Character vector of files to copy
#' @param recursive Logical, use cp -r if TRUE
#' @param indent Number of spaces for indentation
#' @return String with copy commands
#' @keywords internal
build_copy_commands <- function(files, recursive = TRUE, indent = 6) {
  if (is.null(files) || !any(nzchar(files))) {
    return("")
  }

  files_clean <- files[nzchar(files)]
  if (length(files_clean) == 0) {
    return("")
  }

  indent_str <- paste(rep(" ", indent), collapse = "")
  cp_flag <- if (recursive) "cp -r" else "cp"

  copy_lines <- vapply(
    files_clean,
    function(f) sprintf("%s ${./%s} %s", cp_flag, f, f),
    character(1)
  )

  paste0(
    paste(paste0(indent_str, copy_lines), collapse = "\n"),
    "\n",
    indent_str
  )
}

#' Build source/import commands for user functions
#'
#' @param user_functions Character vector of function files
#' @param lang Language: "R", "Py", or "Jl"
#' @param indent Number of spaces for indentation
#' @return String with source/import commands
#' @keywords internal
build_source_commands <- function(user_functions, lang, indent = 8) {
  if (is.null(user_functions) || !any(nzchar(user_functions))) {
    return("")
  }

  user_functions_clean <- user_functions[nzchar(user_functions)]
  if (length(user_functions_clean) == 0) {
    return("")
  }

  indent_str <- paste(rep(" ", indent), collapse = "")

  commands <- switch(
    lang,
    "R" = vapply(
      user_functions_clean,
      function(f) sprintf("source('%s')", f),
      character(1)
    ),
    "Py" = vapply(
      user_functions_clean,
      function(f) sprintf("exec(open('%s').read())", f),
      character(1)
    ),
    "Jl" = vapply(
      user_functions_clean,
      function(f) sprintf("include(\\\"%s\\\")", f),
      character(1)
    ),
    stop("Unknown language: ", lang)
  )

  if (lang == "Jl") {
    # Julia uses semicolon separator on same line
    paste0(paste(commands, collapse = "; "), "; ")
  } else {
    # R and Python use newlines
    paste0(
      paste(paste0(indent_str, commands), collapse = "\n"),
      "\n",
      indent_str
    )
  }
}

#' Build src snippet for Nix derivation
#'
#' @param fileset_parts Character vector of files to include
#' @return String with src snippet or empty string
#' @keywords internal
build_src_snippet <- function(fileset_parts) {
  if (length(fileset_parts) == 0) {
    return("")
  }

  fileset_nix <- paste0("./", fileset_parts, collapse = " ")
  sprintf(
    "     src = defaultPkgs.lib.fileset.toSource {\n      root = ./.;\n      fileset = defaultPkgs.lib.fileset.unions [ %s ];\n    };\n",
    fileset_nix
  )
}

#' Sanitize Nix environment name to valid identifier
#'
#' @param nix_env Nix environment file path
#' @return Sanitized base name
#' @keywords internal
sanitize_nix_base <- function(nix_env) {
  base <- gsub("[^a-zA-Z0-9]", "_", nix_env)
  sub("_nix$", "", base)
}

#' Create derivation object
#'
#' @param name Derivation name
#' @param snippet Nix snippet
#' @param type Derivation type
#' @param additional_files Additional files
#' @param user_functions User function files
#' @param nix_env Nix environment
#' @param serialize_function Serialize function
#' @param unserialize_function Unserialize function
#' @param env_var Environment variables
#' @param noop_build No-op build flag
#' @param ... Additional fields for specific derivation types
#' @return Derivation object with class "rxp_derivation"
#' @keywords internal
create_derivation <- function(
  name,
  snippet,
  type,
  additional_files = "",
  user_functions = "",
  nix_env = "default.nix",
  serialize_function = NULL,
  unserialize_function = NULL,
  env_var = NULL,
  noop_build = FALSE,
  ...
) {
  base_list <- list(
    name = name,
    snippet = snippet,
    type = type,
    additional_files = additional_files,
    nix_env = nix_env,
    noop_build = noop_build
  )

  # Add optional fields if they exist
  if (!is.null(user_functions) && any(nzchar(user_functions))) {
    base_list$user_functions <- user_functions
  }
  if (!is.null(serialize_function)) {
    base_list$serialize_function <- serialize_function
  }
  if (!is.null(unserialize_function)) {
    base_list$unserialize_function <- unserialize_function
  }
  if (!is.null(env_var)) {
    base_list$env_var <- env_var
  }

  # Add any additional type-specific fields
  extra_fields <- list(...)
  if (length(extra_fields) > 0) {
    base_list <- c(base_list, extra_fields)
  }

  structure(base_list, class = "rxp_derivation")
}

#' Create a Nix expression running an R function
#' @family derivations
#' @param name Symbol, name of the derivation.
#' @param expr R code to generate the expression.
#' @param additional_files Character vector, additional files to include
#'   during the build process. For example, if a function expects a certain
#'   file to be available, this is where you should include it.
#' @param user_functions Character vector, user-defined functions to include.
#'   This should be a script (or scripts) containing user-defined functions
#'   to include during the build process for this derivation. It is recommended
#'   to use one script per function, and only include the required script(s) in
#'   the derivation.
#' @param nix_env Character, path to the Nix environment file, default is
#'   "default.nix".
#' @param serialize_function Function, defaults to NULL. A function used to
#'   serialize objects for transfer between derivations. It must accept two
#'   arguments: the object to serialize (first), and the target file path
#'   (second). If your function has a different signature, wrap it to match this
#'   interface. By default, `saveRDS()` is used, but this may yield unexpected
#'   results, especially for complex objects like machine learning models. For
#'   instance, for `{keras}` models, use `keras::save_model_hdf5()` to capture
#'   the full model (architecture, weights, training config, optimiser state,
#'   etc.).
#' @param unserialize_function Function, character, or named vector/list,
#'   defaults to NULL. Can be:
#'   - A single function/string to unserialize all upstream objects (e.g., `readRDS`)
#'   - A named vector/list where names are upstream dependency names and values
#'     are their specific unserialize functions (e.g.,
#'     `c(mtcars_tail = "qs::qread", mtcars_head = "read.csv")`)
#'   By default, `readRDS()` is used.
#' @param env_var Character vector, defaults to NULL. A named vector of
#'   environment variables to set before running the R script, e.g.,
#'   `c("CMDSTAN" = "${defaultPkgs.cmdstan}/opt/cmdstan)"`.
#'   Each entry will be added as an export statement in the build phase.
#' @param noop_build Logical, defaults to FALSE. If TRUE, the derivation
#'   produces a no-op build (a stub output with no actual build steps). Any
#'   downstream derivations depending on a no-op build will themselves also
#'   become no-op builds.
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
#'   # Skip building this derivation
#'   rxp_r(
#'     name = turtles,
#'     expr = occurrence(species, geometry = atlantic),
#'     noop_build = TRUE
#'   )
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
  user_functions = "",
  nix_env = "default.nix",
  serialize_function = NULL,
  unserialize_function = NULL,
  env_var = NULL,
  noop_build = FALSE
) {
  out_name <- deparse1(substitute(name))
  expr_str <- deparse1(substitute(expr))
  expr_str <- gsub("\"", "'", expr_str)
  expr_str <- gsub("$", "\\$", expr_str, fixed = TRUE)

  # Parse serialize function
  serialize_expr <- substitute(serialize_function)
  serialize_str <- if (identical(serialize_expr, quote(NULL))) {
    "saveRDS"
  } else if (is.character(serialize_expr)) {
    serialize_expr
  } else {
    deparse1(serialize_expr)
  }

  # Parse unserialize function using helper
  unserialize_str <- parse_unserialize_function(
    substitute(unserialize_function),
    "readRDS",
    parent.frame()
  )

  # Build components
  env_exports <- build_env_exports(env_var)
  copy_cmd <- build_copy_commands(additional_files, recursive = TRUE)
  user_copy_cmd <- build_copy_commands(user_functions, recursive = FALSE)
  source_cmd <- build_source_commands(user_functions, "R")

  # Build phase
  unique_placeholder <- sprintf(
    "# RIXPRESS_LOAD_DEPENDENCIES_HERE:%s",
    out_name
  )

  build_phase <- sprintf(
    "%s%s%sRscript -e \"\n        source('libraries.R')\n        %s\n        %s%s <- %s\n        %s(%s, '%s')\"",
    env_exports,
    copy_cmd,
    user_copy_cmd,
    unique_placeholder,
    source_cmd,
    out_name,
    expr_str,
    serialize_str,
    out_name,
    out_name
  )

  # Prepare fileset and src
  all_files <- c(additional_files, user_functions)
  fileset_parts <- all_files[nzchar(all_files)]
  src_snippet <- build_src_snippet(fileset_parts)

  # Generate snippet
  base <- sanitize_nix_base(nix_env)
  snippet <- make_derivation_snippet(
    out_name = out_name,
    src_snippet = src_snippet,
    base = base,
    build_phase = build_phase,
    derivation_type = "rxp_r",
    noop_build = noop_build
  )

  create_derivation(
    name = out_name,
    snippet = snippet,
    type = "rxp_r",
    additional_files = additional_files,
    user_functions = user_functions,
    nix_env = nix_env,
    serialize_function = serialize_str,
    unserialize_function = unserialize_str,
    env_var = env_var,
    noop_build = noop_build
  )
}

#' Create a Nix expression running a Python function
#'
#' @family derivations
#' @param name Symbol, name of the derivation.
#' @param py_expr Character, Python code to generate the expression.
#' @param additional_files Character vector, additional files to include
#'   during the build process. For example, if a function expects a certain
#'   file to be available, this is where you should include it.
#' @param user_functions Character vector, user-defined functions to include.
#'   This should be a script (or scripts) containing user-defined functions
#'   to include during the build process for this derivation. It is recommended
#'   to use one script per function, and only include the required script(s) in
#'   the derivation.
#' @param nix_env Character, path to the Nix environment file, default is
#'   "default.nix".
#' @param serialize_function Character, defaults to NULL. The name of the Python
#'   function used to serialize the object. It must accept two arguments: the
#'   object to serialize (first), and the target file path (second). If NULL,
#'   the default behaviour uses `pickle.dump`. Define this function in
#'   `functions.py`.
#' @param unserialize_function Character or named vector/list, defaults to NULL. Can be:
#'   - A single string for the Python function to unserialize all upstream objects
#'   - A named vector/list where names are upstream dependency names and values
#'     are their specific unserialize functions
#'   If NULL, the default uses `pickle.load`.
#' @param env_var Character vector, defaults to NULL. A named vector of
#'   environment variables
#'   before running the Python script, e.g., c(PYTHONPATH = "/path/to/modules").
#'   Each entry will be added as an export statement in the build phase.
#' @param noop_build Logical, defaults to FALSE. If TRUE, the derivation
#'   produces a no-op build (a stub output with no actual build steps). Any
#'   downstream derivations depending on a no-op build will themselves also
#'   become no-op builds.
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
#'   # Skip building this derivation
#'   rxp_py(
#'     data_prep,
#'     py_expr = "preprocess_data(raw_data)",
#'     noop_build = TRUE
#'   )
#'
#'   # Custom serialization
#'   rxp_py(
#'     mtcars_pl_am,
#'     py_expr = "mtcars_pl.filter(polars.col('am') == 1).to_pandas()",
#'     user_functions = "functions.py",
#'     serialize_function = "serialize_model",
#'     additional_files = "some_required_file.bin")
#' }
#' @export
rxp_py <- function(
  name,
  py_expr,
  additional_files = "",
  user_functions = "",
  nix_env = "default.nix",
  serialize_function = NULL,
  unserialize_function = NULL,
  env_var = NULL,
  noop_build = FALSE
) {
  out_name <- deparse1(substitute(name))
  py_expr <- gsub("'", "\\'", py_expr, fixed = TRUE)

  # Parse serialize function
  serialize_str <- if (is.null(serialize_function)) {
    sprintf(
      "with open('%s', 'wb') as f: pickle.dump(globals()['%s'], f)",
      out_name,
      out_name
    )
  } else {
    if (!is.character(serialize_function)) {
      stop("serialize_function must be a character string or NULL")
    }
    sprintf("%s(globals()['%s'], '%s')", serialize_function, out_name, out_name)
  }

  # Parse unserialize function using helper
  unserialize_str <- parse_unserialize_function(
    substitute(unserialize_function),
    "pickle.load",
    parent.frame()
  )

  # Build components
  env_exports <- build_env_exports(env_var)
  copy_cmd <- build_copy_commands(additional_files, recursive = TRUE)
  user_copy_cmd <- build_copy_commands(user_functions, recursive = FALSE)
  user_import_cmd <- if (
    !is.null(user_functions) && any(nzchar(user_functions))
  ) {
    paste0(build_source_commands(user_functions, "Py", indent = 0), "\n")
  } else {
    ""
  }

  # Build phase
  unique_placeholder <- sprintf(
    "# RIXPRESS_PY_LOAD_DEPENDENCIES_HERE:%s",
    out_name
  )

  build_phase <- paste0(
    env_exports,
    copy_cmd,
    user_copy_cmd,
    "python -c \"\n",
    "exec(open('libraries.py').read())\n",
    unique_placeholder,
    "\n",
    user_import_cmd,
    "exec('",
    out_name,
    " = ",
    py_expr,
    "')\n",
    serialize_str,
    "\n\""
  )

  # Prepare fileset and src
  fileset_parts <- c(
    if (!is.null(additional_files) && any(nzchar(additional_files))) {
      additional_files[nzchar(additional_files)]
    },
    if (!is.null(user_functions) && any(nzchar(user_functions))) {
      user_functions[nzchar(user_functions)]
    }
  )
  src_snippet <- build_src_snippet(fileset_parts)

  # Generate snippet
  base <- sanitize_nix_base(nix_env)
  snippet <- make_derivation_snippet(
    out_name = out_name,
    src_snippet = src_snippet,
    base = base,
    build_phase = build_phase,
    derivation_type = "rxp_py",
    noop_build = noop_build
  )

  create_derivation(
    name = out_name,
    snippet = snippet,
    type = "rxp_py",
    additional_files = additional_files,
    user_functions = user_functions,
    nix_env = nix_env,
    serialize_function = serialize_str,
    unserialize_function = unserialize_str,
    env_var = env_var,
    noop_build = noop_build
  )
}

#' Create a Nix expression running a Julia function
#'
#' @param name Symbol, name of the derivation.
#' @param jl_expr Character, Julia code to generate the expression.
#' @param additional_files Character vector, additional files to include
#'   during the build process. For example, if a function expects a certain
#'   file to be available, this is where you should include it.
#' @param user_functions Character vector, user-defined functions to include.
#'   This should be a script (or scripts) containing user-defined functions
#'   to include during the build process for this derivation. It is recommended
#'   to use one script per function, and only include the required script(s) in
#'   the derivation.
#' @param nix_env Character, path to the Nix environment file, default is
#'   "default.nix".
#' @param serialize_function Character, defaults to NULL. The name of the Julia
#'   function used to serialize the object. It must accept two arguments: the
#'   object to serialize (first), and the target file path (second). If NULL,
#'   the default behaviour uses the built‐in `Serialization.serialize` API. Define
#'   any custom serializer in `functions.jl`.
#' @param unserialize_function Character or named vector/list, defaults to NULL. Can be:
#'   - A single string for the Julia function to unserialize all upstream objects
#'   - A named vector/list where names are upstream dependency names and values
#'     are their specific unserialize functions
#'   If NULL, the default is `Serialization.deserialize`.
#' @param env_var Character vector, defaults to NULL. A named vector of
#'   environment variables to set before running the Julia script, e.g.,
#'   `c("JULIA_DEPOT_PATH" = "/path/to/depot")`. Each entry will be added as
#'   an `export` statement in the build phase.
#' @param noop_build Logical, defaults to FALSE. If TRUE, the derivation
#'   produces a no-op build (a stub output with no actual build steps). Any
#'   downstream derivations depending on a no-op build will themselves also
#'   become no-op builds.
#' @details At a basic level,
#'   `rxp_jl(filtered_data, "filter(df, :col .> 10)")` is equivalent to
#'   `filtered_data = filter(df, :col .> 10)` in Julia. `rxp_jl()` generates the
#'   required Nix boilerplate to output a so‐called "derivation" in Nix jargon.
#'   A Nix derivation is a recipe that defines how to create an output (in this
#'   case `filtered_data`) including its dependencies, build steps, and output
#'   paths.
#' @return An object of class derivation which inherits from lists.
#' @examples
#' \dontrun{
#' # Basic usage, no custom serializer
#' rxp_jl(
#'   name = filtered_df,
#'   jl_expr = "filter(df, :col .> 10)"
#' )
#'
#' # Skip building this derivation
#' rxp_jl(
#'   name = model_result,
#'   jl_expr = "train_model(data)",
#'   noop_build = TRUE
#' )
#'
#' # Custom serialization: assume `save_my_obj(obj, path)` is defined in functions.jl
#' rxp_jl(
#'   name = model_output,
#'   jl_expr = "train_model(data)",
#'   serialize_function = "save_my_obj",
#'   user_functions = "functions.jl"
#' )
#'
#' # Multiple unserialize functions for different dependencies
#' rxp_jl(
#'   name = combined_data,
#'   jl_expr = "merge(data1, data2)",
#'   unserialize_function = c(data1 = "JLD2.load", data2 = "CSV.read")
#' )
#' }
#' @family derivations
#' @export
rxp_jl <- function(
  name,
  jl_expr,
  additional_files = "",
  user_functions = "",
  nix_env = "default.nix",
  serialize_function = NULL,
  unserialize_function = NULL,
  env_var = NULL,
  noop_build = FALSE
) {
  out_name <- deparse1(substitute(name))
  # Escape double quotes for Julia one-liner
  jl_expr_escaped <- gsub("\"", "\\\\\"", jl_expr)

  # Parse serialize function
  serialize_str <- if (is.null(serialize_function)) {
    # Default: use built-in Serialization.serialize
    paste0(
      "using Serialization; ",
      "io = open(\\\"",
      out_name,
      "\\\", \\\"w\\\"); ",
      "serialize(io, ",
      out_name,
      "); ",
      "close(io)"
    )
  } else {
    if (!is.character(serialize_function) || length(serialize_function) != 1) {
      stop("serialize_function must be a single character string or NULL")
    }
    sprintf("%s(%s, \\\"%s\\\")", serialize_function, out_name, out_name)
  }

  # Parse unserialize function using helper
  unserialize_str <- parse_unserialize_function(
    substitute(unserialize_function),
    "Serialization.deserialize",
    parent.frame()
  )

  # Build components
  env_exports <- build_env_exports(env_var)

  # Build copy commands for additional files
  additional_files_clean <- additional_files[nzchar(additional_files)]
  copy_cmd <- build_copy_commands(additional_files_clean, recursive = TRUE)

  # Build copy commands for user functions (non-recursive)
  user_copy_cmd <- build_copy_commands(user_functions, recursive = FALSE)

  # Build include commands for user functions (Julia specific format)
  user_include_cmd <- build_source_commands(user_functions, "Jl", indent = 0)

  # Build phase
  unique_placeholder <- sprintf(
    "# RIXPRESS_JL_LOAD_DEPENDENCIES_HERE:%s",
    out_name
  )

  build_phase <- paste0(
    env_exports,
    copy_cmd,
    user_copy_cmd,
    "julia -e \"\n",
    "if isfile(\\\"libraries.jl\\\"); include(\\\"libraries.jl\\\"); end;\n",
    unique_placeholder,
    "\n",
    user_include_cmd,
    out_name,
    " = ",
    jl_expr_escaped,
    "; ",
    serialize_str,
    "\n\""
  )

  # Prepare fileset and src
  fileset_parts <- c(
    if (!is.null(additional_files) && any(nzchar(additional_files))) {
      additional_files[nzchar(additional_files)]
    },
    if (!is.null(user_functions) && any(nzchar(user_functions))) {
      user_functions[nzchar(user_functions)]
    }
  )
  src_snippet <- build_src_snippet(fileset_parts)

  # Generate snippet
  base <- sanitize_nix_base(nix_env)
  snippet <- make_derivation_snippet(
    out_name = out_name,
    src_snippet = src_snippet,
    base = base,
    build_phase = build_phase,
    derivation_type = "rxp_jl",
    noop_build = noop_build
  )

  # Create derivation object
  create_derivation(
    name = out_name,
    snippet = snippet,
    type = "rxp_jl",
    additional_files = additional_files,
    user_functions = user_functions,
    nix_env = nix_env,
    serialize_function = if (is.null(serialize_function)) {
      "Serialization.serialize"
    } else {
      serialize_function
    },
    unserialize_function = unserialize_str,
    env_var = env_var,
    noop_build = noop_build
  )
}

#' Helper function to extract rxp_read/rxp_load matches from content
#'
#' @param content_str String content of the document
#' @param func_name Function name to search for ("rxp_read" or "rxp_load")
#' @return Data frame with match information
#' @keywords internal
extract_rxp_matches <- function(content_str, func_name) {
  results <- list()

  for (quote_char in c('"', "'")) {
    if (quote_char == '"') {
      pattern <- sprintf('((?:rixpress::)?%s)\\("([^"]+)"\\)', func_name)
    } else {
      pattern <- sprintf("((?:rixpress::)?%s)\\('([^']+)'\\)", func_name)
    }

    matches <- gregexpr(pattern, content_str)
    full_matches <- regmatches(content_str, matches)[[1]]

    if (length(full_matches) > 0) {
      for (match in full_matches) {
        if (quote_char == '"') {
          parts <- regmatches(
            match,
            regexec(
              sprintf('((?:rixpress::)?%s)\\("([^"]+)"\\)', func_name),
              match
            )
          )[[1]]
        } else {
          parts <- regmatches(
            match,
            regexec(
              sprintf("((?:rixpress::)?%s)\\('([^']+)'\\)", func_name),
              match
            )
          )[[1]]
        }

        if (length(parts) == 3) {
          results[[length(results) + 1]] <- list(
            full_match = parts[1],
            func_call = parts[2],
            path = parts[3],
            quote_char = quote_char
          )
        }
      }
    }
  }

  if (length(results) > 0) {
    do.call(rbind, lapply(results, as.data.frame, stringsAsFactors = FALSE))
  } else {
    data.frame(
      full_match = character(0),
      func_call = character(0),
      path = character(0),
      quote_char = character(0),
      stringsAsFactors = FALSE
    )
  }
}

#' Generate substitution commands for document references
#'
#' @param all_matches Data frame with match information
#' @param doc_file Document file name
#' @return Character vector of substitution commands
#' @keywords internal
generate_substitution_commands <- function(all_matches, doc_file) {
  if (nrow(all_matches) == 0) {
    return(character(0))
  }

  sub_cmds <- character(nrow(all_matches))

  for (i in 1:nrow(all_matches)) {
    match <- all_matches[i, ]

    # Build the search pattern
    if (match$quote_char == '"') {
      search_pattern <- sprintf('%s("%s")', match$func_call, match$path)
    } else {
      search_pattern <- sprintf("%s('%s')", match$func_call, match$path)
    }

    # Build replacement based on function type
    is_load <- grepl("rxp_load", match$func_call)

    # Preserve namespace if present
    rxp_read_func <- if (grepl("rixpress::", match$func_call)) {
      "rixpress::rxp_read"
    } else {
      "rxp_read"
    }

    if (is_load) {
      replacement <- sprintf(
        '%s <- %s("${%s}")',
        match$path,
        rxp_read_func,
        match$path
      )
    } else {
      replacement <- sprintf('%s("${%s}")', rxp_read_func, match$path)
    }

    sub_cmds[i] <- sprintf(
      "substituteInPlace %s --replace-fail '%s' '%s'",
      doc_file,
      search_pattern,
      replacement
    )
  }

  sub_cmds
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
#' @param noop_build Logical, defaults to FALSE. If TRUE, the derivation
#'   produces a no-op build (a stub output with no actual build steps). Any
#'   downstream derivations depending on a no-op build will themselves also
#'   become no-op builds.
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
#'
#'   # Skip building this derivation
#'   rxp_qmd(
#'     name = draft_report,
#'     qmd_file = "draft.qmd",
#'     noop_build = TRUE
#'   )
#' }
#' @export
rxp_qmd <- function(
  name,
  qmd_file,
  additional_files = "",
  nix_env = "default.nix",
  args = "",
  env_var = NULL,
  noop_build = FALSE
) {
  out_name <- deparse1(substitute(name))

  # Read and analyze document content
  content <- readLines(qmd_file, warn = FALSE)
  content_str <- paste(content, collapse = "\n")

  # Extract all rxp_read and rxp_load matches
  read_matches <- extract_rxp_matches(content_str, "rxp_read")
  load_matches <- extract_rxp_matches(content_str, "rxp_load")
  all_matches <- rbind(read_matches, load_matches)

  # Generate substitution commands
  sub_cmds <- generate_substitution_commands(all_matches, qmd_file)

  # Build environment exports with specific indent for build phase
  env_exports <- if (!is.null(env_var)) {
    paste(
      vapply(
        names(env_var),
        function(var_name) {
          sprintf("      export %s=%s", var_name, env_var[[var_name]])
        },
        character(1)
      ),
      collapse = "\n"
    )
  } else {
    NULL
  }

  # Build phase
  build_phase_lines <- c(
    "      mkdir home",
    "      export HOME=$PWD/home",
    "      export RETICULATE_PYTHON=${defaultPkgs.python3}/bin/python",
    env_exports,
    if (length(sub_cmds) > 0) paste("      ", sub_cmds, sep = ""),
    sprintf("      quarto render %s %s --output-dir $out", qmd_file, args)
  )

  build_phase <- paste(
    Filter(Negate(is.null), build_phase_lines),
    collapse = "\n"
  )

  # Prepare fileset and src
  if (identical(additional_files, "")) {
    additional_files <- NULL
  }
  fileset_parts <- c(qmd_file, additional_files)
  src_snippet <- build_src_snippet(fileset_parts)

  # Generate snippet
  base <- sanitize_nix_base(nix_env)
  snippet <- make_derivation_snippet(
    out_name = out_name,
    src_snippet = src_snippet,
    base = base,
    build_phase = build_phase,
    derivation_type = "rxp_qmd",
    noop_build = noop_build
  )

  create_derivation(
    name = out_name,
    snippet = snippet,
    type = "rxp_qmd",
    additional_files = additional_files,
    nix_env = nix_env,
    env_var = env_var,
    noop_build = noop_build,
    qmd_file = qmd_file,
    args = args
  )
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
#' @param noop_build Logical, defaults to FALSE. If TRUE, the derivation
#'   produces a no-op build (a stub output with no actual build steps). Any
#'   downstream derivations depending on a no-op build will themselves also
#'   become no-op builds.
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
#'
#'   # Skip building this derivation
#'   rxp_rmd(
#'     name = draft_report,
#'     rmd_file = "draft.Rmd",
#'     noop_build = TRUE
#'   )
#' }
#' @export
rxp_rmd <- function(
  name,
  rmd_file,
  additional_files = "",
  nix_env = "default.nix",
  params = NULL,
  env_var = NULL,
  noop_build = FALSE
) {
  out_name <- deparse1(substitute(name))

  # Read and analyze document content
  content <- readLines(rmd_file, warn = FALSE)
  content_str <- paste(content, collapse = "\n")

  # For R Markdown, we can use simpler pattern matching since it's R-specific
  # Extract rxp_read references (both with and without quotes)
  matches <- gregexpr('rxp_read\\("([^"]+)"\\)', content_str)
  refs <- regmatches(content_str, matches)[[1]]
  refs <- sub('rxp_read\\("([^"]+)"\\)', '\\1', refs)
  refs <- unique(refs)

  # Generate substitution commands
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
          function(name, value) sprintf("%s = %s", name, deparse1(value)),
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

  # Build environment exports with specific indent for build phase
  env_exports <- if (!is.null(env_var)) {
    paste(
      vapply(
        names(env_var),
        function(var_name) {
          sprintf("      export %s=%s", var_name, env_var[[var_name]])
        },
        character(1)
      ),
      collapse = "\n"
    )
  } else {
    NULL
  }

  # Build phase
  build_phase_lines <- c(
    "      mkdir home",
    "      export HOME=$PWD/home",
    "      export RETICULATE_PYTHON=${defaultPkgs.python3}/bin/python",
    env_exports,
    if (length(sub_cmds) > 0) paste("      ", sub_cmds, sep = ""),
    sprintf("      Rscript -e \"rmd_file <- '%s'; %s\"", rmd_file, render_args)
  )

  build_phase <- paste(
    Filter(Negate(is.null), build_phase_lines),
    collapse = "\n"
  )

  # Prepare fileset and src
  fileset_parts <- c(rmd_file, additional_files)
  fileset_parts <- fileset_parts[nzchar(fileset_parts)]
  src_snippet <- build_src_snippet(fileset_parts)

  # Generate snippet
  base <- sanitize_nix_base(nix_env)
  snippet <- make_derivation_snippet(
    out_name = out_name,
    src_snippet = src_snippet,
    base = base,
    build_phase = build_phase,
    derivation_type = "rxp_rmd",
    noop_build = noop_build
  )

  create_derivation(
    name = out_name,
    snippet = snippet,
    type = "rxp_rmd",
    additional_files = additional_files,
    nix_env = nix_env,
    env_var = env_var,
    noop_build = noop_build,
    rmd_file = rmd_file,
    params = params
  )
}


#' Print method for derivation objects
#' @param x An object of class "rxp_derivation"
#' @param ... Additional arguments passed to print methods
#' @return Nothing, prints a summary of the derivation object to the console.
#' @examples
#' \dontrun{
#' # d0 is a previously defined derivation
#'   print(d0)
#' }
#' @family utilities
#' @export
print.rxp_derivation <- function(x, ...) {
  cat("Name:", x$name, "\n")
  cat("Type:", x$type, "\n")
  cat("No-op Build:", x$noop_build, "\n")
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
    if (
      is.null(x$additional_files) ||
        length(x$additional_files) == 0 ||
        all(x$additional_files == "")
    ) {
      "None"
    } else {
      paste(x$additional_files, collapse = ", ")
    },
    "\n"
  )
  if ("user_functions" %in% names(x)) {
    cat(
      "User functions:",
      if (
        is.null(x$user_functions) ||
          length(x$user_functions) == 0 ||
          all(x$user_functions == "")
      ) {
        "None"
      } else {
        paste(x$user_functions, collapse = ", ")
      },
      "\n"
    )
  }
  cat("Nix env:", x$nix_env, "\n")
  if ("env_var" %in% names(x)) {
    cat(
      "Env variables:",
      if (is.null(x$env_var)) {
        "None"
      } else {
        paste(names(x$env_var), x$env_var, sep = "=", collapse = ", ")
      },
      "\n"
    )
  }
}
