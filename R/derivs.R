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

#' Create a Nix expression running an R function
#' @family derivations
#' @param name Symbol, name of the derivation.
#' @param expr R code to generate the expression. Ideally it should be a call
#'   to a pure function, or a piped expression. Multi-line expressions are not
#'   supported.
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
#' @param encoder Function or character defaults to NULL. A function used to
#'   encode (serialize) objects for transfer between derivations. It must accept two
#'   arguments: the object to encode (first), and the target file path
#'   (second). If your function has a different signature, wrap it to match this
#'   interface. By default, `saveRDS()` is used, but this may yield unexpected
#'   results, especially for complex objects like machine learning models. For
#'   instance, for `{keras}` models, use `keras::save_model_hdf5()` to capture
#'   the full model (architecture, weights, training config, optimiser state,
#'   etc.). See `vignette("encoding-decoding")` for more details.
#' @param decoder Function, character, or named vector/list,
#'   defaults to NULL. Can be:
#'   - A single function/string to decode (unserialize) all upstream objects (e.g., `readRDS`)
#'   - A named vector/list where names are upstream dependency names and values
#'     are their specific decoding functions (e.g.,
#'     `c(mtcars_tail = "qs::qread", mtcars_head = "read.csv")`)
#'   By default, `readRDS()` is used. See `vignette("encoding-decoding")` for more details.
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
#'    encoder = qs::qsave
#'   )
#'   # Unerialize using qs::qread in the next derivation
#'   rxp_r(
#'    name = mtcars_mpg,
#'    expr = select(filtered_mtcars, mpg),
#'    decoder = qs::qread
#'   )
#' }
#' @export
rxp_r <- function(
  name,
  expr,
  additional_files = "",
  user_functions = "",
  nix_env = "default.nix",
  encoder = NULL,
  decoder = NULL,
  env_var = NULL,
  noop_build = FALSE
) {
  out_name <- deparse1(substitute(name))
  expr_str <- deparse1(substitute(expr))
  expr_str <- gsub("\"", "'", expr_str) # Replace " with ' for Nix
  expr_str <- gsub("$", "\\$", expr_str, fixed = TRUE) # Replace $ with \$ for Nix

  # Capture without evaluating promises; supports bare symbols (qs::qsave)
  # and character literals ("qs::qsave") without loading packages now.
  serialize_expr <- substitute(encoder)
  if (identical(serialize_expr, quote(NULL))) {
    serialize_str <- "saveRDS"
  } else if (is.character(serialize_expr)) {
    # User passed a character literal; use it as-is (no quotes in final code)
    serialize_str <- serialize_expr
  } else {
    serialize_str <- deparse1(serialize_expr)
  }

  # Handle decoder - can be single value or named vector/list
  unserialize_expr <- substitute(decoder)
  if (identical(unserialize_expr, quote(NULL))) {
    unserialize_str <- "readRDS"
  } else {
    # Check if it's a call to c() with named arguments
    if (
      is.call(unserialize_expr) && identical(unserialize_expr[[1]], quote(c))
    ) {
      # It's a c() call - check if it has names
      call_names <- names(unserialize_expr)
      if (!is.null(call_names) && any(nzchar(call_names[-1]))) {
        # Has named arguments (skip first element which is the function name 'c')
        # Extract as a named list to preserve names in JSON
        unserialize_str <- list()
        for (i in 2:length(unserialize_expr)) {
          if (nzchar(call_names[i])) {
            # Get the value as a string
            val <- unserialize_expr[[i]]
            if (is.character(val)) {
              unserialize_str[[call_names[i]]] <- val
            } else {
              unserialize_str[[call_names[i]]] <- as.character(val)
            }
          }
        }
        # Convert to a named list explicitly
        unserialize_str <- as.list(unserialize_str)
      } else {
        # No names, treat as single value
        unserialize_str <- deparse1(unserialize_expr)
      }
    } else if (is.character(unserialize_expr)) {
      # Direct character value
      unserialize_str <- unserialize_expr
    } else {
      # Try to evaluate it to see if it's a pre-existing named vector
      tryCatch(
        {
          unserialize_val <- eval(unserialize_expr, envir = parent.frame())
          if (
            !is.null(names(unserialize_val)) &&
              length(names(unserialize_val)) > 0
          ) {
            # Convert named vector to named list for JSON preservation
            unserialize_str <- as.list(setNames(
              unserialize_val,
              names(unserialize_val)
            ))
          } else {
            # Single value
            if (is.character(unserialize_val)) {
              unserialize_str <- unserialize_val
            } else {
              unserialize_str <- deparse1(unserialize_expr)
            }
          }
        },
        error = function(e) {
          # If evaluation fails, treat as a symbol
          unserialize_str <- deparse1(unserialize_expr)
        }
      )
    }
  }

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

  # Prepare the fileset for src
  all_files <- c(additional_files, user_functions)
  fileset_parts <- all_files[nzchar(all_files)]

  # build copy command for additional files only (not user_functions)
  copy_cmd <- ""
  if (length(additional_files) > 0) {
    additional_files_clean <- additional_files[nzchar(additional_files)]
    if (length(additional_files_clean) > 0) {
      copy_lines <- vapply(
        additional_files_clean,
        function(f) sprintf("cp -r ${./%s} %s", f, f),
        character(1)
      )
      copy_cmd <- paste0(paste(copy_lines, collapse = "\n      "), "\n      ")
    }
  }

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

  # If you adopted name-scoped placeholders, keep using it here
  unique_placeholder <- sprintf(
    "# RIXPRESS_LOAD_DEPENDENCIES_HERE:%s",
    out_name
  )

  build_phase <- sprintf(
    "%s%s%sRscript -e \"\n        source('libraries.R')\n        %s\n        %s%s <- %s\n        %s(%s, '%s')\"",
    env_exports,
    copy_cmd,
    user_functions_copy_cmd,
    unique_placeholder,
    source_cmd,
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
      "     src = defaultPkgs.lib.fileset.toSource {\n      root = ./.;\n      fileset = defaultPkgs.lib.fileset.unions [ %s ];\n    };\n",
      fileset_nix
    )
  } else {
    src_snippet <- ""
  }

  snippet <- make_derivation_snippet(
    out_name = out_name,
    src_snippet = src_snippet,
    base = base,
    build_phase = build_phase,
    derivation_type = "rxp_r",
    noop_build = noop_build
  )

  list(
    name = out_name,
    snippet = snippet,
    type = "rxp_r",
    additional_files = additional_files,
    user_functions = user_functions,
    nix_env = nix_env,
    encoder = serialize_str,
    decoder = unserialize_str,
    env_var = env_var,
    noop_build = noop_build
  ) |>
    structure(class = "rxp_derivation")
}

#' Create a Nix expression running a Python function
#'
#' @family derivations
#' @param name Symbol, name of the derivation.
#' @param expr Character, Python code to generate the expression. Ideally it
#'   should be a call to a pure function. Multi-line expressions are not
#'   supported.
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
#' @param encoder Character, defaults to NULL. The name of the Python
#'   function used to serialize the object. It must accept two arguments: the
#'   object to serialize (first), and the target file path (second). If NULL,
#'   the default behaviour uses `pickle.dump`. Define this function in
#'   `functions.py`. See `vignette("encoding-decoding")` for more details.
#' @param decoder Character or named vector/list, defaults to NULL. Can be:
#'   - A single string for the Python function to unserialize all upstream objects
#'   - A named vector/list where names are upstream dependency names and values
#'     are their specific unserialize functions
#'   If NULL, the default uses `pickle.load`. See `vignette("encoding-decoding")` for more details.
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
#'     expr = "mtcars_pl.filter(polars.col('am') == 1).to_pandas()"
#'   )
#'
#'   # Skip building this derivation
#'   rxp_py(
#'     data_prep,
#'     expr = "preprocess_data(raw_data)",
#'     noop_build = TRUE
#'   )
#'
#'   # Custom serialization
#'   rxp_py(
#'     mtcars_pl_am,
#'     expr = "mtcars_pl.filter(polars.col('am') == 1).to_pandas()",
#'     user_functions = "functions.py",
#'     encoder = "serialize_model",
#'     additional_files = "some_required_file.bin")
#' }
#' @export
rxp_py <- function(
  name,
  expr,
  additional_files = "",
  user_functions = "",
  nix_env = "default.nix",
  encoder = NULL,
  decoder = NULL,
  env_var = NULL,
  noop_build = FALSE
) {
  out_name <- deparse1(substitute(name))
  expr <- gsub("'", "\\'", expr, fixed = TRUE)

  # Handle encoder for the build_phase
  if (is.null(encoder)) {
    serialize_str <- sprintf(
      "with open('%s', 'wb') as f: pickle.dump(globals()['%s'], f)",
      out_name,
      out_name
    )
  } else {
    if (!is.character(encoder)) {
      stop("encoder must be a character string or NULL")
    }
    serialize_str <- sprintf(
      "%s(globals()['%s'], '%s')",
      encoder,
      out_name,
      out_name
    )
  }

  # Handle decoder - can be single value or named vector/list
  unserialize_expr <- substitute(decoder)
  if (identical(unserialize_expr, quote(NULL))) {
    unserialize_str <- "pickle.load"
  } else {
    # Check if it's a call to c() with named arguments
    if (
      is.call(unserialize_expr) && identical(unserialize_expr[[1]], quote(c))
    ) {
      # It's a c() call - check if it has names
      call_names <- names(unserialize_expr)
      if (!is.null(call_names) && any(nzchar(call_names[-1]))) {
        # Has named arguments (skip first element which is the function name 'c')
        # Extract as a named list to preserve names in JSON
        unserialize_str <- list()
        for (i in 2:length(unserialize_expr)) {
          if (nzchar(call_names[i])) {
            # Get the value as a string
            val <- unserialize_expr[[i]]
            if (is.character(val)) {
              unserialize_str[[call_names[i]]] <- val
            } else {
              unserialize_str[[call_names[i]]] <- as.character(val)
            }
          }
        }
        # Convert to a named list explicitly
        unserialize_str <- as.list(unserialize_str)
      } else {
        # No names, treat as single value
        if (length(unserialize_expr) > 1) {
          # It's c() with multiple unnamed values - take the first
          val <- unserialize_expr[[2]]
          unserialize_str <- if (is.character(val)) val else as.character(val)
        } else {
          unserialize_str <- deparse1(unserialize_expr)
        }
      }
    } else if (is.character(unserialize_expr)) {
      # Direct character value
      unserialize_str <- unserialize_expr
    } else {
      # Try to evaluate it to see if it's a pre-existing named vector
      tryCatch(
        {
          unserialize_val <- eval(unserialize_expr, envir = parent.frame())
          if (
            !is.null(names(unserialize_val)) &&
              length(names(unserialize_val)) > 0
          ) {
            # Convert named vector to named list for JSON preservation
            unserialize_str <- as.list(setNames(
              unserialize_val,
              names(unserialize_val)
            ))
          } else {
            # Single value
            if (is.character(unserialize_val)) {
              unserialize_str <- unserialize_val
            } else {
              unserialize_str <- as.character(unserialize_val)
            }
          }
        },
        error = function(e) {
          # If evaluation fails, treat as a symbol and convert to string
          unserialize_str <- deparse1(unserialize_expr)
        }
      )
    }
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

  # Prepare the fileset for src
  # Combine additional_files and user_functions for the fileset
  fileset_parts <- c()
  if (!is.null(additional_files) && any(nzchar(additional_files))) {
    fileset_parts <- c(
      fileset_parts,
      additional_files[nzchar(additional_files)]
    )
  }
  if (!is.null(user_functions) && any(nzchar(user_functions))) {
    fileset_parts <- c(fileset_parts, user_functions[nzchar(user_functions)])
  }

  # build copy command for additional files (excluding user_functions)
  copy_cmd <- ""
  if (!is.null(additional_files) && any(nzchar(additional_files))) {
    additional_files_clean <- additional_files[nzchar(additional_files)]
    copy_lines <- vapply(
      additional_files_clean,
      function(f) sprintf("cp -r ${./%s} %s", f, f),
      character(1)
    )
    copy_cmd <- paste0(paste(copy_lines, collapse = "\n      "), "\n      ")
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
      paste(user_copy_lines, collapse = "\n      "),
      "\n      "
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

  # Unique placeholder per derivation
  unique_placeholder <- sprintf(
    "# RIXPRESS_PY_LOAD_DEPENDENCIES_HERE:%s",
    out_name
  )

  # Construct build_phase including cp commands then python execution
  build_phase <- paste0(
    env_exports,
    copy_cmd,
    user_functions_copy_cmd,
    "python -c \"\n",
    "exec(open('libraries.py').read())\n",
    unique_placeholder,
    "\n",
    user_import_cmd,
    "exec('",
    out_name,
    " = ",
    expr,
    "')\n",
    serialize_str,
    "\n",
    "\""
  )

  # Derive base from nix_env
  base <- gsub("[^a-zA-Z0-9]", "_", nix_env)
  base <- sub("_nix$", "", base)

  # Prepare the src snippet with all files
  if (length(fileset_parts) > 0) {
    fileset_nix <- paste0("./", fileset_parts, collapse = " ")
    src_snippet <- sprintf(
      "     src = defaultPkgs.lib.fileset.toSource {\n      root = ./.;\n      fileset = defaultPkgs.lib.fileset.unions [ %s ];\n    };\n",
      fileset_nix
    )
  } else {
    src_snippet <- ""
  }

  # Generate the Nix snippet
  snippet <- make_derivation_snippet(
    out_name = out_name,
    src_snippet = src_snippet,
    base = base,
    build_phase = build_phase,
    derivation_type = "rxp_py",
    noop_build = noop_build
  )

  list(
    name = out_name,
    snippet = snippet,
    type = "rxp_py",
    additional_files = additional_files,
    user_functions = user_functions,
    nix_env = nix_env,
    encoder = serialize_str,
    decoder = unserialize_str,
    env_var = env_var,
    noop_build = noop_build
  ) |>
    structure(class = "rxp_derivation")
}

#' Create a Nix expression running a Julia function
#'
#' @param name Symbol, name of the derivation.
#' @param expr Character, Julia code to generate the expression. Ideally it
#'   should be a call to a pure function. Multi-line expressions are not
#'   supported.
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
#' @param encoder Character, defaults to NULL. The name of the Julia
#'   function used to serialize the object. It must accept two arguments: the
#'   object to serialize (first), and the target file path (second). If NULL,
#'   the default behaviour uses the built‐in `Serialization.serialize` API. Define
#'   any custom serializer in `functions.jl`. See `vignette("encoding-decoding")` for more details.
#' @param decoder Character or named vector/list, defaults to NULL. Can be:
#'   - A single string for the Julia function to unserialize all upstream objects
#'   - A named vector/list where names are upstream dependency names and values
#'     are their specific unserialize functions
#'   If NULL, the default is `Serialization.deserialize`. See `vignette("encoding-decoding")` for more details.
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
#'   expr = "filter(df, :col .> 10)"
#' )
#'
#' # Skip building this derivation
#' rxp_jl(
#'   name = model_result,
#'   expr = "train_model(data)",
#'   noop_build = TRUE
#' )
#'
#' # Custom serialization: assume `save_my_obj(obj, path)` is defined in functions.jl
#' rxp_jl(
#'   name = model_output,
#'   expr = "train_model(data)",
#'   encoder = "save_my_obj",
#'   user_functions = "functions.jl"
#' )
#' }
#' @family derivations
#' @export
rxp_jl <- function(
  name,
  expr,
  additional_files = "",
  user_functions = "",
  nix_env = "default.nix",
  encoder = NULL,
  decoder = NULL,
  env_var = NULL,
  noop_build = FALSE
) {
  out_name <- deparse1(substitute(name))
  # Escape double quotes for Julia one-liner
  expr_escaped <- gsub("\"", "\\\\\"", expr)

  # Determine which serialize function to call
  if (is.null(encoder)) {
    # Default: use built-in Serialization.serialize
    serialize_str <- paste0(
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
    if (!is.character(encoder) || length(encoder) != 1) {
      stop("encoder must be a single character string or NULL")
    }
    serialize_str <- sprintf(
      "%s(%s, \\\"%s\\\")",
      encoder,
      out_name,
      out_name
    )
  }

  # Handle decoder - can be single value or named vector/list
  unserialize_expr <- substitute(decoder)
  if (identical(unserialize_expr, quote(NULL))) {
    unserialize_str <- "Serialization.deserialize"
  } else {
    # Check if it's a call to c() with named arguments
    if (
      is.call(unserialize_expr) && identical(unserialize_expr[[1]], quote(c))
    ) {
      # It's a c() call - check if it has names
      call_names <- names(unserialize_expr)
      if (!is.null(call_names) && any(nzchar(call_names[-1]))) {
        # Has named arguments (skip first element which is the function name 'c')
        # Extract as a named list to preserve names in JSON
        unserialize_str <- list()
        for (i in 2:length(unserialize_expr)) {
          if (nzchar(call_names[i])) {
            # Get the value as a string
            val <- unserialize_expr[[i]]
            if (is.character(val)) {
              unserialize_str[[call_names[i]]] <- val
            } else {
              unserialize_str[[call_names[i]]] <- as.character(val)
            }
          }
        }
        # Convert to a named list explicitly
        unserialize_str <- as.list(unserialize_str)
      } else {
        # No names, treat as single value
        if (length(unserialize_expr) > 1) {
          # It's c() with multiple unnamed values - take the first
          val <- unserialize_expr[[2]]
          unserialize_str <- if (is.character(val)) val else as.character(val)
        } else {
          unserialize_str <- deparse1(unserialize_expr)
        }
      }
    } else if (is.character(unserialize_expr)) {
      # Direct character value
      unserialize_str <- unserialize_expr
    } else {
      # Try to evaluate it to see if it's a pre-existing named vector
      tryCatch(
        {
          unserialize_val <- eval(unserialize_expr, envir = parent.frame())
          if (
            !is.null(names(unserialize_val)) &&
              length(names(unserialize_val)) > 0
          ) {
            # Convert named vector to named list for JSON preservation
            unserialize_str <- as.list(setNames(
              unserialize_val,
              names(unserialize_val)
            ))
          } else {
            # Single value
            if (is.character(unserialize_val)) {
              unserialize_str <- unserialize_val
            } else {
              unserialize_str <- as.character(unserialize_val)
            }
          }
        },
        error = function(e) {
          # If evaluation fails, treat as a symbol and convert to string
          unserialize_str <- deparse1(unserialize_expr)
        }
      )
    }
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
    if (nzchar(env_exports)) {
      env_exports <- paste0(env_exports, "\n      ")
    }
  }

  # Prepare the fileset for src, INCLUDE user_functions as well
  fileset_parts <- c()
  if (!is.null(additional_files) && any(nzchar(additional_files))) {
    fileset_parts <- c(
      fileset_parts,
      additional_files[nzchar(additional_files)]
    )
  }
  if (!is.null(user_functions) && any(nzchar(user_functions))) {
    fileset_parts <- c(fileset_parts, user_functions[nzchar(user_functions)])
  }

  # Build copy command for additional files (not user_functions)
  additional_files_clean <- additional_files[nzchar(additional_files)]
  copy_cmd <- ""
  if (length(additional_files_clean) > 0) {
    copy_lines <- vapply(
      additional_files_clean,
      function(f) sprintf("cp -r ${./%s} %s", f, f),
      character(1)
    )
    copy_cmd <- paste0(paste(copy_lines, collapse = "\n      "), "\n      ")
  }

  # Build copy command for user_functions (explicit copy, not -r)
  user_functions_copy_cmd <- ""
  if (!is.null(user_functions) && any(nzchar(user_functions))) {
    user_functions_clean <- user_functions[nzchar(user_functions)]
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

  # Generate include commands for user_functions
  user_include_cmd <- ""
  if (!is.null(user_functions) && any(nzchar(user_functions))) {
    user_functions_clean <- user_functions[nzchar(user_functions)]
    include_lines <- vapply(
      user_functions_clean,
      function(f) sprintf("include(\\\"%s\\\")", f),
      character(1)
    )
    user_include_cmd <- paste0(paste(include_lines, collapse = "; "), "; ")
  }

  # Unique placeholder per derivation (line-only, no trailing semicolon)
  unique_placeholder <- sprintf(
    "# RIXPRESS_JL_LOAD_DEPENDENCIES_HERE:%s",
    out_name
  )

  # Construct the Julia build phase: include libraries.jl if present,
  # include user_functions, run expression, then serialize
  build_phase <- paste0(
    env_exports,
    copy_cmd,
    user_functions_copy_cmd,
    "julia -e \"\n",
    "if isfile(\\\"libraries.jl\\\"); include(\\\"libraries.jl\\\"); end;\n",
    unique_placeholder,
    "\n",
    user_include_cmd,
    out_name,
    " = ",
    expr_escaped,
    "; ",
    serialize_str,
    "\n",
    "\""
  )

  # Derive base variable from nix_env
  base <- gsub("[^a-zA-Z0-9]", "_", nix_env)
  base <- sub("_nix$", "", base)

  # Prepare src snippet with all relevant files
  if (length(fileset_parts) > 0) {
    fileset_nix <- paste0("./", fileset_parts, collapse = " ")
    src_snippet <- sprintf(
      "    src = defaultPkgs.lib.fileset.toSource {\n      root = ./.;\n      fileset = defaultPkgs.lib.fileset.unions [ %s ];\n    };\n",
      fileset_nix
    )
  } else {
    src_snippet <- ""
  }

  # Assemble the Nix-derivation snippet
  snippet <- make_derivation_snippet(
    out_name = out_name,
    src_snippet = src_snippet,
    base = base,
    build_phase = build_phase,
    derivation_type = "rxp_jl",
    noop_build = noop_build
  )

  list(
    name = out_name,
    snippet = snippet,
    type = "rxp_jl",
    additional_files = additional_files,
    user_functions = user_functions,
    nix_env = nix_env,
    encoder = if (is.null(encoder)) {
      "Serialization.serialize"
    } else {
      encoder
    },
    decoder = unserialize_str,
    env_var = env_var,
    noop_build = noop_build
  ) |>
    structure(class = "rxp_derivation")
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

  content <- readLines(qmd_file, warn = FALSE)
  content_str <- paste(content, collapse = "\n")

  # Helper function to extract actual matches (including namespace info)
  extract_actual_matches <- function(func_name, quote_char) {
    # Pattern that matches both bare and namespaced versions
    if (quote_char == '"') {
      pattern <- sprintf('((?:rixpress::)?%s)\\("([^"]+)"\\)', func_name)
    } else {
      pattern <- sprintf("((?:rixpress::)?%s)\\('([^']+)'\\)", func_name)
    }

    matches <- gregexpr(pattern, content_str)
    full_matches <- regmatches(content_str, matches)[[1]]

    if (length(full_matches) == 0) {
      return(data.frame())
    }

    results <- data.frame(
      full_match = character(0),
      func_call = character(0),
      path = character(0),
      quote_char = character(0),
      stringsAsFactors = FALSE
    )

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
        results <- rbind(
          results,
          data.frame(
            full_match = parts[1],
            func_call = parts[2],
            path = parts[3],
            quote_char = quote_char,
            stringsAsFactors = FALSE
          )
        )
      }
    }

    results
  }

  # Extract all actual matches
  read_matches_double <- extract_actual_matches('rxp_read', '"')
  read_matches_single <- extract_actual_matches('rxp_read', "'")
  load_matches_double <- extract_actual_matches('rxp_load', '"')
  load_matches_single <- extract_actual_matches('rxp_load', "'")

  # Combine all matches
  all_matches <- rbind(
    read_matches_double,
    read_matches_single,
    load_matches_double,
    load_matches_single
  )

  # Get unique paths for environment variables
  all_refs <- unique(all_matches$path)

  # Generate substitution commands based on actual matches
  sub_cmds <- character(0)

  if (nrow(all_matches) > 0) {
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

      # Determine the correct function name to use in replacement
      # If original had namespace, preserve it for rxp_read
      if (grepl("rixpress::", match$func_call)) {
        rxp_read_func <- "rixpress::rxp_read"
      } else {
        rxp_read_func <- "rxp_read"
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

      cmd <- sprintf(
        "substituteInPlace %s --replace-fail '%s' '%s'",
        qmd_file,
        search_pattern,
        replacement
      )

      sub_cmds <- c(sub_cmds, cmd)
    }
  }

  # Generate environment variable export statements if env_var is provided
  env_exports <- ""
  if (!is.null(env_var)) {
    env_exports <- paste(
      vapply(
        names(env_var),
        function(var_name) {
          sprintf("      export %s=%s", var_name, env_var[[var_name]])
        },
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
    if (length(sub_cmds) > 0) {
      paste("      ", sub_cmds, sep = "", collapse = "\n")
    } else {
      ""
    },
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
  snippet <- make_derivation_snippet(
    out_name = out_name,
    src_snippet = sprintf(
      "    src = defaultPkgs.lib.fileset.toSource {\n      root = ./.;\n      fileset = defaultPkgs.lib.fileset.unions [ %s ];\n    };\n",
      fileset_nix
    ),
    base = base,
    build_phase = build_phase,
    derivation_type = "rxp_qmd",
    noop_build = noop_build
  )

  list(
    name = out_name,
    snippet = snippet,
    type = "rxp_qmd",
    qmd_file = qmd_file,
    additional_files = additional_files,
    nix_env = nix_env,
    args = args,
    env_var = env_var,
    noop_build = noop_build
  ) |>
    structure(class = "rxp_derivation")
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

  # Generate environment variable export statements if env_var is provided
  env_exports <- ""
  if (!is.null(env_var)) {
    env_exports <- paste(
      vapply(
        names(env_var),
        function(var_name) {
          sprintf("      export %s=%s", var_name, env_var[[var_name]])
        },
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
    if (length(sub_cmds) > 0) {
      paste("      ", sub_cmds, sep = "", collapse = "\n")
    } else {
      ""
    },
    sprintf("      Rscript -e \"rmd_file <- '%s'; %s\"", rmd_file, render_args),
    sep = "\n"
  )

  # Prepare the fileset for src
  fileset_parts <- c(rmd_file, additional_files)
  fileset_nix <- paste0("./", fileset_parts, collapse = " ")

  # Derive base from nix_env
  base <- gsub("[^a-zA-Z0-9]", "_", nix_env)
  base <- sub("_nix$", "", base)

  snippet <- make_derivation_snippet(
    out_name = out_name,
    src_snippet = sprintf(
      "    src = defaultPkgs.lib.fileset.toSource {\n      root = ./.;\n      fileset = defaultPkgs.lib.fileset.unions [ %s ];\n    };\n",
      fileset_nix
    ),
    base = base,
    build_phase = build_phase,
    derivation_type = "rxp_rmd",
    noop_build = noop_build
  )

  list(
    name = out_name,
    snippet = snippet,
    type = "rxp_rmd",
    rmd_file = rmd_file,
    additional_files = additional_files,
    nix_env = nix_env,
    params = params,
    env_var = env_var,
    noop_build = noop_build
  ) |>
    structure(class = "rxp_derivation")
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
  if ("encoder" %in% names(x)) {
    cat("Serialize function:", x$encoder, "\n")
  }
  if ("decoder" %in% names(x)) {
    cat("Unserialize function:", x$decoder, "\n")
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
