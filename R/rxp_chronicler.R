#' Check If chronicler Package Is Available
#'
#' @return Logical, TRUE if package is available
#' @noRd
.rxp_has_chronicler <- function() {
  requireNamespace("chronicler", quietly = TRUE) &&
    requireNamespace("maybe", quietly = TRUE)
}


#' Determine Chronicle Status: "success", "warning", or "nothing"
#'
#' @param x Object to check
#' @return Character: "success", "warning", "nothing", or NULL if not a chronicle
#' @noRd
.rxp_chronicle_state <- function(x) {

  if (!.rxp_has_chronicler()) {
    return(NULL)
  }

  if (!inherits(x, "chronicle")) {
    return(NULL)
  }

  if (maybe::is_nothing(x$value)) {
    return("nothing")
  }

  log_df <- x$log_df
  has_nok <- any(grepl("NOK", log_df$outcome))

  if (has_nok) {
    return("warning")
  }

  "success"
}


#' Get Detailed Chronicle Status Information
#'
#' @param x A chronicle object
#' @return List with status information, or NULL if not a chronicle
#' @noRd
.rxp_chronicle_status <- function(x) {
  if (!.rxp_has_chronicler()) {
    return(NULL)
  }

  if (!inherits(x, "chronicle")) {
    return(NULL)
  }

  state <- .rxp_chronicle_state(x)
  is_nothing <- (state == "nothing")

  # Extract log summary
  log_df <- x$log_df
  nok_ops <- log_df[grepl("NOK", log_df$outcome), , drop = FALSE]

  list(
    is_chronicle = TRUE,
    state = state,
    is_nothing = is_nothing,
    has_warnings = (state == "warning"),
    num_operations = nrow(log_df),
    num_failed = nrow(nok_ops),
    failed_functions = if (nrow(nok_ops) > 0) {
      nok_ops[["function"]]
    } else {
      character(0)
    },
    messages = if (nrow(nok_ops) > 0) nok_ops$message else character(0)
  )
}

#' Get the Display Symbol for a Chronicle State
#'
#' @param state Character: "success", "warning", or "nothing"
#' @return Character: the display symbol
#' @noRd
.rxp_chronicle_symbol <- function(state) {
  switch(
    state,
    "success" = "\u2713", # checkmark
    "warning" = "\u26A0", # warning sign
    "nothing" = "\u2716", # X mark
    "?" # fallback
  )
}

#' Format Chronicle Status Message for Display
#'
#' @param derivation_name Name of the derivation
#' @param status Output from .rxp_chronicle_status()
#' @return Formatted message string
#' @noRd
.rxp_format_chronicle_message <- function(derivation_name, status) {
  if (is.null(status)) {
    return(NULL)
  }

  symbol <- .rxp_chronicle_symbol(status$state)

  if (status$state == "success") {
    return(sprintf("%s %s (chronicle: OK)", symbol, derivation_name))
  }

  msg <- sprintf(
    "%s %s (chronicle: %s)",
    symbol,
    derivation_name,
    toupper(status$state)
  )

  if (length(status$failed_functions) > 0) {
    msg <- paste0(
      msg,
      "\n    Failed: ",
      paste(status$failed_functions, collapse = ", ")
    )
  }

  if (length(status$messages) > 0) {
    real_msgs <- status$messages[!is.na(status$messages)]
    real_msgs <- real_msgs[real_msgs != "Pipeline failed upstream"]
    if (length(real_msgs) > 0) {
      msg <- paste0(
        msg,
        "\n    Message: ",
        paste(real_msgs, collapse = "; ")
      )
    }
  }

  msg
}

#' Check Pipeline Outputs for Chronicle Status
#'
#' @family utilities
#' @description Scans all derivation outputs for chronicle objects and
#'   reports their status: success (Just, no warnings), warning (Just with
#'   warnings), or nothing (failed computation). Only active when chronicler
#'   is installed.
#' @param project_path Character, defaults to ".".
#'   Path to the root directory of the project.
#' @param which_log Character, defaults to NULL. If NULL the most recent
#'   build log is used. If a string is provided, it's used as a
#'   regular expression to match against available log files.
#' @return A data frame with columns: derivation, chronicle_state,
#'   num_operations, num_failed, failed_functions, messages.
#'   Returns NULL invisibly if chronicler is not installed or no
#'   chronicle objects are found.
#' @details
#' This function is useful when using the `{chronicler}` package in your
#' rixpress pipeline. Because chronicler catches errors and warnings,
#' returning `Nothing` values instead of failing, Nix builds will always
#' succeed. This function helps identify derivations that contain failed
#' computations.
#'
#' The function displays one of three symbols for each chronicle:
#' \itemize{
#'   \item \code{checkmark} Success: Just value, no warnings or errors
#'   \item \code{warning sign} Warning: Just value, but warnings were captured
#'   \item \code{X mark} Nothing: Failed computation, errors captured
#' }
#' @examples
#' \dontrun{
#'   # After building a pipeline with chronicler functions
#'   rxp_check_chronicles()
#'
#'   # Check a specific build log

#'   rxp_check_chronicles(which_log = "20250131")
#' }
#' @export
rxp_check_chronicles <- function(project_path = ".", which_log = NULL) {
  if (!.rxp_has_chronicler()) {
    message(
      "chronicler package not installed. Chronicle checking not available."
    )
    return(invisible(NULL))
  }

  build_log <- rxp_inspect(project_path = project_path, which_log = which_log)

  # Filter to successful builds only
  successful <- build_log[build_log$build_success, ]

  results <- lapply(seq_len(nrow(successful)), function(i) {
    deriv_name <- successful$derivation[i]

    # Skip internal derivations
    if (deriv_name == "all-derivations") {
      return(NULL)
    }

    # Try to read the derivation, suppressing messages/warnings
    # since we'll report them ourselves
    obj <- tryCatch(
      suppressMessages(suppressWarnings(
        rxp_read(deriv_name, which_log = which_log, project_path = project_path)
      )),
      error = function(e) NULL
    )

    if (is.null(obj)) {
      return(NULL)
    }

    status <- .rxp_chronicle_status(obj)

    if (is.null(status)) {
      return(NULL) # Not a chronicle object
    }

    data.frame(
      derivation = deriv_name,
      chronicle_state = status$state,
      num_operations = status$num_operations,
      num_failed = status$num_failed,
      failed_functions = I(list(status$failed_functions)),
      messages = I(list(status$messages)),
      stringsAsFactors = FALSE
    )
  })

  results <- results[!vapply(results, is.null, logical(1))]

  if (length(results) == 0) {
    message("No chronicle objects found in pipeline outputs.")
    return(invisible(NULL))
  }

  result_df <- do.call(rbind, results)

  # Display summary with symbols
  cat("Chronicle status:\n")
  for (i in seq_len(nrow(result_df))) {
    row <- result_df[i, ]
    status <- list(
      state = row$chronicle_state,
      failed_functions = row$failed_functions[[1]],
      messages = row$messages[[1]]
    )
    cat(.rxp_format_chronicle_message(row$derivation, status), "\n")
  }

  # Summary counts
  n_success <- sum(result_df$chronicle_state == "success")
  n_warning <- sum(result_df$chronicle_state == "warning")
  n_nothing <- sum(result_df$chronicle_state == "nothing")

  cat(sprintf(
    "\nSummary: %d success, %d with warnings, %d nothing\n",
    n_success,
    n_warning,
    n_nothing
  ))

  if (n_nothing > 0) {
    warning(
      sprintf("%d derivation(s) contain Nothing values!", n_nothing),
      call. = FALSE
    )
  }

  invisible(result_df)
}
