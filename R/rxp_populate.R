#' Populate the pipeline (generate files) without building
#'
#' rxp_populate() generates the pipeline assets (e.g., _rixpress/, pipeline.nix,
#' dag.json) but does not build. It is equivalent to calling rixpress(build =
#' FALSE). Additionally, you can inline Python import adjustments via
#' `py_imports`.
#'
#' @family pipeline functions
#' @param derivs A list of derivation objects created by rxp_*() helpers.
#' @param project_path Path to the project root. Defaults to ".".
#' @param py_imports Named character vector of Python import rewrites. Names are
#'   the base modules that rixpress auto-imports as "import <name>", and values
#'   are the desired import lines. For example: c(numpy = "import numpy as np",
#'   pillow = "from PIL import Image"). Each entry is applied by replacing
#'   "import <name>" with the provided string across generated _rixpress Python
#'   library files.
#' @param ... Ignored when build = FALSE; kept for signature parity with
#'   rixpress().
#' @return Invisibly returns NULL. Side effects: writes generated files.
#' @examples
#' \dontrun{
#' list(
#'   rxp_py(
#'     name = "mdl",
#'     py_expr = "XGBClassifier()"
#'   )
#' ) |>
#'   rxp_populate(
#'     py_imports = c(
#'       numpy   = "import numpy as np",
#'       xgboost = "from xgboost import XGBClassifier"
#'     )
#'   )
#'
#' rxp_make()
#' }
#' @export
rxp_populate <- function(derivs, project_path = ".", py_imports = NULL, ...) {
  rixpress(
    derivs = derivs,
    project_path = project_path,
    build = FALSE,
    py_imports = py_imports,
    ...
  )
  invisible(NULL)
}
