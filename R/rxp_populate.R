#' Generate Nix Pipeline Code without Building
#'
#' This function generates a `pipeline.nix` file based on a list of derivation
#' objects but does not build the pipeline. It is equivalent to calling
#' `rixpress(build = FALSE)` but with additional support for inline Python 
#' import adjustments. After generating the pipeline, you can build it using
#' `rxp_make()`.
#'
#' @family pipeline functions
#' @param derivs A list of derivation objects, where each object is a list of
#'   five elements:
#'     - `$name`, character, name of the derivation
#'     - `$snippet`, character, the nix code snippet to build this derivation
#'     - `$type`, character, can be R, Python or Quarto
#'     - `$additional_files`, character vector of paths to files to make
#'        available to build sandbox
#'     - `$nix_env`, character, path to Nix environment to build this derivation
#'   A single deriv is the output of `rxp_r()`, `rxp_qmd()` or `rxp_py()`
#'   function.
#'
#' @param project_path Path to root of project, defaults to ".".
#'
#' @param py_imports Named list for Python import adjustments. Names should be
#'   the old import statements to replace, and values should be the new import
#'   statements. For example: `list("import pillow" = "from PIL import Image",
#'   "import numpy" = "import numpy as np")`. If NULL (default), no import
#'   adjustments are made.
#'
#' @return Nothing, writes a file called `pipeline.nix` with the Nix code to
#'   build the pipeline.
#'
#' @details
#' This function provides a two-step workflow that is particularly useful for
#' Python projects:
#' 1. `rxp_populate()` - Generate the pipeline and optionally adjust Python imports
#' 2. `rxp_make()` - Build the pipeline
#'
#' The `py_imports` parameter allows you to make common Python import adjustments
#' inline, without needing to call `adjust_import()` separately. This is especially
#' useful for packages like `pillow` (which needs `from PIL import Image` instead
#' of `import pillow`) or when you want to use common aliases like `import numpy as np`.
#'
#' @examples
#' \dontrun{
#' # Basic usage without import adjustments
#' d1 <- rxp_r(mtcars_am, filter(mtcars, am == 1))
#' d2 <- rxp_r(mtcars_head, head(mtcars_am))
#' list_derivs <- list(d1, d2)
#' 
#' rxp_populate(derivs = list_derivs)
#' rxp_make()
#'
#' # Python project with import adjustments
#' derivs <- list(
#'   rxp_py(data_load, "df = pd.read_csv('data.csv')"),
#'   rxp_py(image_proc, "img = Image.open('photo.jpg')")
#' )
#'
#' rxp_populate(
#'   derivs = derivs,
#'   py_imports = list(
#'     "import pandas" = "import pandas as pd",
#'     "import pillow" = "from PIL import Image"
#'   )
#' )
#' rxp_make()
#' }
#' @export
rxp_populate <- function(derivs, project_path = ".", py_imports = NULL) {
  # Generate the pipeline without building
  rixpress(derivs = derivs, project_path = project_path, build = FALSE)
  
  # Apply Python import adjustments if provided
  if (!is.null(py_imports)) {
    if (!is.list(py_imports) || is.null(names(py_imports))) {
      stop("py_imports must be a named list where names are old imports and values are new imports")
    }
    
    for (old_import in names(py_imports)) {
      new_import <- py_imports[[old_import]]
      adjust_import(
        old_import = old_import,
        new_import = new_import,
        project_path = project_path
      )
    }
  }
  
  invisible(NULL)
}