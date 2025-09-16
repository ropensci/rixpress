test_that("rxp_populate generates expected assets", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("rix")

  # Create temporary directory
  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  rix::rix(
    date = "2025-04-11",
    r_pkgs = "dplyr",
    py_conf = list(
      py_version = "3.13",
      py_pkgs = "xgboost"
    ),
    git_pkgs = list(
      package_name = "rixpress",
      repo_url = "https://github.com/ropensci/rixpress",
      commit = "HEAD"
    ),
    ide = "rstudio",
    project_path = temp_dir,
    overwrite = TRUE
  )

  derivs <- list(
    rxp_py(
      name = "mdl",
      expr = "XGBClassifier()"
    )
  )

  rxp_populate(
    derivs,
    project_path = temp_dir,
    py_imports = c(
      xgboost = "from xgboost import XGBClassifier"
    )
  )

  result <- readLines(paste0(temp_dir, "/_rixpress/default_libraries.py"))

  snapshot_gen_population_pipeline <- function(result) {
    tfile <- tempfile(pattern = "default_libraries", fileext = ".py")
    writeLines(result, tfile)
    tfile
  }

  testthat::expect_snapshot_file(
    path = snapshot_gen_population_pipeline(result),
    name = "default_libraries.py"
  )
})
