test_that("generate_r_libraries_from_nix: generate R script by parsing default.nix", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("rix")
  path_tmpdir <- tempdir()

  dir.create(paste0(path_tmpdir, "/", "_rixpress"))

  on.exit(unlink(path_tmpdir, recursive = TRUE), add = TRUE, after = TRUE)

  rix::rix(
    date = "2025-03-10",
    r_pkgs = c("dplyr", "tidyr", "stringi"),
    project_path = path_tmpdir,
    overwrite = TRUE
  )

  generate_r_libraries_from_nix(
    nix_file = "default.nix",
    project_path = file.path(path_tmpdir, "/")
  )

  library_lines <- readLines(file.path(
    path_tmpdir,
    "_rixpress",
    "default_libraries.R"
  ))

  testthat::expect_equal(
    library_lines,
    c("library(dplyr)", "library(stringi)", "library(tidyr)")
  )
})


test_that("generate_py_libraries_from_nix: generate Py script by parsing default.nix", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("rix")

  path_tmpdir <- tempdir()

  dir.create(paste0(path_tmpdir, "/", "_rixpress"))

  on.exit(unlink(path_tmpdir, recursive = TRUE), add = TRUE, after = TRUE)

  rix::rix(
    date = "2025-03-10",
    r_pkgs = "reticulate",
    py_conf = list(
      py_version = "3.12",
      py_pkgs = c("polars", "great-tables")
    ),
    project_path = path_tmpdir,
    overwrite = TRUE
  )

  generate_py_libraries_from_nix(
    nix_file = "default.nix",
    project_path = file.path(path_tmpdir, "/")
  )

  library_lines <- readLines(file.path(
    path_tmpdir,
    "_rixpress",
    "default_libraries.py"
  ))

  testthat::expect_equal(
    library_lines,
    c("import great-tables", "import pickle", "import polars")
  )
})
