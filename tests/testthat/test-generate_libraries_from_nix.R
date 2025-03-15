test_that("generate_libraries_from_nix: generate R script by parsing default.nix", {
  path_tmpdir <- tempdir()

  dir.create(paste0(path_tmpdir, "/", "_rixpress"))

  on.exit(unlink(path_tmpdir, recursive = TRUE), add = TRUE, after = TRUE)

  rix::rix(
    date = "2025-03-10",
    r_pkgs = c("dplyr", "tidyr", "stringi"),
    project_path = path_tmpdir,
    overwrite = TRUE
  )

  generate_libraries_from_nix("default.nix", file.path(path_tmpdir, "/"))

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
