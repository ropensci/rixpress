test_that("rixpress: helper functions get_need_r/py work", {
  types_r_only <- c("rxp_r", "rxp_r_file")
  types_py_only <- c("rxp_py", "rxp_py_file")
  types_quarto <- c("rxp_qmd")
  types_transfer <- c("rxp_py2r", "rxp_r2py")
  types_mixed <- c("rxp_r", "rxp_py", "rxp_qmd", "rxp_py2r")

  expect_true(get_need_r(types_r_only))
  expect_false(get_need_py(types_r_only))

  expect_false(get_need_r(types_py_only))
  expect_true(get_need_py(types_py_only))

  expect_true(get_need_r(types_quarto))
  expect_false(get_need_py(types_quarto))

  expect_true(get_need_r(types_transfer))
  expect_false(get_need_py(types_transfer)) # Transfer functions are R-based

  expect_true(get_need_r(types_mixed))
  expect_true(get_need_py(types_mixed))
})

test_that("rixpress: gen_flat_pipeline", {
  testthat::skip_on_cran()
  d1 <- rxp_r(mtcars_am, dplyr::filter(mtcars, am == 1))
  d2 <- rxp_r(mtcars_head, head(mtcars_am))
  derivs <- list(d1, d2)

  flat_pipeline <- gen_flat_pipeline(derivs)

  snapshot_gen_flat_pipeline <- function(derivs, flat_pipeline) {
    tfile <- tempfile(pattern = "flat_pipeline", fileext = ".nix")
    writeLines(flat_pipeline, tfile)
    tfile
  }

  testthat::expect_snapshot_file(
    path = snapshot_gen_flat_pipeline(derivs, flat_pipeline),
    name = "flat_pipeline.nix"
  )

  dag_out <- tempfile(pattern = "dag", fileext = ".json")
  rxp_write_dag(derivs, output_file = dag_out)

  snapshot_gen_pipeline <- function(dag_out, flat_pipeline) {
    pipeline <- gen_pipeline(
      dag_file = dag_out,
      flat_pipeline = flat_pipeline
    )

    tfile <- tempfile(pattern = "pipeline", fileext = ".nix")
    writeLines(pipeline, tfile)
    tfile
  }

  testthat::expect_snapshot_file(
    path = snapshot_gen_pipeline(dag_out, flat_pipeline),
    name = "pipeline.nix"
  )
})

test_that("rixpress: several environments", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("rix")

  path_tmpdir <- "tempdir"
  dir.create(path_tmpdir)
  on.exit(unlink(path_tmpdir, recursive = TRUE), add = TRUE, after = TRUE)

  path_to_default.nix <- paste0(path_tmpdir, "/default.nix")
  path_to_default2.nix <- paste0(path_tmpdir, "/default2.nix")

  rix::rix(date = "2025-03-10", r_pkgs = "dplyr", project_path = path_tmpdir)
  file.rename(
    from = path_to_default.nix,
    to = path_to_default2.nix
  )

  d1 <- rxp_r(mtcars_am, dplyr::filter(mtcars, am == 1))
  d2 <- rxp_r(mtcars_head, head(mtcars_am), nix_env = path_to_default2.nix)
  derivs <- list(d1, d2)

  flat_pipeline <- gen_flat_pipeline(derivs)

  snapshot_gen_flat_pipeline <- function(derivs, flat_pipeline) {
    tfile <- tempfile(pattern = "flat_pipeline", fileext = ".nix")
    writeLines(flat_pipeline, tfile)
    tfile
  }

  testthat::expect_snapshot_file(
    path = snapshot_gen_flat_pipeline(derivs, flat_pipeline),
    name = "flat_pipeline_several_envs.nix"
  )

  dag_out <- tempfile(pattern = "dag", fileext = ".json")
  rxp_write_dag(derivs, output_file = dag_out)

  snapshot_gen_pipeline <- function(dag_out, flat_pipeline) {
    pipeline <- gen_pipeline(
      dag_file = dag_out,
      flat_pipeline = flat_pipeline
    )

    tfile <- tempfile(pattern = "pipeline", fileext = ".nix")
    writeLines(pipeline, tfile)
    tfile
  }

  testthat::expect_snapshot_file(
    path = snapshot_gen_pipeline(dag_out, flat_pipeline),
    name = "pipeline_several_envs.nix"
  )
})

test_that("parse_nix_envs handles different environments correctly", {
  # Create temporary directory for test files
  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  # Create _rixpress directory
  rixpress_dir <- file.path(temp_dir, "_rixpress")
  dir.create(rixpress_dir)

  # Create mock library files
  writeLines("# R libraries", file.path(rixpress_dir, "default_r_libraries.R"))
  writeLines(
    "# Python libraries",
    file.path(rixpress_dir, "py_env_py_libraries.py")
  )

  # Set working directory to temp_dir for the test
  old_wd <- getwd()
  setwd(temp_dir)
  on.exit(setwd(old_wd), add = TRUE)

  # Create test derivations with different environments
  d1 <- list(
    name = "test_r",
    snippet = "test_r_snippet",
    type = "rxp_r",
    additional_files = "",
    nix_env = "default.nix",
    base_name = "default"
  )

  d2 <- list(
    name = "test_py",
    snippet = "test_py_snippet",
    type = "rxp_py",
    additional_files = "",
    nix_env = "data-raw/py2r_example/py-env.nix",
    base_name = "py_env"
  )

  # Test the function
  result <- parse_nix_envs(list(d1, d2))

  # Check that the result contains expected strings for both environments
  expect_true(grepl("default = import ./default.nix;", result))
  expect_true(grepl(
    "py_env = import ./data-raw/py2r_example/py-env.nix;",
    result
  ))
  expect_true(grepl("defaultConfigurePhase", result))
  expect_true(grepl("py_envConfigurePhase", result))
})

test_that("rixpress correctly handles dependencies", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("rix")

  # Create temporary directory
  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  setwd(temp_dir)

  rix::rix(
    date = "2025-04-11",
    r_pkgs = "dplyr",
    git_pkgs = list(
      package_name = "rixpress",
      repo_url = "https://github.com/ropensci/rixpress",
      commit = "HEAD"
    ),
    ide = "rstudio",
    project_path = ".",
    overwrite = TRUE
  )

  # Create actual derivations
  d1 <- rxp_r(data_prep, mtcars)
  d2 <- rxp_r(analysis, summary(data_prep), nix_env = "default.nix")
  d3 <- rxp_py(
    py_process,
    "pd.DataFrame({'a': [1, 2, 3]})",
    nix_env = "default.nix"
  )

  # Create derivations list
  derivs <- list(d1, d2, d3)

  rxp_populate(derivs, project_path = ".", build = FALSE)

  result <- readLines("pipeline.nix")

  # Create snapshot of the pipeline
  snapshot_pipeline <- function(result) {
    tfile <- tempfile(pattern = "pipeline_with_deps", fileext = ".nix")
    writeLines(result, tfile)
    tfile
  }

  testthat::expect_snapshot_file(
    path = snapshot_pipeline(result),
    name = "pipeline_with_dependencies.nix"
  )
})
