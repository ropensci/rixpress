test_that("rixpress: gen_flat_pipeline", {
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
  generate_dag(derivs, output_file = dag_out)

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
  path_tmpdir <- "tempdir"
  dir.create(path_tmpdir)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE, after = TRUE)

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
  generate_dag(derivs, output_file = dag_out)

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
