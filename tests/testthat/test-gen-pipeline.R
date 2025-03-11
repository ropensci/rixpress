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
