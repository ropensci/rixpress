test_that("generate_dag: generate correct json of dag", {
  d1 <- mk_r(mtcars_am, dplyr::filter(mtcars, am == 1))
  d2 <- mk_r(mtcars_head, head(mtcars_am))
  derivs <- list(d1, d2)
  testthat::expect_snapshot_file(
    path = generate_dag(list(d1, d2)),
    name = "dag.json"
  )
})
