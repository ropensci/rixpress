test_that("generate_dag: generate correct json of dag R derivs only", {
  dag_out <- tempfile(pattern = "dag", fileext = ".json")
  on.exit(unlink(dag_out), add = TRUE, after = TRUE)

  d1 <- rxp_r(mtcars_am, dplyr::filter(mtcars, am == 1))
  d2 <- rxp_r(mtcars_head, head(mtcars_am))
  derivs <- list(d1, d2)
  testthat::expect_snapshot_file(
    path = generate_dag(list(d1, d2), output_file = dag_out),
    name = "r-dag.json"
  )
})

test_that("generate_dag: generate correct json of dag Py derivs only", {
  dag_out <- tempfile(pattern = "dag", fileext = ".json")
  on.exit(unlink(dag_out), add = TRUE, after = TRUE)

  d1 <- rxp_py(mtcars_am, 'mtcars[mtcars["am"] == 1]')
  d2 <- rxp_py(mtcars_head, "mtcars_am.head()")
  derivs <- list(d1, d2)
  testthat::expect_snapshot_file(
    path = generate_dag(list(d1, d2), output_file = dag_out),
    name = "py-dag.json"
  )
})
