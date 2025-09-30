testthat::skip_on_cran()

test_that("rxp_write_dag: generate correct json of dag R derivs only", {
  d1 <- rxp_r(mtcars_am, dplyr::filter(mtcars, am == 1))
  d2 <- rxp_r(mtcars_head, head(mtcars_am))
  derivs <- list(d1, d2)
  testthat::expect_snapshot_file(
    path = rxp_write_dag(derivs),
    name = "r-dag.json"
  )
})

test_that("rxp_write_dag: generate correct json of dag Py derivs only", {
  d1 <- rxp_py(mtcars_am, 'mtcars[mtcars["am"] == 1]')
  d2 <- rxp_py(mtcars_head, "mtcars_am.head()")
  derivs <- list(d1, d2)
  testthat::expect_snapshot_file(
    path = rxp_write_dag(derivs),
    name = "py-dag.json"
  )
})

test_that("rxp_write_dag: generate correct json of dag R derivs only, unsorted", {
  d1 <- rxp_r(mtcars_am, dplyr::filter(mtcars, am == 1))
  d2 <- rxp_r(mtcars_head, head(mtcars_am))
  derivs <- list(d2, d1)
  testthat::expect_snapshot_file(
    path = rxp_write_dag(derivs),
    name = "r-dag-unsorted.json"
  )
})
