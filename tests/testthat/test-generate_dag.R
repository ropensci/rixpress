test_that("generate_dag: generate correct json of dag", {
  d1 <- list(
    name = "mtcars_am",
    snippet = "mtcars_am <- filter(mtcars, am == 1)\nsaveRDS(mtcars_am, 'mtcars_am.rds')"
  )
  d2 <- list(
    name = "mtcars_head",
    snippet = "mtcars_head <- head(mtcars_am)\nsaveRDS(mtcars_head, 'mtcars_head.rds')"
  )
  testthat::expect_snapshot_file(
    path = generate_dag(list(d1, d2)),
    name = "dag.json"
  )
})
