test_that("rxp_inspect throws error when build log is missing", {
  expect_error(
    rxp_inspect(),
    "Build log not found, did you run build the pipeline?"
  )
})
