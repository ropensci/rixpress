testthat::skip_on_cran()

test_that("rxp_ga creates expected GitHub Actions workflow file", {
  # Create a temporary directory for testing
  test_dir <- tempfile("rxp_ga_test")
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  # Call rxp_ga
  workflow_file <- rxp_ga()

  # Capture file content for snapshot testing
  capture_file_content <- function(filepath, filename) {
    content <- readLines(filepath)
    output_file <- tempfile(
      pattern = paste0("rxp_ga_", filename),
      fileext = ".yaml"
    )
    writeLines(content, output_file)
    output_file
  }

  # Test the content of generated workflow file using snapshot
  testthat::expect_snapshot_file(
    path = capture_file_content(workflow_file, "workflow"),
    name = "rxp_ga_github_workflow.yaml"
  )
})
