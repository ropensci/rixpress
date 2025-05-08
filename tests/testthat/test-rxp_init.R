test_that("rxp_init creates expected files", {
  testthat::skip_on_cran()
  # Create a temporary directory for testing
  test_dir <- tempfile("rxp_init_test")
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  # Call rxp_init with skip_prompt=TRUE to avoid interactivity during testing
  rxp_init(project_path = test_dir, skip_prompt = TRUE)

  # Check if files were created
  env_file <- file.path(test_dir, "gen-env.R")
  pipeline_file <- file.path(test_dir, "gen-pipeline.R")

  expect_true(file.exists(env_file))
  expect_true(file.exists(pipeline_file))

  # Create snapshot function to capture file contents
  capture_file_content <- function(filepath, filename) {
    content <- readLines(filepath)
    output_file <- tempfile(
      pattern = paste0("rxp_init_", filename),
      fileext = ".R"
    )
    writeLines(content, output_file)
    output_file
  }

  # Test the content of generated files using snapshots
  testthat::expect_snapshot_file(
    path = capture_file_content(env_file, "env"),
    name = "rxp_init_gen-env.R"
  )

  testthat::expect_snapshot_file(
    path = capture_file_content(pipeline_file, "pipeline"),
    name = "rxp_init_gen-pipeline.R"
  )
})
