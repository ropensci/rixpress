test_that("rxp_copy: test for failure with non-existent derivation", {
  testthat::skip_if_not_installed("mockery")
  # Create a temporary mock of build_log.rds
  mock_build_log <- data.frame(
    derivation = c("existing_derivation1", "existing_derivation2"),
    build_success = c(TRUE, TRUE),
    path = c("/nix/store/path1", "/nix/store/path2"),
    output = I(list("output1", "output2"))
  )

  # Create temp directory structure for the test
  temp_dir <- tempdir()
  rixpress_dir <- file.path(temp_dir, "_rixpress")
  dir.create(rixpress_dir, recursive = TRUE, showWarnings = FALSE)

  # Save mock build log
  saveRDS(mock_build_log, file.path(rixpress_dir, "build_log.rds"))

  # Temporarily modify working directory
  old_wd <- getwd()
  setwd(temp_dir)

  # Mock rxp_inspect to return our mock data
  mockery::stub(rxp_copy, "rxp_inspect", mock_build_log)

  # Test that rxp_copy errors for a non-existent derivation
  expect_error(
    rxp_copy("non_existent_derivation"),
    "No derivation non_existent_derivation found in the Nix store"
  )

  # Clean up
  setwd(old_wd)
})
