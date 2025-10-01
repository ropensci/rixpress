testthat::skip_on_cran()

test_that("rxp_inspect throws error when build log is missing", {
  expect_error(
    rxp_inspect(),
    "No build logs found, did you build the pipeline?"
  )
})

test_that("rxp_inspect correctly reads build log", {
  # Create a temporary directory for the test
  temp_dir <- tempdir()
  old_wd <- getwd()
  rixpress_dir <- file.path(temp_dir, "_rixpress")
  dir.create(rixpress_dir, recursive = TRUE, showWarnings = FALSE)

  # Change to temp directory
  setwd(temp_dir)
  on.exit(setwd(old_wd), add = TRUE)

  # Create mock build log data frame
  mock_build_log <- data.frame(
    derivation = c(
      "all-derivations",
      "mtcars",
      "mtcars_am",
      "mtcars_head",
      "mtcars_mpg",
      "mtcars_tail",
      "page"
    ),
    build_success = rep(TRUE, 7),
    path = c(
      "/nix/store/j77yah0z3jsnangrfg18mx5shpbv16bd-all-derivations",
      "/nix/store/8gk580ivgpd3hssdbasgl9mn9dnj6fjv-mtcars",
      "/nix/store/1709wy6ky5a408vaf8rxl04kdiyb5k0f-mtcars_am",
      "/nix/store/a9ahnfdx06cvny0f2gjvqvgrw81q4wf6-mtcars_head",
      "/nix/store/hf6ql2f5lci20zfkw1h5b5w10b4bsznr-mtcars_mpg",
      "/nix/store/96lnlag5bvdyph32wq27fjxw4pc6c85n-mtcars_tail",
      "/nix/store/lfzl996k7py3138wik49b8flklpa9dj3-page"
    ),
    output = c(
      "images, ....",
      "mtcars",
      "mtcars_am",
      "mtcars_head",
      "mtcars_mpg",
      "mtcars_tail",
      "images, ...."
    ),
    stringsAsFactors = FALSE
  )

  # Save mock build log as JSON
  jsonlite::write_json(
    mock_build_log,
    file.path(rixpress_dir, "build_log.json"),
    pretty = TRUE,
    auto_unbox = TRUE,
    dataframe = "rows"
  )

  # Test that rxp_inspect returns the correct data frame
  expected <- rxp_inspect()

  expect_equal(mock_build_log, expected)
})
