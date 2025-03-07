test_that("generate_libraries_from_nix: generate R script by parsing default.nix", {
  outfile <- tempfile("libraries", fileext = ".R")

  snapshot_gen_libs_from_nix <- function(nix_file, outfile) {
    generate_libraries_from_nix(
      nix_file,
      outfile = outfile
    )
    outfile
  }

  testthat::expect_snapshot_file(
    path = snapshot_gen_libs_from_nix("test-data/default.nix", outfile),
    name = "libraries.R"
  )
})
