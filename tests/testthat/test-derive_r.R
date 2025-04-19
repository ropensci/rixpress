test_that("rxp_r: generates correct list", {
  d1 <- rxp_r(mtcars_am, dplyr::filter(mtcars, am == 1))
  testthat::expect_equal(
    d1,
    list(
      "name" = "mtcars_am",
      "snippet" = '  mtcars_am = makeRDerivation {\n    name = \"mtcars_am\";\n    buildInputs = defaultBuildInputs;\n    configurePhase = defaultConfigurePhase;\n    buildPhase = \'\'\n      Rscript -e \"\n        source(\'libraries.R\')\n        mtcars_am <- dplyr::filter(mtcars, am == 1)\n        saveRDS(mtcars_am, \'mtcars_am\')\"\n    \'\';\n  };',
      "type" = "rxp_r",
      "additional_files" = "",
      "nix_env" = "default.nix",
      "unserialize_function" = "readRDS"
    )
  )
})
