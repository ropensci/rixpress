test_that("rxp_r: generates correct list", {
  d1 <- rxp_r(mtcars_am, dplyr::filter(mtcars, am == 1))
  testthat::expect_equal(
    d1,
    list(
      "name" = "mtcars_am",
      "snippet" = '  mtcars_am = makeRDerivation {\n    name = \"mtcars_am\";\n    buildPhase = \'\'\n      Rscript -e \"\n        source(\'libraries.R\')\n        mtcars_am <- dplyr::filter(mtcars, am == 1)\n        saveRDS(mtcars_am, \'mtcars_am.rds\')\"\n    \'\';\n  };',
      "type" = "rxp_r"
    )
  )
})
