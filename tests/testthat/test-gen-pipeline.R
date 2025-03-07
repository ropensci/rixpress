test_that("gen_flat_pipeline: test if it returns flat pipeline", {
  d1 <- mk_r(mtcars_am, dplyr::filter(mtcars, am == 1))
  d2 <- mk_r(mtcars_head, head(mtcars_am))

  derivs <- list(d1, d2)

  testthat::expect_equal(
    d1,
    list(
      "name" = "mtcars_am",
      "snippet" = '  mtcars_am = makeRDerivation {\n    name = \"mtcars_am\";\n    buildPhase = \'\'\n      Rscript -e \"\n        source(\'libraries.R\')\n        mtcars_am <- dplyr::filter(mtcars, am == 1)\n        saveRDS(mtcars_am, \'mtcars_am.rds\')\"\n    \'\';\n  };'
    )
  )
})
