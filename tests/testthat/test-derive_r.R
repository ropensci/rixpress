test_that("rxp_r: generates correct list", {
  d1 <- rxp_r(mtcars_am, dplyr::filter(mtcars, am == 1))
  testthat::expect_equal(
    d1,
    structure(
      list(
        "name" = "mtcars_am",
        "snippet" = '  mtcars_am = makeRDerivation {\n    name = \"mtcars_am\";\n    buildInputs = defaultBuildInputs;\n    configurePhase = defaultConfigurePhase;\n    buildPhase = \'\'\n      Rscript -e \"\n        source(\'libraries.R\')\n        mtcars_am <- dplyr::filter(mtcars, am == 1)\n        saveRDS(mtcars_am, \'mtcars_am\')\"\n    \'\';\n  };',
        "type" = "rxp_r",
        "additional_files" = "",
        "nix_env" = "default.nix",
        "serialize_function" = "saveRDS",
        "unserialize_function" = "readRDS"
      ),
      class = "derivation"
    )
  )
})

test_that("rxp_py: generates correct list", {
  d1 <- rxp_py(mtcars_pl_am, "mtcars_pl.filter(pl.col('am') == 1)")
  testthat::expect_equal(
    d1,
    structure(
      list(
        "name" = "mtcars_pl_am",
        "snippet" = '  mtcars_pl_am = makePyDerivation {\n    name = \"mtcars_pl_am\";\n    buildInputs = defaultBuildInputs;\n    configurePhase = defaultConfigurePhase;\n    buildPhase = \'\'\n      python -c "\nexec(open(\'libraries.py\').read())\nexec(\'mtcars_pl_am = mtcars_pl.filter(pl.col(\\\'am\\\') == 1)\')\nwith open(\'mtcars_pl_am\', \'wb\') as f: pickle.dump(globals()[\'mtcars_pl_am\'], f)\n"\n    \'\';\n  };',
        "type" = "rxp_py",
        "additional_files" = "",
        "nix_env" = "default.nix",
        "serialize_function" = "with open('mtcars_pl_am', 'wb') as f: pickle.dump(globals()['mtcars_pl_am'], f)",
        "unserialize_function" = "pickle.load"
      ),
      class = "derivation"
    )
  )
})

test_that("rxp_py: custom serialization functions work", {
  d1 <- rxp_py(
    mtcars_pl_am,
    "mtcars_pl.filter(pl.col('am') == 1)",
    serialize_function = "custom_save",
    unserialize_function = "custom_load"
  )
  
  testthat::expect_equal(d1$serialize_function, "custom_save(globals()['mtcars_pl_am'], 'mtcars_pl_am')")
  testthat::expect_equal(d1$unserialize_function, "custom_load")
})

test_that("rxp_quarto: generates correct list", {
  # Create a temporary qmd file for testing
  qmd_file <- tempfile(fileext = ".qmd")
  writeLines("---\ntitle: Test\n---\n\nThis is a test quarto document with rxp_read(\"test_data\").", qmd_file)
  
  d1 <- rxp_quarto(report, qmd_file, additional_files = "images", args = "--to pdf")
  
  # Extract the name and type from the result
  testthat::expect_equal(d1$name, "report")
  testthat::expect_equal(d1$type, "rxp_quarto")
  testthat::expect_equal(d1$additional_files, "images")
  testthat::expect_equal(d1$qmd_file, qmd_file)
  testthat::expect_equal(d1$nix_env, "default.nix")
  
  # Cleanup
  unlink(qmd_file)
})

test_that("rxp_r_file: single file reading works", {
  # Create a temporary CSV for testing
  csv_file <- tempfile(fileext = ".csv")
  write.csv(mtcars[1:5, ], csv_file, row.names = FALSE)
  
  d1 <- rxp_r_file(mtcars_data, path = csv_file, read_function = read.csv)
  
  testthat::expect_equal(d1$name, "mtcars_data")
  testthat::expect_equal(d1$type, "rxp_r")
  
  # Cleanup
  unlink(csv_file)
})

test_that("rxp_r_file: folder reading works", {
  # Create a temporary folder with data for testing
  test_dir <- tempdir()
  test_data_dir <- file.path(test_dir, "test_data")
  dir.create(test_data_dir, showWarnings = FALSE)
  
  write.csv(mtcars[1:5, ], file.path(test_data_dir, "data1.csv"), row.names = FALSE)
  write.csv(mtcars[6:10, ], file.path(test_data_dir, "data2.csv"), row.names = FALSE)
  
  d1 <- rxp_r_file(
    all_data, 
    path = test_data_dir, 
    read_function = function(x) lapply(list.files(x, full.names = TRUE, pattern = "\\.csv$"), read.csv),
    copy_data_folder = TRUE
  )
  
  testthat::expect_equal(d1$name, "all_data")
  testthat::expect_equal(d1$type, "rxp_r")
  
  # Cleanup
  unlink(test_data_dir, recursive = TRUE)
})

test_that("rxp_py_file: basic functionality works", {
  # Create a temporary CSV for testing
  csv_file <- tempfile(fileext = ".csv")
  write.csv(mtcars[1:5, ], csv_file, row.names = FALSE)
  
  d1 <- rxp_py_file(mtcars_data, path = csv_file, read_function = "pandas.read_csv")
  
  testthat::expect_equal(d1$name, "mtcars_data")
  testthat::expect_equal(d1$type, "rxp_py")
  
  # Cleanup
  unlink(csv_file)
})

# Mock reticulate package for testing
mockReticulate <- function() {
  if (!exists("reticulate", envir = globalenv())) {
    reticulate <- new.env()
    assign("reticulate", reticulate, envir = globalenv())
  }
}

test_that("rxp_py2r: generates correct list", {
  mockReticulate()
  
  d1 <- rxp_py2r(r_data, py_data)
  
  testthat::expect_equal(d1$name, "r_data")
  testthat::expect_equal(d1$type, "rxp_py2r")
})

test_that("rxp_r2py: generates correct list", {
  mockReticulate()
  
  d1 <- rxp_r2py(py_data, r_data)
  
  testthat::expect_equal(d1$name, "py_data")
  testthat::expect_equal(d1$type, "rxp_r2py")
})

test_that("print.derivation: outputs expected format", {
  d1 <- rxp_r(mtcars_am, dplyr::filter(mtcars, am == 1))
  
  # Capture the output of print.derivation
  output <- capture.output(print(d1))
  
  testthat::expect_true(any(grepl("Name: mtcars_am", output)))
  testthat::expect_true(any(grepl("Type: rxp_r", output)))
  testthat::expect_true(any(grepl("Serialize function: saveRDS", output)))
  testthat::expect_true(any(grepl("Unserialize function: readRDS", output)))
})

test_that("rxp_r: custom serialization functions work", {
  # Test with custom serialization and unserialization functions
  d1 <- rxp_r(
    mtcars_am, 
    dplyr::filter(mtcars, am == 1), 
    serialize_function = qs::qsave,
    unserialize_function = qs::qread
  )
  
  testthat::expect_equal(d1$serialize_function, "qs::qsave")
  testthat::expect_equal(d1$unserialize_function, "qs::qread")
})

test_that("rxp_r: with additional files", {
  d1 <- rxp_r(
    mtcars_am, 
    dplyr::filter(mtcars, am == 1), 
    additional_files = c("functions.R", "data.csv")
  )
  
  testthat::expect_equal(d1$additional_files, c("functions.R", "data.csv"))
  # Check that the src_snippet is included in the main snippet
  testthat::expect_true(grepl("fileset = defaultPkgs.lib.fileset.unions", d1$snippet))
  testthat::expect_true(grepl("./data.csv", d1$snippet))
})

test_that("rxp_py: with additional files", {
  d1 <- rxp_py(
    mtcars_pl_am, 
    "mtcars_pl.filter(pl.col('am') == 1)", 
    additional_files = c("functions.py", "data.csv")
  )
  
  testthat::expect_equal(d1$additional_files, c("functions.py", "data.csv"))
  # Check that the src_snippet is included in the main snippet
  testthat::expect_true(grepl("fileset = defaultPkgs.lib.fileset.unions", d1$snippet))
  testthat::expect_true(grepl("./data.csv", d1$snippet))
})

test_that("rxp_common_setup: handles invalid direction", {
  testthat::expect_error(
    rxp_common_setup("test_name", "test_expr", "default.nix", "invalid_direction"),
    "Invalid direction. Use 'py2r' or 'r2py'."
  )
})

test_that("rxp_file_common: URL handling works", {
  # Mock system to return a fake hash for URL fetching
  mockSystem <- function(...) "fake_hash"
  
  with_mock(
    `base::system` = mockSystem,
    d1 <- rxp_file_common(
      "test_data",
      "https://example.com/data.csv",
      "default.nix",
      "test build phase",
      "rxp_r",
      "makeRDerivation",
      "R"
    )
  )
  
  testthat::expect_true(grepl("defaultPkgs.fetchurl", d1$snippet))
  testthat::expect_true(grepl("https://example.com/data.csv", d1$snippet))
})
