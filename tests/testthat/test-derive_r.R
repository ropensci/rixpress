test_that("rxp_r: generates correct list", {
  d1 <- rxp_r(mtcars_am, dplyr::filter(mtcars, am == 1))
  testthat::expect_equal(
    d1,
    structure(
      list(
        "name" = "mtcars_am",
        "snippet" = '  mtcars_am = makeRDerivation {\n    name = \"mtcars_am\";\n    buildInputs = defaultBuildInputs;\n    configurePhase = defaultConfigurePhase;\n    buildPhase = \'\'\n      Rscript -e \"\n        source(\'libraries.R\')\n        # RIXPRESS_LOAD_DEPENDENCIES_HERE:mtcars_am\n        mtcars_am <- dplyr::filter(mtcars, am == 1)\n        saveRDS(mtcars_am, \'mtcars_am\')\"\n    \'\';\n  };',
        "type" = "rxp_r",
        "additional_files" = "",
        "user_functions" = "",
        "nix_env" = "default.nix",
        "encoder" = "saveRDS",
        "decoder" = "readRDS",
        "env_var" = NULL,
        "noop_build" = FALSE
      ),
      class = "rxp_derivation"
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
        "snippet" = '  mtcars_pl_am = makePyDerivation {\n    name = \"mtcars_pl_am\";\n    buildInputs = defaultBuildInputs;\n    configurePhase = defaultConfigurePhase;\n    buildPhase = \'\'\n      python -c "\nexec(open(\'libraries.py\').read())\n# RIXPRESS_PY_LOAD_DEPENDENCIES_HERE:mtcars_pl_am\nexec(\'mtcars_pl_am = mtcars_pl.filter(pl.col(\\\'am\\\') == 1)\')\nwith open(\'mtcars_pl_am\', \'wb\') as f: pickle.dump(globals()[\'mtcars_pl_am\'], f)\n"\n    \'\';\n  };',
        "type" = "rxp_py",
        "additional_files" = "",
        "user_functions" = "",
        "nix_env" = "default.nix",
        "encoder" = "with open('mtcars_pl_am', 'wb') as f: pickle.dump(globals()['mtcars_pl_am'], f)",
        "decoder" = "pickle.load",
        "env_var" = NULL,
        "noop_build" = FALSE
      ),
      class = "rxp_derivation"
    )
  )
})

test_that("rxp_py: custom serialization functions work", {
  d1 <- rxp_py(
    mtcars_pl_am,
    "mtcars_pl.filter(pl.col('am') == 1)",
    encoder = "custom_save",
    decoder = "custom_load"
  )

  # Test the entire object
  testthat::expect_equal(
    d1,
    structure(
      list(
        "name" = "mtcars_pl_am",
        "snippet" = '  mtcars_pl_am = makePyDerivation {\n    name = \"mtcars_pl_am\";\n    buildInputs = defaultBuildInputs;\n    configurePhase = defaultConfigurePhase;\n    buildPhase = \'\'\n      python -c "\nexec(open(\'libraries.py\').read())\n# RIXPRESS_PY_LOAD_DEPENDENCIES_HERE:mtcars_pl_am\nexec(\'mtcars_pl_am = mtcars_pl.filter(pl.col(\\\'am\\\') == 1)\')\ncustom_save(globals()[\'mtcars_pl_am\'], \'mtcars_pl_am\')\n"\n    \'\';\n  };',
        "type" = "rxp_py",
        "additional_files" = "",
        "user_functions" = "",
        "nix_env" = "default.nix",
        "encoder" = "custom_save(globals()['mtcars_pl_am'], 'mtcars_pl_am')",
        "decoder" = "custom_load",
        "env_var" = NULL,
        "noop_build" = FALSE
      ),
      class = "rxp_derivation"
    )
  )
})

test_that("rxp_qmd: generates correct list", {
  # Create a temporary qmd file for testing
  qmd_file <- tempfile(fileext = ".qmd")
  on.exit(unlink(qmd_file))
  writeLines(
    "---\ntitle: Test\n---\n\nThis is a test quarto document with rxp_read(\"test_data\").",
    qmd_file
  )

  d1 <- suppressWarnings(rxp_qmd(
    report,
    qmd_file,
    additional_files = "images",
    args = "--to pdf"
  ))

  testthat::expect_equal(
    d1,
    structure(
      list(
        "name" = "report",
        "snippet" = paste0(
          '  report = defaultPkgs.stdenv.mkDerivation {\n    name = "report";\n    src = defaultPkgs.lib.fileset.toSource {\n      root = ./.;\n      fileset = defaultPkgs.lib.fileset.unions [ ./',
          qmd_file,
          ' ./images ];\n    };\n    buildInputs = defaultBuildInputs;\n    configurePhase = defaultConfigurePhase;\n    buildPhase = \'\'\n      mkdir home\n      export HOME=$PWD/home\n      export RETICULATE_PYTHON=${defaultPkgs.python3}/bin/python\n\n      substituteInPlace ',
          qmd_file,
          ' --replace-fail \'rxp_read("test_data")\' \'rxp_read("${test_data}")\'\n      quarto render ',
          qmd_file,
          ' --to pdf --output-dir $out\n    \'\';\n  };'
        ),
        "type" = "rxp_qmd",
        "qmd_file" = qmd_file,
        "additional_files" = "images",
        "nix_env" = "default.nix",
        "args" = "--to pdf",
        "env_var" = NULL,
        "noop_build" = FALSE
      ),
      class = "rxp_derivation"
    )
  )
})

test_that("rxp_r_file: single file reading works", {
  # Create a temporary CSV for testing
  csv_file <- tempfile(fileext = ".csv")
  on.exit(unlink(csv_file))

  write.csv(mtcars[1:5, ], csv_file, row.names = FALSE)

  d1 <- rxp_r_file(mtcars_data, path = csv_file, read_function = read.csv)

  # Create a subset of d1 with only the fields we want to test
  d1_subset <- list(
    "name" = d1$name,
    "type" = d1$type,
    "path" = csv_file,
    "read_function" = read.csv,
    "nix_env" = d1$nix_env
  )
  class(d1_subset) <- "rxp_derivation"

  testthat::expect_equal(
    d1_subset,
    structure(
      list(
        "name" = "mtcars_data",
        "type" = "rxp_r",
        "path" = csv_file,
        "read_function" = read.csv,
        "nix_env" = "default.nix"
      ),
      class = "rxp_derivation"
    )
  )
})

test_that("rxp_r_file: folder reading works", {
  # Create a temporary folder with data for testing
  test_dir <- tempdir()
  test_data_dir <- file.path(test_dir, "test_data")
  dir.create(test_data_dir, showWarnings = FALSE)
  # Cleanup
  on.exit(unlink(test_data_dir, recursive = TRUE))

  write.csv(
    mtcars[1:5, ],
    file.path(test_data_dir, "data1.csv"),
    row.names = FALSE
  )
  write.csv(
    mtcars[6:10, ],
    file.path(test_data_dir, "data2.csv"),
    row.names = FALSE
  )

  d1 <- rxp_r_file(
    all_data,
    path = test_data_dir,
    read_function = function(x) {
      lapply(list.files(x, full.names = TRUE, pattern = "\\.csv$"), read.csv)
    },
  )

  # Create a subset of d1 with only the fields we want to test
  d1_subset <- list(
    "name" = d1$name,
    "type" = d1$type,
    "path" = test_data_dir,
    "read_function" = function(x) {
      lapply(list.files(x, full.names = TRUE, pattern = "\\.csv$"), read.csv)
    },
    "nix_env" = d1$nix_env
  )
  class(d1_subset) <- "rxp_derivation"

  # Compare function bodies instead of the functions themselves
  testthat::expect_equal(
    body(d1_subset$read_function),
    body(
      function(x) {
        lapply(list.files(x, full.names = TRUE, pattern = "\\.csv$"), read.csv)
      }
    )
  )

  # Remove read_function for direct comparison
  d1_subset$read_function <- NULL
  expected <- structure(
    list(
      "name" = "all_data",
      "type" = "rxp_r",
      "path" = test_data_dir,
      "nix_env" = "default.nix"
    ),
    class = "rxp_derivation"
  )

  testthat::expect_equal(d1_subset, expected)
})

test_that("rxp_py_file: basic functionality works", {
  # Create a temporary CSV for testing
  csv_file <- tempfile(fileext = ".csv")
  on.exit(unlink(csv_file))
  write.csv(mtcars[1:5, ], csv_file, row.names = FALSE)

  d1 <- rxp_py_file(
    mtcars_data,
    path = csv_file,
    read_function = "pandas.read_csv"
  )

  # Create a subset of d1 with only the fields we want to test
  d1_subset <- list(
    "name" = d1$name,
    "type" = d1$type,
    "path" = csv_file,
    "read_function" = "pandas.read_csv",
    "nix_env" = d1$nix_env
  )
  class(d1_subset) <- "rxp_derivation"

  testthat::expect_equal(
    d1_subset,
    structure(
      list(
        "name" = "mtcars_data",
        "type" = "rxp_py",
        "path" = csv_file,
        "read_function" = "pandas.read_csv",
        "nix_env" = "default.nix"
      ),
      class = "rxp_derivation"
    )
  )
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

  d1 <- suppressWarnings(rxp_py2r(r_data, py_data))

  # Create a subset of d1 with only the fields we want to test
  d1_subset <- list(
    "name" = d1$name,
    "type" = d1$type,
    "nix_env" = d1$nix_env
  )
  class(d1_subset) <- "rxp_derivation"

  testthat::expect_equal(
    d1_subset,
    structure(
      list(
        "name" = "r_data",
        "type" = "rxp_py2r",
        "nix_env" = "default.nix"
      ),
      class = "rxp_derivation"
    )
  )
})

test_that("rxp_r2py: generates correct list", {
  mockReticulate()

  d1 <- suppressWarnings(rxp_r2py(py_data, r_data))

  # Test the entire object
  testthat::expect_equal(
    d1,
    structure(
      list(
        "name" = "py_data",
        "snippet" = '  py_data = makeRDerivation {\n    name = "py_data";\n    buildInputs = defaultBuildInputs;\n    configurePhase = defaultConfigurePhase;\n    buildPhase = \'\'\n      export RETICULATE_PYTHON=${defaultPkgs.python3}/bin/python\n       Rscript -e "\n         source(\'libraries.R\')\n         r_data <- readRDS(\'${r_data}/r_data\')\n         reticulate::py_save_object(r_data, \'py_data\', pickle = \'pickle\')"\n    \'\';\n  };',
        "type" = "rxp_r2py",
        "additional_files" = "",
        "nix_env" = "default.nix"
      ),
      class = "rxp_derivation"
    )
  )
})

test_that("print.derivation: outputs expected format", {
  d1 <- rxp_r(mtcars_am, dplyr::filter(mtcars, am == 1))

  # Capture the output of print.derivation
  output <- capture.output(print(d1))

  # Create the expected output
  expected_output <- c(
    "Name: mtcars_am",
    "Type: rxp_r",
    "Serialize function: saveRDS",
    "Unserialize function: readRDS"
  )

  # Check that all expected lines are in the output
  testthat::expect_equal(
    all(sapply(expected_output, function(line) any(grepl(line, output)))),
    TRUE
  )
})

test_that("rxp_r: with additional files", {
  d1 <- rxp_r(
    mtcars_am,
    dplyr::filter(mtcars, am == 1),
    additional_files = "data.csv",
    user_functions = "functions.R"
  )

  # Test the entire object
  testthat::expect_equal(
    d1,
    structure(
      list(
        "name" = "mtcars_am",
        "snippet" = '  mtcars_am = makeRDerivation {\n    name = "mtcars_am";\n     src = defaultPkgs.lib.fileset.toSource {\n      root = ./.;\n      fileset = defaultPkgs.lib.fileset.unions [ ./data.csv ./functions.R ];\n    };\n    buildInputs = defaultBuildInputs;\n    configurePhase = defaultConfigurePhase;\n    buildPhase = \'\'\n      cp -r $src/* .\n      Rscript -e "\n        source(\'libraries.R\')\n        # RIXPRESS_LOAD_DEPENDENCIES_HERE:mtcars_am\n        source(\'functions.R\')\n        mtcars_am <- dplyr::filter(mtcars, am == 1)\n        saveRDS(mtcars_am, \'mtcars_am\')"\n    \'\';\n  };',
        "type" = "rxp_r",
        "additional_files" = "data.csv",
        "user_functions" = "functions.R",
        "nix_env" = "default.nix",
        "encoder" = "saveRDS",
        "decoder" = "readRDS",
        "env_var" = NULL,
        "noop_build" = FALSE
      ),
      class = "rxp_derivation"
    )
  )
})

test_that("rxp_py: with additional files", {
  d1 <- rxp_py(
    mtcars_pl_am,
    "mtcars_pl.filter(pl.col('am') == 1)",
    additional_files = "data.csv",
    user_functions = "functions.py"
  )

  # Test the entire object
  testthat::expect_equal(
    d1,
    structure(
      list(
        "name" = "mtcars_pl_am",
        "snippet" = '  mtcars_pl_am = makePyDerivation {\n    name = "mtcars_pl_am";\n     src = defaultPkgs.lib.fileset.toSource {\n      root = ./.;\n      fileset = defaultPkgs.lib.fileset.unions [ ./data.csv ./functions.py ];\n    };\n    buildInputs = defaultBuildInputs;\n    configurePhase = defaultConfigurePhase;\n    buildPhase = \'\'\n      cp -r $src/* .\n      python -c "\nexec(open(\'libraries.py\').read())\n# RIXPRESS_PY_LOAD_DEPENDENCIES_HERE:mtcars_pl_am\nexec(open(\'functions.py\').read())\nexec(\'mtcars_pl_am = mtcars_pl.filter(pl.col(\\\'am\\\') == 1)\')\nwith open(\'mtcars_pl_am\', \'wb\') as f: pickle.dump(globals()[\'mtcars_pl_am\'], f)\n"\n    \'\';\n  };',
        "type" = "rxp_py",
        "additional_files" = "data.csv",
        "user_functions" = "functions.py",
        "nix_env" = "default.nix",
        "encoder" = "with open('mtcars_pl_am', 'wb') as f: pickle.dump(globals()['mtcars_pl_am'], f)",
        "decoder" = "pickle.load",
        "env_var" = NULL,
        "noop_build" = FALSE
      ),
      class = "rxp_derivation"
    )
  )
})

test_that("rxp_r: with env_var parameter", {
  d1 <- rxp_r(
    mtcars_am,
    dplyr::filter(mtcars, am == 1),
    env_var = c(MY_VAR = "test_value", ANOTHER_VAR = "123")
  )

  # Test the entire object
  testthat::expect_equal(
    d1,
    structure(
      list(
        "name" = "mtcars_am",
        "snippet" = '  mtcars_am = makeRDerivation {\n    name = "mtcars_am";\n    buildInputs = defaultBuildInputs;\n    configurePhase = defaultConfigurePhase;\n    buildPhase = \'\'\n      export MY_VAR=test_value\n      export ANOTHER_VAR=123\n      Rscript -e "\n        source(\'libraries.R\')\n        # RIXPRESS_LOAD_DEPENDENCIES_HERE:mtcars_am\n        mtcars_am <- dplyr::filter(mtcars, am == 1)\n        saveRDS(mtcars_am, \'mtcars_am\')"\n    \'\';\n  };',
        "type" = "rxp_r",
        "additional_files" = "",
        "user_functions" = "",
        "nix_env" = "default.nix",
        "encoder" = "saveRDS",
        "decoder" = "readRDS",
        "env_var" = c(MY_VAR = "test_value", ANOTHER_VAR = "123"),
        "noop_build" = FALSE
      ),
      class = "rxp_derivation"
    )
  )
})

test_that("rxp_common_setup: handles invalid direction", {
  testthat::expect_error(
    rxp_common_setup(
      "test_name",
      "test_expr",
      "default.nix",
      "invalid_direction"
    ),
    "Invalid direction. Use 'py2r' or 'r2py'."
  )
})

test_that("rxp_py: with env_var parameter", {
  d1 <- rxp_py(
    mtcars_pl_am,
    "mtcars_pl.filter(pl.col('am') == 1)",
    env_var = c(PYTHON_ENV = "production", DEBUG = "0")
  )

  # Test the entire object
  testthat::expect_equal(
    d1,
    structure(
      list(
        "name" = "mtcars_pl_am",
        "snippet" = '  mtcars_pl_am = makePyDerivation {\n    name = "mtcars_pl_am";\n    buildInputs = defaultBuildInputs;\n    configurePhase = defaultConfigurePhase;\n    buildPhase = \'\'\n      export PYTHON_ENV=production\n      export DEBUG=0\n      python -c "\nexec(open(\'libraries.py\').read())\n# RIXPRESS_PY_LOAD_DEPENDENCIES_HERE:mtcars_pl_am\nexec(\'mtcars_pl_am = mtcars_pl.filter(pl.col(\\\'am\\\') == 1)\')\nwith open(\'mtcars_pl_am\', \'wb\') as f: pickle.dump(globals()[\'mtcars_pl_am\'], f)\n"\n    \'\';\n  };',
        "type" = "rxp_py",
        "additional_files" = "",
        "user_functions" = "",
        "nix_env" = "default.nix",
        "encoder" = "with open('mtcars_pl_am', 'wb') as f: pickle.dump(globals()['mtcars_pl_am'], f)",
        "decoder" = "pickle.load",
        "env_var" = c(PYTHON_ENV = "production", DEBUG = "0"),
        "noop_build" = FALSE
      ),
      class = "rxp_derivation"
    )
  )
})

test_that("rxp_qmd: with env_var parameter", {
  # Create a temporary qmd file for testing
  qmd_file <- tempfile(fileext = ".qmd")
  on.exit(unlink(qmd_file))

  writeLines(
    "---\ntitle: Test\n---\n\nThis is a test quarto document with rxp_read(\"test_data\").",
    qmd_file
  )

  d1 <- suppressWarnings(rxp_qmd(
    report,
    qmd_file,
    additional_files = "images",
    args = "--to pdf",
    env_var = c(QUARTO_PROFILE = "production", QUARTO_RENDER_TOKEN = "abc123")
  ))

  testthat::expect_equal(
    d1,
    structure(
      list(
        "name" = "report",
        "snippet" = paste0(
          '  report = defaultPkgs.stdenv.mkDerivation {\n    name = "report";\n    src = defaultPkgs.lib.fileset.toSource {\n      root = ./.;\n      fileset = defaultPkgs.lib.fileset.unions [ ./',
          qmd_file,
          ' ./images ];\n    };\n    buildInputs = defaultBuildInputs;\n    configurePhase = defaultConfigurePhase;\n    buildPhase = \'\'\n      mkdir home\n      export HOME=$PWD/home\n      export RETICULATE_PYTHON=${defaultPkgs.python3}/bin/python\n      export QUARTO_PROFILE=production\n      export QUARTO_RENDER_TOKEN=abc123\n\n      substituteInPlace ',
          qmd_file,
          ' --replace-fail \'rxp_read("test_data")\' \'rxp_read("${test_data}")\'\n      quarto render ',
          qmd_file,
          ' --to pdf --output-dir $out\n    \'\';\n  };'
        ),
        "type" = "rxp_qmd",
        "qmd_file" = qmd_file,
        "additional_files" = "images",
        "nix_env" = "default.nix",
        "args" = "--to pdf",
        "env_var" = c(
          QUARTO_PROFILE = "production",
          QUARTO_RENDER_TOKEN = "abc123"
        ),
        "noop_build" = FALSE
      ),
      class = "rxp_derivation"
    )
  )
})

test_that("rxp_r_file: with env_var parameter", {
  # Create a temporary CSV for testing
  csv_file <- tempfile(fileext = ".csv")
  on.exit(unlink(csv_file))

  write.csv(mtcars[1:5, ], csv_file, row.names = FALSE)

  d1 <- rxp_r_file(
    mtcars_data,
    path = csv_file,
    read_function = read.csv,
    env_var = c(R_DATA_DIR = "/path/to/data", R_DEBUG = "TRUE")
  )

  # Test the entire object
  testthat::expect_equal(
    d1$env_var,
    c(R_DATA_DIR = "/path/to/data", R_DEBUG = "TRUE")
  )
})

test_that("rxp_py_file: with env_var parameter", {
  # Create a temporary CSV for testing
  csv_file <- tempfile(fileext = ".csv")
  on.exit(unlink(csv_file))

  write.csv(mtcars[1:5, ], csv_file, row.names = FALSE)

  d1 <- rxp_py_file(
    mtcars_data,
    path = csv_file,
    read_function = "pandas.read_csv",
    env_var = c(PYTHONPATH = "/custom/modules", PYTHON_DEBUG = "1")
  )

  # Test the entire object
  testthat::expect_equal(
    d1$env_var,
    c(PYTHONPATH = "/custom/modules", PYTHON_DEBUG = "1")
  )
})

test_that("rxp_rmd: with env_var parameter", {
  # Create a temporary Rmd file for testing
  rmd_file <- tempfile(fileext = ".Rmd")
  on.exit(unlink(rmd_file))

  writeLines(
    "---\ntitle: Test\n---\n\nThis is a test R Markdown document with rxp_read(\"test_data\").",
    rmd_file
  )

  d1 <- suppressWarnings(rxp_rmd(
    report,
    rmd_file,
    additional_files = "images",
    env_var = c(
      RSTUDIO_PANDOC = "/usr/local/bin/pandoc",
      R_LIBS_USER = "/custom/r/libs"
    )
  ))

  # Test the entire object
  testthat::expect_equal(
    d1,
    structure(
      list(
        "name" = "report",
        "snippet" = paste0(
          '  report = defaultPkgs.stdenv.mkDerivation {\n    name = "report";\n    src = defaultPkgs.lib.fileset.toSource {\n      root = ./.;\n      fileset = defaultPkgs.lib.fileset.unions [ ./',
          rmd_file,
          ' ./images ];\n    };\n    buildInputs = defaultBuildInputs;\n    configurePhase = defaultConfigurePhase;\n    buildPhase = \'\'\n      mkdir home\n      export HOME=$PWD/home\n      export RETICULATE_PYTHON=${defaultPkgs.python3}/bin/python\n      export RSTUDIO_PANDOC=/usr/local/bin/pandoc\n      export R_LIBS_USER=/custom/r/libs\n\n      substituteInPlace ',
          rmd_file,
          ' --replace-fail \'rxp_read("test_data")\' \'rxp_read("${test_data}")\'\n      Rscript -e "rmd_file <- \'',
          rmd_file,
          '\'; rmarkdown::render(input = file.path(\'$PWD\', rmd_file), output_dir = \'$out\')"\n    \'\';\n  };'
        ),
        "type" = "rxp_rmd",
        "rmd_file" = rmd_file,
        "additional_files" = "images",
        "nix_env" = "default.nix",
        "params" = NULL,
        "env_var" = c(
          RSTUDIO_PANDOC = "/usr/local/bin/pandoc",
          R_LIBS_USER = "/custom/r/libs"
        ),
        "noop_build" = FALSE
      ),
      class = "rxp_derivation"
    )
  )
})
