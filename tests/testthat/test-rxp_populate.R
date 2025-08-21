test_that("rxp_populate basic functionality", {
  d1 <- rxp_r(mtcars_am, dplyr::filter(mtcars, am == 1))
  d2 <- rxp_r(mtcars_head, head(mtcars_am))
  derivs <- list(d1, d2)
  
  # Create temporary directory for test
  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  
  # Test that rxp_populate creates pipeline.nix without building
  expect_silent(rxp_populate(derivs, project_path = temp_dir))
  expect_true(file.exists(file.path(temp_dir, "pipeline.nix")))
})

test_that("rxp_populate with py_imports", {
  skip_if_not_installed("rix")
  
  # Create temporary directory
  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  
  setwd(temp_dir)
  
  # Create a basic nix environment
  rix::rix(
    date = "2025-04-11",
    r_pkgs = "dplyr",
    py_pkgs = c("pandas", "pillow"),
    project_path = ".",
    overwrite = TRUE
  )
  
  # Create Python derivations
  d1 <- rxp_py(data_proc, "df = pandas.DataFrame({'a': [1, 2, 3]})")
  d2 <- rxp_py(img_proc, "from PIL import Image; img = Image.new('RGB', (100, 100))")
  derivs <- list(d1, d2)
  
  # Test py_imports parameter
  py_imports <- list(
    "import pandas" = "import pandas as pd",
    "import pillow" = "from PIL import Image"
  )
  
  expect_silent(rxp_populate(derivs, project_path = temp_dir, py_imports = py_imports))
  expect_true(file.exists(file.path(temp_dir, "pipeline.nix")))
  
  # Check that _rixpress directory was created with library files
  expect_true(dir.exists(file.path(temp_dir, "_rixpress")))
})

test_that("rxp_populate validates py_imports parameter", {
  d1 <- rxp_r(test_data, mtcars)
  derivs <- list(d1)
  
  # Test invalid py_imports (not a named list)
  expect_error(
    rxp_populate(derivs, py_imports = c("import pandas", "import numpy")),
    "py_imports must be a named list"
  )
  
  # Test invalid py_imports (unnamed list)
  expect_error(
    rxp_populate(derivs, py_imports = list("import pandas", "import numpy")),
    "py_imports must be a named list"
  )
})