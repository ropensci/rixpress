test_that("rxp_py2r sets RETICULATE_AUTOCONFIGURE=0 by default", {
  # Mock user input
  nix_env <- "default.nix"

  # Default behavior
  res <- rxp_py2r(
    name = my_obj,
    expr = py_obj,
    nix_env = nix_env
  )

  expect_true(!is.null(res$env_var))
  expect_equal(res$env_var[["RETICULATE_AUTOCONFIGURE"]], "0")

  # Verify it appears in the snippet (indirectly validating build_reticulate_phase logic if it was simple string matching)
  # But better to check the structured 'env_var' component we added.
})

test_that("rxp_r2py sets RETICULATE_AUTOCONFIGURE=0 by default", {
  nix_env <- "default.nix"

  res <- rxp_r2py(
    name = py_obj,
    expr = r_obj,
    nix_env = nix_env
  )

  expect_true(!is.null(res$env_var))
  expect_equal(res$env_var[["RETICULATE_AUTOCONFIGURE"]], "0")
})

test_that("User can override RETICULATE_AUTOCONFIGURE", {
  nix_env <- "default.nix"

  # Override to "1"
  res_override <- rxp_py2r(
    name = my_obj,
    expr = py_obj,
    nix_env = nix_env,
    env_var = c(RETICULATE_AUTOCONFIGURE = "1")
  )

  expect_equal(res_override$env_var[["RETICULATE_AUTOCONFIGURE"]], "1")

  # Override to empty string/something else
  res_override2 <- rxp_r2py(
    name = py_obj,
    expr = r_obj,
    nix_env = nix_env,
    env_var = c(RETICULATE_AUTOCONFIGURE = "custom")
  )
  expect_equal(res_override2$env_var[["RETICULATE_AUTOCONFIGURE"]], "custom")
})

test_that("env_var merging works correctly with other variables", {
  nix_env <- "default.nix"

  res <- rxp_py2r(
    name = my_obj,
    expr = py_obj,
    nix_env = nix_env,
    env_var = c(OTHER_VAR = "foo")
  )

  expect_equal(res$env_var[["RETICULATE_AUTOCONFIGURE"]], "0")
  expect_equal(res$env_var[["OTHER_VAR"]], "foo")
})
