test_that(".rxp_prepare_nix_store_args works with verbose = 0", {
  args <- .rxp_prepare_nix_store_args(
    max_jobs = 1,
    cores = 1,
    drv_paths = c("/nix/store/test.drv"),
    verbose = 0
  )

  # Should not contain --verbose flag
  expect_false("--verbose" %in% args)

  # Should contain other expected args
  expect_true("--realise" %in% args)
  expect_true("--keep-going" %in% args)
  expect_true("--max-jobs" %in% args)
  expect_true("--cores" %in% args)
})

test_that(".rxp_prepare_nix_store_args works with verbose = 1", {
  args <- .rxp_prepare_nix_store_args(
    max_jobs = 2,
    cores = 3,
    drv_paths = c("/nix/store/test.drv"),
    verbose = 1
  )

  # Should contain exactly one --verbose flag
  verbose_count <- sum(args == "--verbose")
  expect_equal(verbose_count, 1)

  # Should contain other expected args
  expect_true("--realise" %in% args)
  expect_true("--keep-going" %in% args)
  expect_equal(args[which(args == "--max-jobs") + 1], "2")
  expect_equal(args[which(args == "--cores") + 1], "3")
})

test_that(".rxp_prepare_nix_store_args works with verbose = 2", {
  args <- .rxp_prepare_nix_store_args(
    max_jobs = 1,
    cores = 1,
    drv_paths = c("/nix/store/test.drv"),
    verbose = 2
  )

  # Should contain exactly two --verbose flags
  verbose_count <- sum(args == "--verbose")
  expect_equal(verbose_count, 2)
})

test_that(".rxp_prepare_nix_store_args works with verbose = 3", {
  args <- .rxp_prepare_nix_store_args(
    max_jobs = 1,
    cores = 1,
    drv_paths = c("/nix/store/test.drv"),
    verbose = 3
  )

  # Should contain exactly three --verbose flags
  verbose_count <- sum(args == "--verbose")
  expect_equal(verbose_count, 3)
})

test_that("verbose parameter validation works", {
  # Test that negative verbose triggers an error
  expect_error(
    .rxp_validate_verbose(-1),
    "verbose must be a non-negative integer"
  )

  # Test that non-integer triggers an error
  expect_error(
    .rxp_validate_verbose("invalid"),
    "verbose must be a single numeric or integer value"
  )

  # Test that multiple values trigger an error
  expect_error(
    .rxp_validate_verbose(c(1, 2)),
    "verbose must be a single numeric or integer value"
  )

  # Test that valid integers work
  expect_equal(.rxp_validate_verbose(0), 0L)
  expect_equal(.rxp_validate_verbose(1), 1L)
  expect_equal(.rxp_validate_verbose(2L), 2L)
  expect_equal(.rxp_validate_verbose(3.0), 3L)
})
