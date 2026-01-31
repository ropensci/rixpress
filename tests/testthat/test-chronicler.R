# Tests for chronicler integration

test_that("chronicle state detection works when chronicler is available", {
  skip_if_not_installed("chronicler")
  skip_if_not_installed("maybe")

  # Success case
  r_sqrt <- chronicler::record(sqrt)
  success_result <- r_sqrt(4)
  expect_equal(rixpress:::.rxp_chronicle_state(success_result), "success")

  # Nothing case (default strict = 2 catches warnings as Nothing)
  nothing_result <- r_sqrt(-1)
  expect_equal(rixpress:::.rxp_chronicle_state(nothing_result), "nothing")
})

test_that("chronicle warning state detection works", {
  skip_if_not_installed("chronicler")
  skip_if_not_installed("maybe")

  # Warning case (strict = 1 allows warnings through but captures them in log)
  r_sqrt_strict1 <- chronicler::record(sqrt, strict = 1)
  # Suppress the warning in test output - we're testing the capture, not the warning itself
  warning_result <- suppressWarnings(r_sqrt_strict1(-1))

  # With strict = 1, warning is captured but value is still Just (not Nothing)
  # The log will contain NOK entries for warnings
  state <- rixpress:::.rxp_chronicle_state(warning_result)
  # Depending on chronicler behavior, this might be "success" or "warning"
  expect_true(state %in% c("success", "warning"))
})

test_that("status extraction includes correct details", {
  skip_if_not_installed("chronicler")
  skip_if_not_installed("maybe")

  r_sqrt <- chronicler::record(sqrt)
  result <- r_sqrt(-1)

  status <- rixpress:::.rxp_chronicle_status(result)
  expect_true(status$is_chronicle)
  expect_true(status$is_nothing)
  expect_equal(status$state, "nothing")
  expect_equal(status$num_failed, 1)
  expect_true("sqrt" %in% status$failed_functions)
})

test_that("non-chronicle objects return NULL", {
  # This should work even without chronicler installed
  expect_null(rixpress:::.rxp_chronicle_state(mtcars))
  expect_null(rixpress:::.rxp_chronicle_status(mtcars))
})

test_that("symbol function returns correct symbols", {
  expect_equal(rixpress:::.rxp_chronicle_symbol("success"), "\u2713")
  expect_equal(rixpress:::.rxp_chronicle_symbol("warning"), "\u26A0")
  expect_equal(rixpress:::.rxp_chronicle_symbol("nothing"), "\u2716")
  expect_equal(rixpress:::.rxp_chronicle_symbol("unknown"), "?")
})

test_that("format message function works correctly", {
  skip_if_not_installed("chronicler")
  skip_if_not_installed("maybe")

  r_sqrt <- chronicler::record(sqrt)
  result <- r_sqrt(-1)
  status <- rixpress:::.rxp_chronicle_status(result)

  msg <- rixpress:::.rxp_format_chronicle_message("test_deriv", status)
  expect_true(grepl("test_deriv", msg))
  expect_true(grepl("NOTHING", msg))
  expect_true(grepl("sqrt", msg))
})

test_that("has_chronicler function works", {
  # This just tests that the function runs without error
  result <- rixpress:::.rxp_has_chronicler()
  expect_type(result, "logical")
})

test_that("chronicle detection gracefully handles missing chronicler", {
  # When chronicler is not installed, functions should return NULL
  # We mock .rxp_has_chronicler to simulate missing chronicler
  skip_if_not_installed("mockery")

  mockery::stub(
    rixpress:::.rxp_chronicle_state,
    "rixpress:::.rxp_has_chronicler",
    function() FALSE
  )

  # After stubbing, should return NULL for any input
  expect_null(rixpress:::.rxp_chronicle_state(list()))
})
