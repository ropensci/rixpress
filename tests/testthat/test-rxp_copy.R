test_that("rxp_copy: test for failure", {
    expect_error(
      rxp_copy("non_existent_derivation"),
      "No derivation non_existent_derivation found in the Nix store"
    )
}
