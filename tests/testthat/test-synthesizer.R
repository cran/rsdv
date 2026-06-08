test_that("fit() is re-exported from generics", {
  # fit should be exported from the rsdv namespace (accessible as rsdv::fit)
  expect_true("fit" %in% getNamespaceExports("rsdv"))
  # and must actually be callable
  expect_true(is.function(rsdv::fit))
})

test_that("sample() dispatches to base::sample for plain vectors", {
  result <- rsdv::sample(1:10, 3)
  expect_length(result, 3L)
  expect_true(all(result %in% 1:10))
})

test_that("validate_data() passes when data matches metadata", {
  expect_no_error(validate_data(small_data(), small_meta()))
})

test_that("validate_data() errors when a required column is missing", {
  bad <- small_data()[, c("age", "income")]
  expect_error(validate_data(bad, small_meta()), "Missing columns")
})

test_that("is_fitted() returns FALSE for unfitted synthesizer stub", {
  stub <- structure(list(fitted = FALSE), class = "rsdv_synthesizer")
  expect_false(is_fitted(stub))
})

test_that("is_fitted() returns TRUE when fitted = TRUE", {
  stub <- structure(list(fitted = TRUE), class = "rsdv_synthesizer")
  expect_true(is_fitted(stub))
})
