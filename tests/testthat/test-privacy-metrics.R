test_that("nndr() returns a score between 0 and 1", {
  set.seed(42)
  real <- data.frame(x = rnorm(50), y = rnorm(50))
  syn  <- data.frame(x = rnorm(50), y = rnorm(50))
  score <- nndr(real, syn)
  expect_length(score, 1L)
  expect_true(score >= 0 && score <= 1)
})

test_that("nndr() scores copies of real data low (privacy risk)", {
  real <- data.frame(x = rnorm(50), y = rnorm(50))
  syn  <- real  # exact copy — all NNDR near 0
  score <- nndr(real, syn)
  expect_lt(score, 0.1)
})

test_that("nndr() scores distant synthetic data high (low risk)", {
  real <- data.frame(x = rnorm(50),          y = rnorm(50))
  syn  <- data.frame(x = rnorm(50) + 1000,   y = rnorm(50) + 1000)
  score <- nndr(real, syn)
  expect_gt(score, 0.9)
})

test_that("attribute_disclosure_risk() returns a score in [0, 1]", {
  set.seed(1)
  n    <- 100
  real <- data.frame(
    age    = sample(20:60, n, replace = TRUE),
    income = sample(c("low", "high"), n, replace = TRUE),
    stringsAsFactors = FALSE
  )
  syn <- real[sample(n), ]  # shuffled rows
  score <- attribute_disclosure_risk(real, syn,
                                     sensitive_col = "income",
                                     known_cols    = "age")
  expect_true(score >= 0 && score <= 1)
})

test_that("attribute_disclosure_risk() is high for exact-copy synthetic data", {
  set.seed(2)
  n    <- 50
  real <- data.frame(
    age    = sample(20:60, n, replace = TRUE),
    income = sample(c("low", "high"), n, replace = TRUE),
    stringsAsFactors = FALSE
  )
  syn  <- real  # exact copy
  score <- attribute_disclosure_risk(real, syn,
                                     sensitive_col = "income",
                                     known_cols    = "age")
  expect_gt(score, 0.5)
})
