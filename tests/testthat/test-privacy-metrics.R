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

test_that("attribute_disclosure_risk() errors clearly on categorical known_cols", {
  real <- data.frame(age = sample(20:60, 50, replace = TRUE),
                     region = sample(c("N","S"), 50, replace = TRUE),
                     income = sample(c("low","high"), 50, replace = TRUE),
                     stringsAsFactors = FALSE)
  syn  <- real[sample.int(50), ]
  expect_error(
    attribute_disclosure_risk(real, syn,
                              sensitive_col = "income",
                              known_cols    = c("age", "region")),
    "must be numeric"
  )
})

test_that("attribute_disclosure_risk() errors when columns are missing", {
  real <- data.frame(age = 1:5, income = c("low","high","low","high","low"),
                     stringsAsFactors = FALSE)
  syn  <- real
  expect_error(
    attribute_disclosure_risk(real, syn, "income", known_cols = "missing"),
    "not found in `real`"
  )
})

test_that("attribute_disclosure_risk() still runs on purely numeric known_cols", {
  real <- data.frame(age = sample(20:60, 50, replace = TRUE),
                     income = sample(c("low","high"), 50, replace = TRUE),
                     stringsAsFactors = FALSE)
  syn  <- real[sample.int(50), ]
  score <- attribute_disclosure_risk(real, syn, "income", known_cols = "age")
  expect_true(is.finite(score) && score >= 0 && score <= 1)
})

test_that("nndr() is invariant to per-column rescaling under the default normalize=TRUE", {
  set.seed(101)
  real <- data.frame(age = rnorm(150, 40, 10),
                     income = rnorm(150, 50000, 12000))
  syn  <- data.frame(age = rnorm(150, 40, 10),
                     income = rnorm(150, 50000, 12000))
  # Rescaling one column should not move the score when normalize = TRUE.
  s_default <- nndr(real, syn)
  real2 <- real; real2$income <- real$income / 1000
  syn2  <- syn;  syn2$income  <- syn$income  / 1000
  s_rescaled <- nndr(real2, syn2)
  expect_equal(s_default, s_rescaled, tolerance = 0.02)
})

test_that("nndr(normalize = FALSE) reproduces the previous scale-sensitive behaviour", {
  set.seed(101)
  real <- data.frame(age = rnorm(150, 40, 10),
                     income = rnorm(150, 50000, 12000))
  syn  <- data.frame(age = rnorm(150, 40, 10),
                     income = rnorm(150, 50000, 12000))
  s_raw <- nndr(real, syn, normalize = FALSE)
  expect_true(is.finite(s_raw))
  # Raw-scale score should differ meaningfully from the normalised one — that
  # is the entire motivation for the new default.
  expect_gt(abs(s_raw - nndr(real, syn)), 0.1)
})

test_that("nndr() drops a constant real column under normalize=TRUE without error", {
  real <- data.frame(x = rnorm(50), const = rep(5, 50))
  syn  <- data.frame(x = rnorm(50), const = rep(5, 50))
  expect_silent(nndr(real, syn))
})
