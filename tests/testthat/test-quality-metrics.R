# Task 11: KS and TVD

test_that("ks_similarity() returns a tibble with column/score for numerical cols", {
  real <- small_data()
  syn  <- small_data()  # identical → score should be near 1
  out  <- ks_similarity(real, syn, small_meta())
  expect_s3_class(out, "data.frame")
  expect_true(all(c("column", "score") %in% names(out)))
  expect_true(all(out$score >= 0 & out$score <= 1))
  expect_true(all(out$score > 0.9))
})

test_that("ks_similarity() scores degraded distribution lower", {
  set.seed(1)
  real <- data.frame(x = rnorm(200))
  syn  <- data.frame(x = rnorm(200, mean = 5))
  meta <- metadata() |> set_column_type("x", "numerical")
  out  <- ks_similarity(real, syn, meta)
  expect_lt(out$score[out$column == "x"], 0.5)
})

test_that("tvd_similarity() returns a tibble with column/score for categorical cols", {
  real <- small_data()
  syn  <- small_data()
  out  <- tvd_similarity(real, syn, small_meta())
  expect_true(all(c("column", "score") %in% names(out)))
  expect_true(all(out$score >= 0 & out$score <= 1))
  expect_true(all(out$score > 0.9))
})

test_that("tvd_similarity() scores degraded distribution lower", {
  real <- data.frame(cat = rep(c("A", "B"), c(90, 10)), stringsAsFactors = FALSE)
  syn  <- data.frame(cat = rep(c("A", "B"), c(10, 90)), stringsAsFactors = FALSE)
  meta <- metadata() |> set_column_type("cat", "categorical")
  out  <- tvd_similarity(real, syn, meta)
  expect_lt(out$score[out$column == "cat"], 0.5)
})

# Task 12: Correlation similarity and ML efficacy

test_that("correlation_similarity() returns per-pair scores and a mean in [0,1]", {
  set.seed(42)
  real <- data.frame(x = rnorm(100), y = rnorm(100))
  syn  <- data.frame(x = rnorm(100), y = rnorm(100))
  meta <- metadata() |>
    set_column_type("x", "numerical") |>
    set_column_type("y", "numerical")
  res <- correlation_similarity(real, syn, meta)
  expect_true(all(c("pairs", "score") %in% names(res)))
  expect_true(all(c("column_1", "column_2", "score") %in% names(res$pairs)))
  expect_equal(nrow(res$pairs), 1L)
  expect_true(res$score >= 0 && res$score <= 1)
})

test_that("correlation_similarity() scores 1 for identical data", {
  real <- data.frame(x = 1:20, y = 1:20)
  meta <- metadata() |>
    set_column_type("x", "numerical") |>
    set_column_type("y", "numerical")
  expect_equal(correlation_similarity(real, real, meta)$score, 1, tolerance = 1e-9)
})

test_that("contingency_similarity() scores 1 for identical data, lower when degraded", {
  set.seed(7)
  real <- data.frame(
    a = sample(c("p", "q"), 200, TRUE),
    b = sample(c("x", "y"), 200, TRUE),
    stringsAsFactors = FALSE
  )
  meta <- metadata() |>
    set_column_type("a", "categorical") |>
    set_column_type("b", "categorical")
  expect_equal(contingency_similarity(real, real, meta)$score, 1, tolerance = 1e-9)

  # Swap one column to destroy the joint distribution.
  bad <- data.frame(a = rev(real$a), b = real$b, stringsAsFactors = FALSE)
  bad$a <- ifelse(real$a == "p", "q", "p")
  expect_lt(contingency_similarity(real, bad, meta)$score, 1)
})

test_that("contingency_similarity() returns 1 with fewer than two categorical cols", {
  meta <- metadata() |> set_column_type("a", "categorical")
  df   <- data.frame(a = c("x", "y", "x"), stringsAsFactors = FALSE)
  expect_equal(contingency_similarity(df, df, meta)$score, 1)
})

test_that("ml_efficacy() returns list with tstr, trtr, score in [0,1]", {
  set.seed(42)
  n    <- 200
  real <- data.frame(
    x      = rnorm(n),
    y      = rnorm(n),
    target = sample(c("A", "B"), n, replace = TRUE),
    stringsAsFactors = FALSE
  )
  meta <- metadata() |>
    set_column_type("x", "numerical") |>
    set_column_type("y", "numerical") |>
    set_column_type("target", "categorical")
  res  <- ml_efficacy(real, real, meta, target_col = "target")
  expect_true(all(c("tstr", "trtr", "score") %in% names(res)))
  expect_true(res$score >= 0 && res$score <= 1)
})

test_that("ks_similarity() does not emit ks.test ties warning to the user", {
  # Tied integer data triggers ks.test's "p-value will be approximate in the
  # presence of ties" warning. The metric only uses the D statistic, so the
  # warning is noise — confirm it does not leak through to the caller.
  set.seed(7)
  real <- data.frame(x = sample(1:5, 80, replace = TRUE))
  syn  <- data.frame(x = sample(1:5, 80, replace = TRUE))
  meta <- metadata() |> set_column_type("x", "numerical")
  expect_no_warning(ks_similarity(real, syn, meta))
})
