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

test_that("contingency_similarity() returns NA with fewer than two categorical cols", {
  # With only one categorical column there is no categorical pair to score;
  # we propagate NA so the aggregated quality report does not record an
  # imaginary "perfect 1" score where there is no signal.
  meta <- metadata() |> set_column_type("a", "categorical")
  df   <- data.frame(a = c("x", "y", "x"), stringsAsFactors = FALSE)
  expect_true(is.na(contingency_similarity(df, df, meta)$score))
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

test_that("ml_efficacy() is reproducible across calls with a fixed seed", {
  set.seed(11)
  df <- data.frame(
    x   = rnorm(150),
    y   = rnorm(150),
    tgt = sample(c("a", "b"), 150, TRUE),
    stringsAsFactors = FALSE
  )
  meta <- metadata(df)
  syn  <- gaussian_copula_synthesizer(meta) |> fit(df) |> sample(n = 150)
  s1 <- ml_efficacy(df, syn, meta, "tgt", seed = 42)$score
  s2 <- ml_efficacy(df, syn, meta, "tgt", seed = 42)$score
  expect_equal(s1, s2)
})

test_that("ml_efficacy() does not perturb the caller's RNG stream", {
  set.seed(5)
  df <- data.frame(
    x   = rnorm(80),
    tgt = sample(c("a", "b"), 80, TRUE),
    stringsAsFactors = FALSE
  )
  meta <- metadata(df)
  syn  <- gaussian_copula_synthesizer(meta) |> fit(df) |> sample(n = 80)

  set.seed(99); before <- stats::runif(1)
  set.seed(99); ml_efficacy(df, syn, meta, "tgt", seed = 1)
  after  <- stats::runif(1)
  expect_equal(before, after)
})

test_that("ml_efficacy() errors on a missing target_col with a clear message", {
  meta <- metadata() |>
    set_column_type("x", "numerical") |>
    set_column_type("tgt", "categorical")
  df   <- data.frame(x = 1:10, tgt = rep(c("a", "b"), 5), stringsAsFactors = FALSE)
  expect_error(ml_efficacy(df, df, meta, target_col = "nope"),
               "target_col 'nope' not found")
})

test_that("ml_efficacy() rejects out-of-range test_fraction", {
  meta <- metadata() |>
    set_column_type("x", "numerical") |>
    set_column_type("tgt", "categorical")
  df   <- data.frame(x = 1:20, tgt = rep(c("a", "b"), 10), stringsAsFactors = FALSE)
  expect_error(ml_efficacy(df, df, meta, "tgt", test_fraction = 0))
  expect_error(ml_efficacy(df, df, meta, "tgt", test_fraction = 1))
  expect_error(ml_efficacy(df, df, meta, "tgt", test_fraction = 1.5))
})

test_that("correlation_similarity() score is NA when fewer than 2 numerical cols", {
  meta <- metadata() |>
    set_column_type("x", "numerical") |>
    set_column_type("g", "categorical")
  df   <- data.frame(x = rnorm(20), g = sample(c("a","b"), 20, TRUE),
                     stringsAsFactors = FALSE)
  res <- correlation_similarity(df, df, meta)
  expect_true(is.na(res$score))
  expect_equal(nrow(res$pairs), 0L)
})

test_that("contingency_similarity() score is NA when fewer than 2 categorical cols", {
  meta <- metadata() |>
    set_column_type("a", "categorical") |>
    set_column_type("x", "numerical")
  df   <- data.frame(a = sample(c("u","v"), 20, TRUE), x = rnorm(20),
                     stringsAsFactors = FALSE)
  res <- contingency_similarity(df, df, meta)
  expect_true(is.na(res$score))
})

# --- issue #12 follow-ups -------------------------------------------------

test_that("tvd_similarity() denominator excludes NAs from both sides", {
  # 90% A / 10% B real with 50% NAs vs the same A/B mix with no NAs:
  # the score should reflect the A/B mix (identical), not be pulled toward 0
  # by the NA dilution that the old denominator imposed.
  meta <- metadata() |> set_column_type("cat", "categorical")
  real <- data.frame(cat = c(rep("A", 90), rep("B", 10),
                             rep(NA_character_, 100)),
                     stringsAsFactors = FALSE)
  syn  <- data.frame(cat = c(rep("A", 90), rep("B", 10)),
                     stringsAsFactors = FALSE)
  out <- tvd_similarity(real, syn, meta)
  expect_gt(out$score[out$column == "cat"], 0.99)
})

test_that("tvd_similarity() returns NA when one side is entirely NA", {
  meta <- metadata() |> set_column_type("cat", "categorical")
  real <- data.frame(cat = c("A", "B", "A"), stringsAsFactors = FALSE)
  syn  <- data.frame(cat = rep(NA_character_, 3), stringsAsFactors = FALSE)
  out  <- tvd_similarity(real, syn, meta)
  expect_true(is.na(out$score[out$column == "cat"]))
})

test_that("ml_efficacy() handles factor columns with extra real-side levels", {
  # tgt has 3 levels in real (a, b, c); synthetic happens to lack 'c'.
  # Without expanding factor levels in .set_levels, predict() on test_real
  # raised "factor has new levels: c".
  set.seed(13)
  n <- 90
  real <- data.frame(
    x   = rnorm(n),
    tgt = factor(sample(c("a", "b", "c"), n, TRUE, prob = c(0.45, 0.45, 0.1)),
                 levels = c("a", "b", "c")),
    stringsAsFactors = FALSE
  )
  synthetic <- data.frame(
    x   = rnorm(n),
    tgt = factor(sample(c("a", "b"), n, TRUE), levels = c("a", "b", "c")),
    stringsAsFactors = FALSE
  )
  meta <- metadata() |>
    set_column_type("x",   "numerical") |>
    set_column_type("tgt", "categorical")
  res <- ml_efficacy(real, synthetic, meta, "tgt", seed = 1)
  expect_true(is.finite(res$score) && res$score >= 0 && res$score <= 1)
})
