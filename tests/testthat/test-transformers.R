test_that("NumericalTransformer transforms to [0,1] and inverts exactly", {
  x  <- c(10, 20, 30, 40, 50)
  tr <- fit_numerical_transformer(x)
  u  <- apply_numerical_transformer(x, tr)
  expect_true(all(u >= 0 & u <= 1))
  x2 <- invert_numerical_transformer(u, tr)
  expect_equal(x2, x, tolerance = 1e-6)
})

test_that("numerical transformer clamps epsilon away from 0 and 1", {
  x  <- c(0, 100)  # min == max endpoints
  tr <- fit_numerical_transformer(x)
  u  <- apply_numerical_transformer(x, tr)
  # Must not produce exactly 0 or 1 (would cause qnorm(±Inf) downstream)
  expect_true(all(u > 0 & u < 1))
})

test_that("numerical transformer handles constant vector", {
  x  <- c(5, 5, 5)
  tr <- fit_numerical_transformer(x)
  u  <- apply_numerical_transformer(x, tr)
  expect_true(all(u == 0.5))
})

test_that("CategoricalTransformer maps levels to integers and back", {
  x  <- c("cat", "dog", "cat", "bird")
  tr <- fit_categorical_transformer(x)
  codes <- apply_categorical_transformer(x, tr)
  expect_true(is.integer(codes))
  x2 <- invert_categorical_transformer(codes, tr)
  expect_equal(x2, x)
})

test_that("CategoricalTransformer samples from empirical distribution", {
  x  <- c(rep("A", 90), rep("B", 10))
  tr <- fit_categorical_transformer(x)
  set.seed(42)
  samples <- sample_categorical(100, tr)
  expect_gt(sum(samples == "A"), 70)
})

test_that("fit_transformers() returns one transformer per registered column", {
  df   <- small_data()
  meta <- small_meta()
  trs  <- fit_transformers(df, meta)
  expect_true(all(c("age", "income", "edu") %in% names(trs)))
})

test_that("fit_numerical_transformer() records miss_rate", {
  tr_clean <- fit_numerical_transformer(c(1, 2, 3, 4, 5))
  expect_equal(tr_clean$miss_rate, 0)

  tr_miss <- fit_numerical_transformer(c(1, NA, 3, NA, 5))
  expect_equal(tr_miss$miss_rate, 0.4)
})

test_that("fit_categorical_transformer() records miss_rate", {
  tr_clean <- fit_categorical_transformer(c("a", "b", "a"))
  expect_equal(tr_clean$miss_rate, 0)

  tr_miss <- fit_categorical_transformer(c("a", NA, "b", NA, "a"))
  expect_equal(tr_miss$miss_rate, 0.4)
})

test_that("fit_boolean_transformer() records miss_rate", {
  tr_miss <- fit_boolean_transformer(c(TRUE, NA, FALSE, TRUE, NA))
  expect_equal(tr_miss$miss_rate, 0.4)
})

test_that("sample() reproduces missingness rates from training data", {
  df <- data.frame(
    x = c(1, NA, 3, NA, 5, 6, 7, 8, 9, 10),   # 20% NA
    y = c("a", "b", NA, "a", "b", "a", "b", "a", NA, "b"),  # 20% NA
    stringsAsFactors = FALSE
  )
  meta <- metadata(df) |>
    set_column_type("x", "numerical") |>
    set_column_type("y", "categorical")
  set.seed(42)
  syn <- gaussian_copula_synthesizer(meta) |> fit(df)
  out <- sample(syn, n = 2000)
  # Observed rates should be within 4pp of the 20% target
  expect_lt(abs(mean(is.na(out$x)) - 0.2), 0.04)
  expect_lt(abs(mean(is.na(out$y)) - 0.2), 0.04)
})

test_that("sample() produces no NAs when training data had none", {
  df   <- small_data()
  meta <- small_meta()
  set.seed(1)
  out  <- gaussian_copula_synthesizer(meta) |> fit(df) |> sample(n = 100)
  expect_equal(sum(is.na(out)), 0L)
})

test_that("apply_transformers() + invert_transformers() round-trips numerical cols", {
  df   <- small_data()
  meta <- small_meta()
  trs  <- fit_transformers(df, meta)
  u    <- apply_transformers(df, trs, meta)
  df2  <- invert_transformers(u, trs, meta)
  expect_equal(df2$age,    df$age,    tolerance = 1e-6)
  expect_equal(df2$income, df$income, tolerance = 1e-6)
})

# Parametric marginals -------------------------------------------------------

test_that("fit_numerical_transformer() selects a supported family", {
  set.seed(1)
  tr <- fit_numerical_transformer(rnorm(500))
  expect_true(tr$dist %in% c("norm", "beta", "gamma", "truncnorm", "uniform"))
})

test_that("fit_numerical_transformer() honors an explicit distribution", {
  set.seed(1)
  tr <- fit_numerical_transformer(rnorm(200), distribution = "norm")
  expect_equal(tr$dist, "norm")
})

test_that("explicit infeasible distribution warns and falls back to uniform", {
  # gamma requires strictly positive support; data spans zero.
  expect_warning(
    tr <- fit_numerical_transformer(c(-3, -1, 0, 2, 4), distribution = "gamma"),
    "could not be fit"
  )
  expect_equal(tr$dist, "uniform")
})

test_that("apply/invert numerical round-trips interior values", {
  set.seed(2)
  x  <- rnorm(100, 10, 3)
  tr <- fit_numerical_transformer(x, distribution = "norm")
  u  <- apply_numerical_transformer(x, tr)
  expect_true(all(u > 0 & u < 1))
  expect_equal(invert_numerical_transformer(u, tr), x, tolerance = 1e-6)
})

test_that("constant numerical column maps to 0.5 and a constant fit", {
  tr <- fit_numerical_transformer(c(5, 5, 5))
  expect_equal(tr$dist, "constant")
  expect_true(all(apply_numerical_transformer(c(5, 5, 5), tr) == 0.5))
})

test_that("categorical encode/decode recovers the dominant level", {
  tr <- fit_categorical_transformer(c(rep("A", 80), rep("B", 20)))
  # Values drawn from the largest interval decode to the largest category.
  expect_equal(decode_categorical_u(0.5, tr), "A")
  expect_equal(decode_categorical_u(0.95, tr), "B")
  u <- encode_categorical_u(rep("A", 100), tr)
  expect_true(all(u >= 0 & u <= tr$breaks[2]))
})
