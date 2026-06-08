test_that("gaussian_copula_synthesizer() creates unfitted synthesizer", {
  syn <- gaussian_copula_synthesizer(small_meta())
  expect_s3_class(syn, "gaussian_copula_synthesizer")
  expect_s3_class(syn, "rsdv_synthesizer")
  expect_false(is_fitted(syn))
})

test_that("fit() returns a fitted synthesizer", {
  syn <- gaussian_copula_synthesizer(small_meta())
  fitted_syn <- fit(syn, small_data())
  expect_true(is_fitted(fitted_syn))
  expect_false(is.null(fitted_syn$transformers))
})

test_that("fit() errors when data is missing required columns", {
  syn <- gaussian_copula_synthesizer(small_meta())
  bad <- small_data()[, c("age", "income")]
  expect_error(fit(syn, bad), "Missing columns")
})

test_that("fit() stores a correlation matrix over all modeled columns", {
  syn <- gaussian_copula_synthesizer(small_meta())
  syn <- fit(syn, small_data())
  # All modeled columns enter the copula: age + income (numerical) + edu (categorical)
  expect_equal(dim(syn$cor_matrix), c(3L, 3L))
  expect_equal(rownames(syn$cor_matrix), c("age", "income", "edu"))
})

test_that("fit() works with only one numerical column", {
  meta <- metadata() |> set_column_type("x", "numerical")
  df   <- data.frame(x = 1:10)
  syn  <- gaussian_copula_synthesizer(meta) |> fit(df)
  expect_true(is_fitted(syn))
  expect_null(syn$copula)  # no copula needed for 1 column
})

test_that("sample() returns a data frame with n rows and correct columns", {
  syn <- gaussian_copula_synthesizer(small_meta()) |> fit(small_data())
  out <- sample(syn, n = 20)
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 20L)
  expect_true(all(c("age", "income", "edu") %in% names(out)))
})

test_that("sample() errors on unfitted synthesizer", {
  syn <- gaussian_copula_synthesizer(small_meta())
  expect_error(sample(syn, n = 10), "must be fitted")
})

test_that("sample() is reproducible with set.seed()", {
  syn <- gaussian_copula_synthesizer(small_meta()) |> fit(small_data())
  set.seed(1); out1 <- sample(syn, n = 10)
  set.seed(1); out2 <- sample(syn, n = 10)
  expect_equal(out1, out2)
})

test_that("sample() with enforce_min_max clamps numerical columns", {
  df  <- small_data()
  syn <- gaussian_copula_synthesizer(small_meta(), enforce_min_max = TRUE) |>
    fit(df)
  out <- sample(syn, n = 200)
  expect_true(all(out$age    >= min(df$age)    & out$age    <= max(df$age)))
  expect_true(all(out$income >= min(df$income) & out$income <= max(df$income)))
})

test_that("sample() with only categorical columns works", {
  meta <- metadata() |>
    set_column_type("color", "categorical") |>
    set_column_type("size", "categorical")
  df   <- data.frame(color = c("red","blue","red"), size = c("S","M","L"),
                     stringsAsFactors = FALSE)
  syn  <- gaussian_copula_synthesizer(meta) |> fit(df)
  out  <- sample(syn, n = 10)
  expect_equal(nrow(out), 10L)
  expect_true(all(out$color %in% c("red","blue")))
})

test_that("sample() respects inequality constraints via rejection sampling", {
  meta <- small_meta() |>
    add_constraint(inequality_constraint("age", "income", type = "lt"))
  syn  <- gaussian_copula_synthesizer(meta) |> fit(small_data())
  out  <- sample(syn, n = 30)
  expect_true(all(out$age < out$income))
})

test_that("rejection loop fires warning and filters rows when constraint nearly impossible", {
  # Two columns with identical range force x ≈ y from the copula;
  # the equality constraint (x == y exactly) almost never holds, triggering
  # the max_tries warning and exercising the accumulation + filter path.
  df   <- data.frame(x = as.numeric(1:10), y = as.numeric(1:10))
  meta <- metadata() |>
    set_column_type("x", "numerical") |>
    set_column_type("y", "numerical") |>
    add_constraint(equality_constraint("x", "y"))
  syn <- gaussian_copula_synthesizer(meta) |> fit(df)
  # With max_tries = 2 and an exact-equality constraint on continuous data,
  # we almost certainly cannot collect 50 valid rows.
  expect_warning(
    out <- sample(syn, n = 50, max_tries = 2L),
    "Could not satisfy"
  )
  # Whatever rows were returned must satisfy the constraint
  if (nrow(out) > 0L) expect_true(all(out$x == out$y))
})

test_that("synthesizer preserves numeric<->categorical dependence", {
  set.seed(123)
  n  <- 800
  # Strong dependence: group "high" has much larger x than group "low".
  grp <- sample(c("low", "high"), n, TRUE)
  x   <- ifelse(grp == "high", rnorm(n, 100, 5), rnorm(n, 10, 5))
  df  <- data.frame(x = x, grp = grp, stringsAsFactors = FALSE)
  meta <- metadata(df)
  syn  <- gaussian_copula_synthesizer(meta) |> fit(df)
  out  <- sample(syn, n = 800)

  real_gap <- mean(df$x[df$grp == "high"])  - mean(df$x[df$grp == "low"])
  syn_gap  <- mean(out$x[out$grp == "high"]) - mean(out$x[out$grp == "low"])
  # The group separation must survive (independent sampling would give ~0 gap).
  expect_gt(syn_gap, 0.5 * real_gap)
})
