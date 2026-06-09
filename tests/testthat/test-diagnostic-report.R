test_that("diagnostic_report() scores valid synthetic data near 1", {
  set.seed(1)
  real <- data.frame(
    x   = rnorm(200, 10, 2),
    grp = sample(c("a", "b", "c"), 200, TRUE),
    stringsAsFactors = FALSE
  )
  meta <- metadata(real)
  syn  <- gaussian_copula_synthesizer(meta) |> fit(real) |> sample(n = 200)
  dr   <- diagnostic_report(real, syn, meta)
  expect_s3_class(dr, "rsdv_diagnostic_report")
  # enforce_min_max keeps numerics in range; decode keeps categories valid.
  expect_equal(dr$overall_score, 1, tolerance = 1e-9)
})

test_that("diagnostic_report() flags out-of-range numerical values", {
  meta <- metadata() |> set_column_type("x", "numerical")
  real <- data.frame(x = c(0, 1, 2, 3, 4, 5))
  syn  <- data.frame(x = c(0, 1, 2, 99, 100, 3))  # two values out of range
  dr   <- diagnostic_report(real, syn, meta)
  expect_lt(dr$validity_score, 1)
  expect_equal(dr$validity$score[dr$validity$check == "boundary adherence"],
               4 / 6, tolerance = 1e-9)
})

test_that("diagnostic_report() flags unseen categories", {
  meta <- metadata() |> set_column_type("g", "categorical")
  real <- data.frame(g = c("a", "b", "a", "b"), stringsAsFactors = FALSE)
  syn  <- data.frame(g = c("a", "b", "z", "a"), stringsAsFactors = FALSE)  # "z" unseen
  dr   <- diagnostic_report(real, syn, meta)
  expect_equal(dr$validity$score[dr$validity$check == "category adherence"],
               3 / 4, tolerance = 1e-9)
})

test_that("diagnostic_report() checks primary key uniqueness", {
  meta <- metadata() |>
    set_column_type("id", "id") |>
    set_primary_key("id")
  real <- data.frame(id = 1:5)
  uniq <- data.frame(id = 11:15)
  dup  <- data.frame(id = c(1, 1, 2, 3, 4))
  expect_equal(
    diagnostic_report(real, uniq, meta)$validity$score[
      diagnostic_report(real, uniq, meta)$validity$check == "key uniqueness"], 1)
  expect_equal(
    diagnostic_report(real, dup, meta)$validity$score[
      diagnostic_report(real, dup, meta)$validity$check == "key uniqueness"], 0)
})

test_that("diagnostic_report() returns NA (not 1) for an all-NA synthetic column", {
  meta <- metadata() |> set_column_type("x", "numerical")
  real <- data.frame(x = 1:10)
  syn  <- data.frame(x = as.numeric(rep(NA, 10)))
  dr   <- diagnostic_report(real, syn, meta)
  expect_true(is.na(
    dr$validity$score[dr$validity$check == "boundary adherence"]
  ))
})

test_that("diagnostic_report() validity_score skips NA per-column scores", {
  meta <- metadata() |>
    set_column_type("x", "numerical") |>
    set_column_type("y", "numerical")
  real <- data.frame(x = 1:10, y = 1:10)
  # x synthetic: in-range. y synthetic: all NA -> NA score, should be excluded
  # from the validity_score average, not pull it down.
  syn <- data.frame(x = 1:10, y = as.numeric(rep(NA, 10)))
  dr  <- diagnostic_report(real, syn, meta)
  expect_equal(dr$validity_score, 1, tolerance = 1e-9)
})
