test_that("sample_conditions() returns rows matching categorical conditions", {
  set.seed(1)
  df <- data.frame(
    age    = rnorm(300, 40, 10),
    income = rnorm(300, 50000, 12000),
    edu    = sample(c("HS", "College", "Grad"), 300, TRUE),
    stringsAsFactors = FALSE
  )
  meta <- metadata(df)
  syn  <- gaussian_copula_synthesizer(meta) |> fit(df)
  out  <- sample_conditions(syn, data.frame(edu = "Grad", .n = 25,
                                            stringsAsFactors = FALSE))
  expect_equal(nrow(out), 25L)
  expect_true(all(out$edu == "Grad"))
})

test_that("sample_conditions() handles multiple condition rows", {
  set.seed(2)
  df <- data.frame(
    x   = rnorm(200),
    grp = sample(c("a", "b"), 200, TRUE),
    stringsAsFactors = FALSE
  )
  meta <- metadata(df)
  syn  <- gaussian_copula_synthesizer(meta) |> fit(df)
  out  <- sample_conditions(syn, data.frame(
    grp = c("a", "b"), .n = c(10L, 5L), stringsAsFactors = FALSE))
  expect_equal(sum(out$grp == "a"), 10L)
  expect_equal(sum(out$grp == "b"), 5L)
})

test_that("sample_conditions() rejects numerical conditions", {
  df   <- data.frame(x = rnorm(50), g = sample(c("a", "b"), 50, TRUE),
                     stringsAsFactors = FALSE)
  meta <- metadata(df)
  syn  <- gaussian_copula_synthesizer(meta) |> fit(df)
  expect_error(sample_conditions(syn, data.frame(x = 0.5)), "not supported")
})

test_that("sample_conditions() errors on unfitted synthesizer", {
  syn <- gaussian_copula_synthesizer(small_meta())
  expect_error(sample_conditions(syn, data.frame(edu = "HS")), "must be fitted")
})

test_that("sample_conditions() honours metadata constraints", {
  set.seed(31)
  df <- data.frame(
    grp = sample(c("a", "b"), 200, TRUE),
    x   = rnorm(200, 0, 1),
    y   = rnorm(200, 0, 1),
    stringsAsFactors = FALSE
  )
  # Metadata requires x < y, which the copula model cannot guarantee for the
  # bulk of rows when both columns share the same distribution.
  meta <- metadata(df) |>
    add_constraint(inequality_constraint("x", "y", type = "lt"))
  syn  <- gaussian_copula_synthesizer(meta) |> fit(df)
  set.seed(0)
  out <- sample_conditions(syn,
                           data.frame(grp = "a", .n = 25,
                                      stringsAsFactors = FALSE))
  # Every returned row must satisfy BOTH the condition and the constraint.
  expect_true(all(out$grp == "a"))
  expect_true(all(out$x < out$y))
})

test_that("sample_conditions() emits the rejection warning when constraints cannot be met", {
  df <- data.frame(grp = c("a", "b", "a", "b", "a"),
                   x   = 1:5, y = 1:5, stringsAsFactors = FALSE)
  meta <- metadata(df) |>
    # x < x is identically false: no row can ever satisfy this.
    add_constraint(inequality_constraint("x", "x", type = "lt"))
  syn <- gaussian_copula_synthesizer(meta) |> fit(df)
  expect_warning(
    out <- sample_conditions(syn,
                             data.frame(grp = "a", .n = 5,
                                        stringsAsFactors = FALSE),
                             max_tries = 2L),
    "could only generate"
  )
  expect_equal(nrow(out), 0L)
})

test_that("sample_conditions() rejects non-positive or non-integer .n values", {
  df  <- data.frame(g = c("a","b","a","b","a"), x = 1:5, stringsAsFactors = FALSE)
  syn <- gaussian_copula_synthesizer(metadata(df)) |> fit(df)
  expect_error(sample_conditions(syn, data.frame(g = "a", .n = 0)),
               "positive whole numbers")
  expect_error(sample_conditions(syn, data.frame(g = "a", .n = -2)),
               "positive whole numbers")
  expect_error(sample_conditions(syn, data.frame(g = "a", .n = 1.5)),
               "positive whole numbers")
})
