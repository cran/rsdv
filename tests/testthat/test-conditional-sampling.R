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
