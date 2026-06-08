test_that("quality_report() returns an rsdv_quality_report object", {
  syn <- gaussian_copula_synthesizer(small_meta()) |> fit(small_data())
  synth <- sample(syn, n = 50)
  qr  <- quality_report(real = small_data(), synthetic = synth, metadata = small_meta())
  expect_s3_class(qr, "rsdv_quality_report")
})

test_that("quality_report() contains expected score components", {
  syn <- gaussian_copula_synthesizer(small_meta()) |> fit(small_data())
  synth <- sample(syn, n = 50)
  qr  <- quality_report(real = small_data(), synthetic = synth, metadata = small_meta())
  expect_true(all(c("ks_scores", "tvd_scores", "correlation_score",
                    "overall_score") %in% names(qr)))
})

test_that("quality_report() overall_score is in [0, 1]", {
  syn <- gaussian_copula_synthesizer(small_meta()) |> fit(small_data())
  synth <- sample(syn, n = 50)
  qr  <- quality_report(small_data(), synth, small_meta())
  expect_true(qr$overall_score >= 0 && qr$overall_score <= 1)
})

test_that("print.rsdv_quality_report() prints without error", {
  syn <- gaussian_copula_synthesizer(small_meta()) |> fit(small_data())
  synth <- sample(syn, n = 50)
  qr  <- quality_report(small_data(), synth, small_meta())
  expect_output(print(qr), "Quality Report")
  expect_output(print(qr), "Overall")
})

test_that("quality_report() score is high when real == synthetic", {
  syn <- gaussian_copula_synthesizer(small_meta()) |> fit(small_data())
  # Compare real data against itself — perfect similarity
  qr <- quality_report(small_data(), small_data(), small_meta())
  expect_gt(qr$overall_score, 0.8)
})

test_that("quality_report() with target_col populates ml_efficacy", {
  syn   <- gaussian_copula_synthesizer(small_meta()) |> fit(small_data())
  synth <- sample(syn, n = 50)
  qr    <- quality_report(small_data(), synth, small_meta(), target_col = "edu")
  expect_false(is.null(qr$ml_efficacy))
})

test_that("quality_report() is valid with only numerical columns (no categorical)", {
  meta <- metadata() |>
    set_column_type("x", "numerical") |>
    set_column_type("y", "numerical")
  df  <- data.frame(x = rnorm(30), y = rnorm(30))
  syn <- gaussian_copula_synthesizer(meta) |> fit(df)
  out <- sample(syn, n = 30)
  qr  <- quality_report(df, out, meta)
  expect_false(is.nan(qr$overall_score))
  expect_true(qr$overall_score >= 0 && qr$overall_score <= 1)
  expect_equal(nrow(qr$tvd_scores), 0L)
})
