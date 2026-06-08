test_that("privacy_report() returns an rsdv_privacy_report", {
  syn   <- gaussian_copula_synthesizer(small_meta()) |> fit(small_data())
  synth <- sample(syn, n = 30)
  pr    <- privacy_report(small_data(), synth)
  expect_s3_class(pr, "rsdv_privacy_report")
})

test_that("privacy_report() contains nndr_score", {
  syn   <- gaussian_copula_synthesizer(small_meta()) |> fit(small_data())
  synth <- sample(syn, n = 30)
  pr    <- privacy_report(small_data(), synth)
  expect_true("nndr_score" %in% names(pr))
  expect_true(pr$nndr_score >= 0 && pr$nndr_score <= 1)
})

test_that("print.rsdv_privacy_report() prints without error", {
  syn   <- gaussian_copula_synthesizer(small_meta()) |> fit(small_data())
  synth <- sample(syn, n = 30)
  pr    <- privacy_report(small_data(), synth)
  expect_output(print(pr), "Privacy Report")
})

test_that("autoplot.rsdv_quality_report() returns a ggplot", {
  syn   <- gaussian_copula_synthesizer(small_meta()) |> fit(small_data())
  synth <- sample(syn, n = 30)
  qr    <- quality_report(small_data(), synth, small_meta())
  p     <- ggplot2::autoplot(qr)
  expect_s3_class(p, "gg")
})

test_that("autoplot.rsdv_privacy_report() returns a ggplot", {
  syn   <- gaussian_copula_synthesizer(small_meta()) |> fit(small_data())
  synth <- sample(syn, n = 30)
  pr    <- privacy_report(small_data(), synth)
  p     <- ggplot2::autoplot(pr)
  expect_s3_class(p, "gg")
})
