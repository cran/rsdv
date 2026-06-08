#' Kolmogorov-Smirnov similarity score per numerical column
#'
#' @param real A data frame of real data.
#' @param synthetic A data frame of synthetic data.
#' @param meta An `rsdv_metadata` object.
#' @return A tibble with columns `column` (chr) and `score` (dbl, 0–1, higher = better).
#' @export
#' @examples
#' \donttest{
#' syn   <- gaussian_copula_synthesizer(metadata(adult_income)) |> fit(adult_income)
#' synth <- sample(syn, n = 500)
#' ks_similarity(adult_income, synth, metadata(adult_income))
#' }
ks_similarity <- function(real, synthetic, meta) {
  num_cols <- get_columns_by_type(meta, "numerical")
  rows <- lapply(num_cols, function(col) {
    # ks.test warns "p-value will be approximate in the presence of ties";
    # we only use the D statistic (the 1 - D similarity score), not the
    # p-value, so the warning is noise for our callers.
    ks <- suppressWarnings(stats::ks.test(real[[col]], synthetic[[col]]))
    list(column = col, score = 1 - ks$statistic[[1L]])
  })
  tibble::tibble(
    column = vapply(rows, `[[`, character(1L), "column"),
    score  = vapply(rows, `[[`, double(1L),    "score")
  )
}

#' Total variation distance similarity score per categorical column
#'
#' @param real A data frame of real data.
#' @param synthetic A data frame of synthetic data.
#' @param meta An `rsdv_metadata` object.
#' @return A tibble with columns `column` (chr) and `score` (dbl, 0–1, higher = better).
#' @export
#' @examples
#' \donttest{
#' syn   <- gaussian_copula_synthesizer(metadata(adult_income)) |> fit(adult_income)
#' synth <- sample(syn, n = 500)
#' tvd_similarity(adult_income, synth, metadata(adult_income))
#' }
tvd_similarity <- function(real, synthetic, meta) {
  cat_cols <- get_columns_by_type(meta, "categorical")
  rows <- lapply(cat_cols, function(col) {
    all_levels <- union(unique(real[[col]]), unique(synthetic[[col]]))
    p_real <- table(factor(real[[col]],      levels = all_levels)) / nrow(real)
    p_syn  <- table(factor(synthetic[[col]], levels = all_levels)) / nrow(synthetic)
    tvd    <- 0.5 * sum(abs(as.numeric(p_real) - as.numeric(p_syn)))
    list(column = col, score = 1 - tvd)
  })
  tibble::tibble(
    column = vapply(rows, `[[`, character(1L), "column"),
    score  = vapply(rows, `[[`, double(1L),    "score")
  )
}

#' Correlation similarity between real and synthetic numerical column pairs
#'
#' For each pair of numerical columns, computes `1 - |corr_real - corr_syn| / 2`
#' (the SDMetrics `CorrelationSimilarity` score), where `corr` is the Pearson
#' correlation. Returns one row per pair plus the mean.
#'
#' @param real A data frame of real data.
#' @param synthetic A data frame of synthetic data.
#' @param meta An `rsdv_metadata` object.
#' @return A list with `pairs` (a tibble of `column_1`, `column_2`, `score`) and
#'   `score` (the mean over pairs; `1` when there are fewer than two numerical
#'   columns).
#' @export
#' @examples
#' \donttest{
#' syn       <- gaussian_copula_synthesizer(metadata(adult_income)) |> fit(adult_income)
#' synth_data <- sample(syn, n = 500)
#' correlation_similarity(adult_income, synth_data, metadata(adult_income))
#' }
correlation_similarity <- function(real, synthetic, meta) {
  num_cols <- get_columns_by_type(meta, "numerical")
  empty    <- tibble::tibble(column_1 = character(), column_2 = character(),
                             score = double())
  if (length(num_cols) < 2L) return(list(pairs = empty, score = 1))

  cor_real <- stats::cor(real[, num_cols, drop = FALSE],
                         use = "pairwise.complete.obs")
  cor_syn  <- stats::cor(synthetic[, num_cols, drop = FALSE],
                         use = "pairwise.complete.obs")

  combos <- utils::combn(num_cols, 2L)
  rows   <- lapply(seq_len(ncol(combos)), function(j) {
    a <- combos[1L, j]; b <- combos[2L, j]
    # |r_real - r_syn| ranges over [0, 2]; halving maps the score to [0, 1].
    s <- 1 - abs(cor_real[a, b] - cor_syn[a, b]) / 2
    list(column_1 = a, column_2 = b, score = s)
  })
  pairs <- tibble::tibble(
    column_1 = vapply(rows, `[[`, character(1L), "column_1"),
    column_2 = vapply(rows, `[[`, character(1L), "column_2"),
    score    = vapply(rows, `[[`, double(1L),    "score")
  )
  list(pairs = pairs, score = mean(pairs$score, na.rm = TRUE))
}

#' Contingency similarity between real and synthetic categorical column pairs
#'
#' For each pair of categorical columns, compares the joint (normalized
#' contingency) distributions of real and synthetic data via total variation
#' distance, scoring `1 - TVD` (the SDMetrics `ContingencySimilarity` score).
#' This is the categorical analogue of correlation similarity and captures
#' categorical-vs-categorical dependence.
#'
#' @param real A data frame of real data.
#' @param synthetic A data frame of synthetic data.
#' @param meta An `rsdv_metadata` object.
#' @return A list with `pairs` (a tibble of `column_1`, `column_2`, `score`) and
#'   `score` (the mean over pairs; `1` when there are fewer than two categorical
#'   columns).
#' @export
#' @examples
#' \donttest{
#' meta  <- metadata(adult_income)
#' syn   <- gaussian_copula_synthesizer(meta) |> fit(adult_income)
#' synth <- sample(syn, n = 500)
#' contingency_similarity(adult_income, synth, meta)
#' }
contingency_similarity <- function(real, synthetic, meta) {
  cat_cols <- get_columns_by_type(meta, "categorical")
  empty    <- tibble::tibble(column_1 = character(), column_2 = character(),
                             score = double())
  if (length(cat_cols) < 2L) return(list(pairs = empty, score = 1))

  combos <- utils::combn(cat_cols, 2L)
  rows   <- lapply(seq_len(ncol(combos)), function(j) {
    a <- combos[1L, j]; b <- combos[2L, j]
    lev_a <- union(unique(real[[a]]), unique(synthetic[[a]]))
    lev_b <- union(unique(real[[b]]), unique(synthetic[[b]]))
    p_real <- table(factor(real[[a]],      levels = lev_a),
                    factor(real[[b]],      levels = lev_b)) / nrow(real)
    p_syn  <- table(factor(synthetic[[a]], levels = lev_a),
                    factor(synthetic[[b]], levels = lev_b)) / nrow(synthetic)
    tvd <- 0.5 * sum(abs(as.numeric(p_real) - as.numeric(p_syn)))
    list(column_1 = a, column_2 = b, score = 1 - tvd)
  })
  pairs <- tibble::tibble(
    column_1 = vapply(rows, `[[`, character(1L), "column_1"),
    column_2 = vapply(rows, `[[`, character(1L), "column_2"),
    score    = vapply(rows, `[[`, double(1L),    "score")
  )
  list(pairs = pairs, score = mean(pairs$score, na.rm = TRUE))
}

#' ML efficacy: train-on-synthetic / test-on-real accuracy ratio (TSTR)
#'
#' Trains an `rpart` decision tree on synthetic data and on a real training
#' split, evaluates both on a real held-out test set, and returns the ratio
#' TSTR / TRTR. A score near 1 means synthetic data is as informative as
#' real data for this prediction task.
#'
#' @param real A data frame of real data.
#' @param synthetic A data frame of synthetic data.
#' @param meta An `rsdv_metadata` object.
#' @param target_col Name of a categorical column to use as the outcome.
#' @param test_fraction Fraction of `real` to hold out as the test set.
#' @return A list with elements `tstr` (accuracy), `trtr` (accuracy), and
#'   `score` (ratio, capped at 1).
#' @export
#' @examples
#' \donttest{
#' meta      <- metadata(adult_income)
#' syn       <- gaussian_copula_synthesizer(meta) |> fit(adult_income)
#' synth_data <- sample(syn, n = 500)
#' ml_efficacy(adult_income, synth_data, meta, target_col = "income")
#' }
ml_efficacy <- function(real, synthetic, meta, target_col,
                        test_fraction = 0.2) {
  n        <- nrow(real)
  test_idx <- sample.int(n, size = floor(n * test_fraction))
  train_real <- real[-test_idx, , drop = FALSE]
  test_real  <- real[ test_idx, , drop = FALSE]

  formula <- stats::as.formula(paste(target_col, "~ ."))

  # Convert character columns to factors with levels from the full real dataset
  # before fitting. This ensures rpart's xlevels cover every value in test_real,
  # preventing "factor has new levels" errors when rare values appear only in
  # the held-out fold or only in the real data (not the 500-row synthetic sample).
  fit_syn  <- rpart::rpart(formula, data = .set_levels(synthetic,  real), method = "class")
  fit_real <- rpart::rpart(formula, data = .set_levels(train_real, real), method = "class")

  pred_syn  <- stats::predict(fit_syn,  newdata = test_real, type = "class")
  pred_real <- stats::predict(fit_real, newdata = test_real, type = "class")

  acc <- function(pred) mean(pred == test_real[[target_col]], na.rm = TRUE)
  tstr  <- acc(pred_syn)
  trtr  <- acc(pred_real)
  score <- if (trtr > 0) min(tstr / trtr, 1) else 0

  list(tstr = tstr, trtr = trtr, score = score)
}

# Convert character columns in df to factors whose levels are the union of the
# values present in df and those present in reference. This expands the level
# set so models fitted on df recognise every value that appears in reference.
.set_levels <- function(df, reference) {
  for (col in names(df)) {
    if (!is.character(df[[col]])) next
    if (!col %in% names(reference)) next
    lvls <- sort(union(unique(as.character(df[[col]])),
                       unique(as.character(reference[[col]]))))
    df[[col]] <- factor(df[[col]], levels = lvls)
  }
  df
}
