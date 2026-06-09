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
    # Drop NAs and divide by the non-NA count on each side. Including NAs in
    # the denominator (the old behaviour) inflated TVD whenever either side
    # had NAs, because the empirical probabilities then summed to less than 1.
    real_vals <- real[[col]][!is.na(real[[col]])]
    syn_vals  <- synthetic[[col]][!is.na(synthetic[[col]])]
    if (length(real_vals) == 0L || length(syn_vals) == 0L) {
      # No usable data on at least one side — nothing to compare.
      return(list(column = col, score = NA_real_))
    }
    all_levels <- union(unique(real_vals), unique(syn_vals))
    p_real <- tabulate(match(real_vals, all_levels),
                       nbins = length(all_levels)) / length(real_vals)
    p_syn  <- tabulate(match(syn_vals,  all_levels),
                       nbins = length(all_levels)) / length(syn_vals)
    tvd    <- 0.5 * sum(abs(p_real - p_syn))
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
#'   `score` (the mean over pairs). `score` is `NA_real_` when there are fewer
#'   than two numerical columns — there is no dependence to measure, so
#'   propagating `NA` (rather than `1`) avoids overstating fidelity in the
#'   aggregated quality report.
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
  if (length(num_cols) < 2L) return(list(pairs = empty, score = NA_real_))

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
#'   `score` (the mean over pairs). `score` is `NA_real_` when there are fewer
#'   than two categorical columns — there is no dependence to measure, so
#'   propagating `NA` (rather than `1`) avoids overstating fidelity in the
#'   aggregated quality report.
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
  if (length(cat_cols) < 2L) return(list(pairs = empty, score = NA_real_))

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
#' @param test_fraction Fraction of `real` to hold out as the test set. Must be
#'   strictly between 0 and 1.
#' @param seed Optional integer seed. When supplied, the train/test split is
#'   reproducible across calls without affecting the caller's RNG stream.
#' @return A list with elements `tstr` (accuracy), `trtr` (accuracy), and
#'   `score` (ratio, capped at 1).
#' @export
#' @examples
#' \donttest{
#' meta      <- metadata(adult_income)
#' syn       <- gaussian_copula_synthesizer(meta) |> fit(adult_income)
#' synth_data <- sample(syn, n = 500)
#' ml_efficacy(adult_income, synth_data, meta, target_col = "income", seed = 1)
#' }
ml_efficacy <- function(real, synthetic, meta, target_col,
                        test_fraction = 0.2, seed = NULL) {
  if (!target_col %in% names(real))
    stop(sprintf("target_col '%s' not found in `real`.", target_col))
  if (!is.numeric(test_fraction) || length(test_fraction) != 1L ||
      !is.finite(test_fraction) || test_fraction <= 0 || test_fraction >= 1)
    stop("`test_fraction` must be a single number strictly between 0 and 1.")

  n <- nrow(real)
  # Reproducible split when seed is given; otherwise use the global RNG so
  # behaviour is unchanged for callers who rely on set.seed() outside.
  test_idx <- if (is.null(seed)) {
    sample.int(n, size = floor(n * test_fraction))
  } else {
    old <- if (exists(".Random.seed", envir = .GlobalEnv))
      get(".Random.seed", envir = .GlobalEnv) else NULL
    on.exit(
      if (is.null(old)) rm(".Random.seed", envir = .GlobalEnv) else
        assign(".Random.seed", old, envir = .GlobalEnv),
      add = TRUE
    )
    set.seed(seed)
    sample.int(n, size = floor(n * test_fraction))
  }
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

# For each character or factor column in df, expand the level set to the
# union of all values present in df *or* reference. Models fitted on df then
# recognise every value that may appear in reference at predict() time,
# preventing rpart's "factor has new levels" error.
#
# Previously only character columns were expanded; factor columns silently
# kept their original levels and broke predict() whenever reference had a
# level the synthesizer hadn't drawn.
.set_levels <- function(df, reference) {
  for (col in names(df)) {
    is_char <- is.character(df[[col]])
    is_fac  <- is.factor(df[[col]])
    if (!is_char && !is_fac) next
    if (!col %in% names(reference)) next

    df_vals  <- if (is_fac) c(as.character(df[[col]]), levels(df[[col]]))
                else        as.character(df[[col]])
    ref_vals <- as.character(reference[[col]])
    lvls     <- sort(unique(c(df_vals[!is.na(df_vals)],
                              ref_vals[!is.na(ref_vals)])))
    df[[col]] <- factor(df[[col]], levels = lvls)
  }
  df
}
