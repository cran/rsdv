#' Nearest-Neighbor Distance Ratio privacy score
#'
#' For each synthetic row, computes the ratio of its distance to the nearest
#' real row vs. its distance to the second-nearest real row. A high ratio
#' (close to 1) means the synthetic row is not unusually close to any
#' specific real row — low disclosure risk. Score = mean(ratio > 0.5).
#'
#' By default columns are z-scored using the real-data mean and standard
#' deviation before the Euclidean distance is computed; without this, a single
#' large-scale column (e.g. income in dollars) dominates the distance and the
#' score becomes a function of measurement units rather than of similarity.
#'
#' @param real,synthetic Data frames; only numerical columns are used.
#' @param normalize Logical. When `TRUE` (default), columns are z-scored using
#'   the real-data mean and standard deviation before distance computation.
#'   Constant columns in `real` are dropped to avoid division by zero.
#' @return A scalar score in \[0, 1\]; higher = more private.
#' @export
#' @examples
#' real <- data.frame(x = rnorm(50), y = rnorm(50))
#' syn  <- data.frame(x = rnorm(50), y = rnorm(50))
#' nndr(real, syn)
nndr <- function(real, synthetic, normalize = TRUE) {
  # Use the intersection of numeric column names so that columns typed as
  # categorical in metadata (character in synthetic, integer in real) do not
  # cause a dimension mismatch inside FNN::knnx.dist.
  real_num   <- names(real)[vapply(real, is.numeric, logical(1))]
  syn_num    <- names(synthetic)[vapply(synthetic, is.numeric, logical(1))]
  shared_num <- intersect(real_num, syn_num)

  if (length(shared_num) == 0L) return(1)

  real_mat <- as.matrix(real[, shared_num, drop = FALSE])
  syn_mat  <- as.matrix(synthetic[, shared_num, drop = FALSE])

  if (normalize) {
    # Z-score using the real-data centre and scale, applied to both matrices.
    # Drop constant real columns (zero sd) — they can't separate rows anyway.
    centre <- colMeans(real_mat, na.rm = TRUE)
    scale_ <- apply(real_mat, 2L, stats::sd, na.rm = TRUE)
    keep   <- is.finite(scale_) & scale_ > 0
    if (!any(keep)) return(1)
    real_mat <- sweep(sweep(real_mat[, keep, drop = FALSE], 2L, centre[keep], "-"),
                      2L, scale_[keep], "/")
    syn_mat  <- sweep(sweep(syn_mat[,  keep, drop = FALSE], 2L, centre[keep], "-"),
                      2L, scale_[keep], "/")
  }

  if (ncol(real_mat) == 0 || ncol(syn_mat) == 0) return(1)
  if (nrow(real_mat) < 2L) stop("`real` must have at least 2 rows for NNDR computation")

  nn_result <- FNN::knnx.dist(data = real_mat, query = syn_mat, k = 2L)
  d1 <- nn_result[, 1L]
  d2 <- nn_result[, 2L]

  # Avoid division by zero: treat ratio as 0 when both distances are 0
  ratio <- ifelse(d2 == 0, 0, d1 / d2)
  mean(ratio > 0.5)
}

#' Attribute disclosure risk
#'
#' Estimates the fraction of synthetic rows where a sensitive column value
#' can be correctly inferred from known columns via a k-NN lookup in the
#' real training data.
#'
#' `known_cols` must be numeric, because nearest-neighbour lookup operates on
#' Euclidean distance over the columns. If you want to use a categorical
#' column as a known attribute, one-hot encode it first (e.g. with
#' `model.matrix(~ col - 1, data)`).
#'
#' @param real,synthetic Data frames.
#' @param sensitive_col Name of the column to protect.
#' @param known_cols Character vector of **numeric** columns assumed known to
#'   an adversary. Categorical columns are rejected with a clear error.
#' @param k Number of nearest neighbors used in inference.
#' @return A scalar in \[0, 1\]; lower = more private.
#' @export
#' @examples
#' real <- data.frame(age = sample(20:60, 50, replace = TRUE),
#'                    income = sample(c("low", "high"), 50, replace = TRUE),
#'                    stringsAsFactors = FALSE)
#' syn  <- real[sample(50), ]
#' attribute_disclosure_risk(real, syn, sensitive_col = "income", known_cols = "age")
attribute_disclosure_risk <- function(real, synthetic,
                                      sensitive_col, known_cols, k = 1L) {
  missing_real <- setdiff(c(known_cols, sensitive_col), names(real))
  if (length(missing_real) > 0L)
    stop(sprintf("Column(s) not found in `real`: %s",
                 paste(sprintf("'%s'", missing_real), collapse = ", ")))
  missing_syn  <- setdiff(c(known_cols, sensitive_col), names(synthetic))
  if (length(missing_syn) > 0L)
    stop(sprintf("Column(s) not found in `synthetic`: %s",
                 paste(sprintf("'%s'", missing_syn), collapse = ", ")))

  non_numeric <- known_cols[!vapply(real[, known_cols, drop = FALSE],
                                    is.numeric, logical(1L))]
  if (length(non_numeric) > 0L)
    stop(sprintf(
      "known_cols must be numeric, but %s %s not. One-hot encode categorical knowns first, e.g. model.matrix(~ col - 1, data).",
      paste(sprintf("'%s'", non_numeric), collapse = ", "),
      if (length(non_numeric) == 1L) "is" else "are"
    ))

  train_x <- as.matrix(real[, known_cols, drop = FALSE])
  train_y <- real[[sensitive_col]]
  test_x  <- as.matrix(synthetic[, known_cols, drop = FALSE])
  true_y  <- synthetic[[sensitive_col]]

  nn_idx  <- FNN::knnx.index(data = train_x, query = test_x, k = k)
  pred_y  <- train_y[nn_idx[, 1L]]
  mean(pred_y == true_y, na.rm = TRUE)
}
