#' Generate a quality report comparing real and synthetic data
#'
#' Aggregates metrics into the two-property hierarchy used by SDMetrics:
#'
#' * **Column Shapes** — per-column marginal fidelity: KS similarity for
#'   numerical columns and TVD similarity for categorical columns.
#' * **Column Pair Trends** — pairwise dependence: correlation similarity for
#'   numerical pairs and contingency similarity for categorical pairs.
#'
#' The overall score is the mean of the two property scores, so a table with
#' many categorical columns and few numerical ones is not weighted by raw column
#' counts. ML efficacy, when requested, is reported separately and does **not**
#' enter the overall score (matching SDMetrics).
#'
#' @param real A data frame of real data.
#' @param synthetic A data frame of synthetic data.
#' @param metadata An `rsdv_metadata` object.
#' @param target_col Optional. Name of a categorical column for ML efficacy.
#'   Reported alongside the score but excluded from the overall.
#' @return An `rsdv_quality_report` object.
#' @export
#' @examples
#' \donttest{
#' meta  <- metadata(adult_income) |>
#'   set_column_type("age", "numerical") |>
#'   set_column_type("occupation", "categorical")
#' syn   <- gaussian_copula_synthesizer(meta) |> fit(adult_income)
#' synth <- sample(syn, n = 500)
#' qr    <- quality_report(adult_income, synth, meta)
#' print(qr)
#' }
quality_report <- function(real, synthetic, metadata, target_col = NULL) {
  ks_scores  <- ks_similarity(real, synthetic, metadata)
  tvd_scores <- tvd_similarity(real, synthetic, metadata)
  cor_sim    <- correlation_similarity(real, synthetic, metadata)
  con_sim    <- contingency_similarity(real, synthetic, metadata)

  # Column Shapes: mean of all per-column marginal scores.
  shape_scores  <- c(ks_scores$score, tvd_scores$score)
  column_shapes <- if (length(shape_scores) > 0L) mean(shape_scores, na.rm = TRUE) else NA_real_

  # Column Pair Trends: mean over all numeric and categorical pair scores.
  pair_scores <- c(cor_sim$pairs$score, con_sim$pairs$score)
  column_pair_trends <- if (length(pair_scores) > 0L) mean(pair_scores, na.rm = TRUE) else NA_real_

  property_scores <- c(column_shapes, column_pair_trends)
  overall <- if (any(!is.na(property_scores)))
    mean(property_scores, na.rm = TRUE) else NA_real_

  efficacy <- if (!is.null(target_col))
    ml_efficacy(real, synthetic, metadata, target_col) else NULL

  structure(
    list(
      ks_scores                = ks_scores,
      tvd_scores               = tvd_scores,
      correlation_pairs        = cor_sim$pairs,
      contingency_pairs        = con_sim$pairs,
      correlation_score        = cor_sim$score,
      contingency_score        = con_sim$score,
      column_shapes_score      = column_shapes,
      column_pair_trends_score = column_pair_trends,
      ml_efficacy              = efficacy,
      overall_score            = overall
    ),
    class = "rsdv_quality_report"
  )
}

#' Print method for rsdv_quality_report
#'
#' @param x An `rsdv_quality_report` object.
#' @param ... Unused.
#' @return `x`, invisibly.
#'
#' @export
print.rsdv_quality_report <- function(x, ...) {
  cat("== rsdv Quality Report ==\n\n")

  if (nrow(x$ks_scores) > 0) {
    cat("Column Similarity (KS, numerical):\n")
    for (i in seq_len(nrow(x$ks_scores))) {
      cat(sprintf("  %-20s %.3f\n", x$ks_scores$column[i], x$ks_scores$score[i]))
    }
    cat("\n")
  }

  if (nrow(x$tvd_scores) > 0) {
    cat("Column Similarity (TVD, categorical):\n")
    for (i in seq_len(nrow(x$tvd_scores))) {
      cat(sprintf("  %-20s %.3f\n", x$tvd_scores$column[i], x$tvd_scores$score[i]))
    }
    cat("\n")
  }

  cat("Property scores:\n")
  cat(sprintf("  %-20s %.3f\n", "Column Shapes",      x$column_shapes_score))
  cat(sprintf("  %-20s %.3f\n", "Column Pair Trends", x$column_pair_trends_score))
  cat(sprintf("    (correlation %.3f, contingency %.3f)\n",
              x$correlation_score, x$contingency_score))

  if (!is.null(x$ml_efficacy)) {
    cat(sprintf("\nML Efficacy (TSTR/TRTR):     %.3f  [reported, not in overall]\n",
                x$ml_efficacy$score))
  }

  cat(sprintf("\nOverall Score:               %.3f\n", x$overall_score))
  invisible(x)
}
