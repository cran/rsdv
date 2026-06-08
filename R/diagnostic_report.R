#' Generate a diagnostic (validity) report for synthetic data
#'
#' Checks whether synthetic data is *structurally valid* against the real data
#' and metadata — independent of how closely it matches the real distributions
#' (that is the job of [quality_report()]). Mirrors the SDMetrics
#' `DiagnosticReport` two-property hierarchy:
#'
#' * **Data Validity** — per-column checks:
#'   * numerical: boundary adherence (fraction of values within the real
#'     min/max range),
#'   * categorical: category adherence (fraction of values whose category was
#'     seen in the real data),
#'   * boolean: always valid,
#'   * primary key: key uniqueness (all values unique and non-missing).
#' * **Data Structure** — fraction of expected columns present in the synthetic
#'   data.
#'
#' Missing (`NA`) values are excluded from adherence denominators, since
#' missingness is modeled separately.
#'
#' @param real A data frame of real data.
#' @param synthetic A data frame of synthetic data.
#' @param metadata An `rsdv_metadata` object.
#' @return An `rsdv_diagnostic_report` object.
#' @export
#' @examples
#' \donttest{
#' meta  <- metadata(adult_income)
#' syn   <- gaussian_copula_synthesizer(meta) |> fit(adult_income)
#' synth <- sample(syn, n = 500)
#' diagnostic_report(adult_income, synth, meta)
#' }
diagnostic_report <- function(real, synthetic, metadata) {
  num_cols  <- get_columns_by_type(metadata, "numerical")
  cat_cols  <- get_columns_by_type(metadata, "categorical")
  bool_cols <- get_columns_by_type(metadata, "boolean")
  pk        <- metadata$primary_key

  validity_rows <- list()
  add <- function(column, check, score)
    validity_rows[[length(validity_rows) + 1L]] <<-
      list(column = column, check = check, score = score)

  for (col in num_cols) {
    if (is.null(synthetic[[col]])) next
    v  <- synthetic[[col]][!is.na(synthetic[[col]])]
    lo <- min(real[[col]], na.rm = TRUE); hi <- max(real[[col]], na.rm = TRUE)
    score <- if (length(v) == 0L) 1 else mean(v >= lo & v <= hi)
    add(col, "boundary adherence", score)
  }

  for (col in c(cat_cols, bool_cols)) {
    if (is.null(synthetic[[col]])) next
    v        <- as.character(synthetic[[col]])
    v        <- v[!is.na(v)]
    allowed  <- as.character(unique(real[[col]]))
    score    <- if (length(v) == 0L) 1 else mean(v %in% allowed)
    add(col, "category adherence", score)
  }

  if (!is.null(pk) && !is.null(synthetic[[pk]])) {
    v     <- synthetic[[pk]]
    score <- as.numeric(!anyNA(v) && !anyDuplicated(v))
    add(pk, "key uniqueness", score)
  }

  validity <- tibble::tibble(
    column = vapply(validity_rows, `[[`, character(1L), "column"),
    check  = vapply(validity_rows, `[[`, character(1L), "check"),
    score  = vapply(validity_rows, `[[`, double(1L),    "score")
  )

  # Data Structure: are all metadata columns present in the synthetic output?
  expected   <- names(metadata$columns)
  present    <- expected %in% names(synthetic)
  structure_score <- if (length(expected) == 0L) 1 else mean(present)

  validity_score <- if (nrow(validity) > 0L) mean(validity$score) else NA_real_
  property_scores <- c(validity_score, structure_score)
  overall <- mean(property_scores, na.rm = TRUE)

  structure(
    list(
      validity         = validity,
      validity_score   = validity_score,
      structure_score  = structure_score,
      missing_columns  = expected[!present],
      overall_score    = overall
    ),
    class = "rsdv_diagnostic_report"
  )
}

#' Print method for rsdv_diagnostic_report
#'
#' @param x An `rsdv_diagnostic_report` object.
#' @param ... Unused.
#' @return `x`, invisibly.
#' @export
print.rsdv_diagnostic_report <- function(x, ...) {
  cat("== rsdv Diagnostic Report ==\n\n")
  if (nrow(x$validity) > 0) {
    cat("Data Validity (per column):\n")
    for (i in seq_len(nrow(x$validity))) {
      cat(sprintf("  %-20s %-20s %.3f\n",
                  x$validity$column[i], x$validity$check[i], x$validity$score[i]))
    }
    cat("\n")
  }
  cat(sprintf("Data Validity score:   %.3f\n", x$validity_score))
  cat(sprintf("Data Structure score:  %.3f\n", x$structure_score))
  if (length(x$missing_columns) > 0)
    cat("  Missing columns:", paste(x$missing_columns, collapse = ", "), "\n")
  cat(sprintf("\nOverall Score:         %.3f\n", x$overall_score))
  invisible(x)
}

#' Plot a diagnostic report
#'
#' Bar chart of per-column validity scores.
#'
#' @param object An `rsdv_diagnostic_report` object.
#' @param ... Unused.
#' @return A `ggplot` object.
#' @importFrom ggplot2 autoplot
#' @examples
#' \donttest{
#' meta  <- metadata(adult_income)
#' syn   <- gaussian_copula_synthesizer(meta) |> fit(adult_income)
#' synth <- sample(syn, n = 500)
#' ggplot2::autoplot(diagnostic_report(adult_income, synth, meta))
#' }
#' @export
autoplot.rsdv_diagnostic_report <- function(object, ...) {
  df <- data.frame(
    column = object$validity$column,
    score  = object$validity$score,
    check  = object$validity$check,
    stringsAsFactors = FALSE
  )
  ggplot2::ggplot(df, ggplot2::aes(x = column, y = score, fill = check)) +
    ggplot2::geom_col(position = "dodge") +
    ggplot2::scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
    ggplot2::labs(
      title    = "rsdv Diagnostic Report",
      subtitle = sprintf("Overall score: %.3f", object$overall_score),
      x        = "Column",
      y        = "Validity score",
      fill     = "Check"
    ) +
    ggplot2::theme_minimal()
}
